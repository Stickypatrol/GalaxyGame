(*
   Contact
   Communications library for F# using Lidgren for networking
*)

module Contact

open Lidgren.Network
open System.Net
open Auxiliary
open Mail

//Mode of operation. 'Create' sets up a new network
type NetMode =
    | Create = 0
    | Join   = 1

//Communicate peer information
let sendPeerInfo (peer : NetPeer) (ip : IPAddress) (port : int) =
    try
        let peerMessage  = peer.CreateMessage ()
        peerMessage.Write ((int)MessageType.PeerInformation)
        
        let ipBytes = ip.GetAddressBytes ()
        peerMessage.Write ((int)MessageType.PeerInformation)
        peerMessage.Write (ipBytes.Length)
        peerMessage.Write (ipBytes)
        peerMessage.Write (port)
        
        if peer.Connections.Count > 0 then
            peer.SendMessage (peerMessage, peer.Connections, NetDeliveryMethod.ReliableOrdered, 0)
        else
            printfn "Contact: Couldn't send peer info, no connections!"

        Success ()
    with
        | ex -> Failure ex.Message

//Approve incoming connection
let approveConnection (peer : NetPeer) (message : NetIncomingMessage) =
    try
        message.SenderConnection.Approve ()
        printfn "Contact: Sending peer info"
        sendPeerInfo peer message.SenderEndPoint.Address message.SenderEndPoint.Port
    with
        | ex -> Failure ex.Message

//Process a status change message
let reportStatusChange (message : NetIncomingMessage) =
    try
        let id     = message.SenderConnection.RemoteUniqueIdentifier.ToString ()
        let status = enum<NetConnectionStatus> (message.ReadInt32 ())
        let reason = message.SenderConnection.RemoteHailMessage.ReadString ()
        printfn "Contact: %s reports: %A - %s" id status reason
        Success ()
    with
        | ex -> Failure ex.Message

//Process an incoming message
let processDataMessage (peer : NetPeer) (message : NetIncomingMessage) =
    try
        //Check the identifier to see which message type we're talking about
        let messageType = message.ReadInt32 ()
        match (enum<MessageType> messageType) with
        | MessageType.PeerInformation ->
            //Peer information is exchanged here
            let byteLength = message.ReadInt32 ()
            let ip         = new IPAddress (message.ReadBytes (byteLength))
            let port       = message.ReadInt32 ()
            let endPoint   = new IPEndPoint (ip, port)

            let endPointConnection = peer.GetConnection(endPoint)
            match endPointConnection with
            | null ->
                let localHash  = peer.Configuration.LocalAddress.GetHashCode ()
                let localPort  = peer.Configuration.Port.GetHashCode ()
                let remoteHash = endPoint.Address.GetHashCode ()
                let remotePort = endPoint.Port.GetHashCode ()
                if  (localHash <> remoteHash) || (localPort <> remotePort)  then
                    printfn "Contact: Initiating new connection to %s:%s"
                        (endPoint.Address.ToString()) (endPoint.Port.ToString ())
                    peer.Connect (endPoint) |> ignore
            | _  -> ()
            Success None
        
        | MessageType.Position ->
            let (target : MailTarget), (id : ID), x, y =
                enum<MailTarget> (message.ReadInt32 ()),
                message.ReadInt32 (),
                message.ReadFloat (),
                message.ReadFloat ()
            Success (Some (target, id, Position (double x, double y)))
        
        | _  ->
            printfn "Contact: Unhandled message type: %A!" messageType
            Success None
    with
        | ex -> Failure ex.Message

//Process any incoming messages and return the updated mailbox
let rec processInbox (peer : NetPeer) (inbox : Map<MailTarget * ID, Mail List>) =
    try
        let rec collectMessages acc =
            //Read a message received by Lidgren
            let (message : NetIncomingMessage) = peer.ReadMessage ()
            match message with
            | null -> acc
            | _ ->
                //Process the message, return Some (mail) if the message contains game info
                let mail =
                    match message.MessageType with
                    | NetIncomingMessageType.VerboseDebugMessage
                    | NetIncomingMessageType.DebugMessage
                    | NetIncomingMessageType.WarningMessage
                    | NetIncomingMessageType.ErrorMessage        ->
                        printfn "Contact: Debug: %s" (message.ReadString ())
                        None
                        
                    | NetIncomingMessageType.DiscoveryRequest ->
                        peer.SendDiscoveryResponse (null, message.SenderEndPoint)
                        None
                        
                    | NetIncomingMessageType.DiscoveryResponse ->
                        peer.Connect (message.SenderEndPoint) |> ignore
                        None
                    | NetIncomingMessageType.UnconnectedData ->
                        printfn "Contact: Unconnected data: %s" (message.ReadString ())
                        None
                    
                    | NetIncomingMessageType.StatusChanged ->
                        match reportStatusChange message with
                        | Success s -> ()
                        | Failure f -> printfn "Contact: Failed to report status change - %A" f
                        None
                        
                    | NetIncomingMessageType.ConnectionApproval  ->
                        match approveConnection peer message with
                        | Success s -> ()
                        | Failure f -> printfn "Contact: Failed to approve connection - %A" f
                        None
                        
                    | NetIncomingMessageType.Data ->
                        match processDataMessage peer message with
                        | Success s -> s
                        | Failure f -> printfn "Contact: Failed to process incoming data message - %A" f; None
                        
                    | _ ->
                        printfn "Unhandled message type: %A" message
                        None
                
                //Garbage collect message
                peer.Recycle message
                
                //Add to accumulator if necessary
                let acc' =
                    match mail with
                    | Some (target, id, m) ->
                        match Map.tryFind (target, id) acc with
                        | Some mailbox ->
                            Map.add (target, id) (m :: mailbox) acc
                        | None ->
                            Map.add (target, id) [m] acc
                    | None -> acc
                
                collectMessages acc'
                
        Success (collectMessages inbox)
    with
        | ex -> Failure ex.Message

//Process an outgoing message
let processOutgoingMessage (peer : NetPeer) (target : MailTarget) (id : ID) (mail : Mail) =
    try
        match mail with
        | Position (x, y) ->
            let message = peer.CreateMessage ()
            message.Write (int MessageType.Position)
            message.Write (int target)
            message.Write (int id)
            message.Write (x)
            message.Write (y)
            peer.SendMessage (message, peer.Connections, NetDeliveryMethod.ReliableOrdered, 0)
            Success None
            
        | _ ->
            printfn "Unhandled outgoing message: %A!" mail
            Success (Some mail) //Message was not sent, keep it in the outbox
    with
        | ex -> Failure ex.Message

//Send messages to all peers
let processOutbox (peer : NetPeer) (outbox : Map<MailTarget * ID, Mail List>) =
    try
        if Map.isEmpty outbox then
            Success outbox
        else
            if peer.ConnectionsCount > 0 then
                let outboxFold =
                    fun outboxAcc (target : MailTarget, id : ID) mailbox ->
                        let mailbox' =
                            List.fold (fun mailboxAcc mail ->
                                match processOutgoingMessage peer target id mail with
                                | Success s ->
                                    match s with
                                    | Some m -> m :: mailboxAcc
                                    | None   -> mailboxAcc
                                | Failure f ->
                                    printfn "Contact: Failed to process outgoing data message - %A" f
                                    mailboxAcc) List.empty mailbox
                        Map.add (target, id) mailbox' outboxAcc
                
                Success (Map.fold outboxFold Map.empty outbox)
            else Success outbox
    with
        | ex -> Failure ex.Message

//This record contains all necessary information for Contact to function
type ContactSession =
    { Peer    : NetPeer Option
      Config  : NetPeerConfiguration Option
      Mailbox : Mailbox } with
    static member Zero =
        { Peer    = None
          Config  = None
          Mailbox = Mailbox.Zero }
    static member Start sessionName localPort remotePort maxCons messageTypes c =
        //Set Lidgren configuration
        let setup =
            try
                let config = new NetPeerConfiguration (sessionName)
                do config.Port                      <- localPort
                do config.LocalAddress              <- new IPAddress ((int64)0x0100007f) //NetUtility.Resolve("localhost")
                do config.MaximumConnections        <- maxCons
                do config.AcceptIncomingConnections <- true
                    
                //Enable Lidgren message types
                List.iter (fun t -> do config.EnableMessageType t) messageTypes
                
                let peer = new NetPeer (config)
                peer.Start()
                
                Success (config, peer)
            with
                | ex -> Failure ex.Message

        let discovery (peer : NetPeer) =
            try
                peer.DiscoverKnownPeer ("localhost", remotePort) |> ignore
                peer.DiscoverLocalPeers (remotePort)
                Success ()
            with
                | ex -> Failure ex.Message

        let c' = 
            match setup with
            | Success (config, peer) -> { c with Config = Some config
                                                 Peer   = Some peer   }
            | Failure f -> printfn "Contact: Failed to generate configuration - %A" f; c
            
        match c.Peer with
        | Some peer ->
            match discovery peer with
            | Success s -> ()
            | Failure f -> printfn "Contact: Failed to initiate peer discovery - %A" f
        | None -> ()
        
        c'
        
    static member Stop c =
        let attemptStop () =
            try
                match c.Peer with
                | Some peer -> peer.Shutdown ("Stopping Contact")
                | None -> ()
                Success ()
            with
                | ex -> Failure ex.Message

        match attemptStop () with
        | Success s -> ()
        | Failure f -> printfn "Contact: Failed to shut down peer - %A" f
        c
        
    static member Send c =
        match c.Peer with
        | Some peer ->
            match processOutbox peer c.Mailbox.Outbox with
            | Success outbox' -> { c with Mailbox = { c.Mailbox with Outbox = outbox' } }
            | Failure f -> printfn "Contact: Failed to process outbox - %A" f; c
        | None -> c
        
    static member Receive c =
        match c.Peer with
        | Some peer ->
            match processInbox peer c.Mailbox.Inbox with
            | Success inbox' -> { c with Mailbox = { c.Mailbox with Inbox = inbox' } }
            | Failure f -> printfn "Contact: Failed to process inbox - %A" f; c
        | None -> c
