(*
   Functionality for communication across a network
*)

module Network

open System.Net
open Lidgren.Network
open Auxiliary
open Mailbox

//Mode of operation, create sets up a new network
type NetMode =
    | Create = 0
    | Join   = 1

//Used to identify the type of incoming information
type NetMessage =
    | PeerInformation = 0
    | Transform       = 1

//Simple alias for cleaner match expression
type Incoming = NetIncomingMessageType

//Initialize networking
let startNetworking mode localPort remotePort maxCons =
    //Generate a random identifier
    let identifier = (newID ()).ToString ()

    //Set up networking configuration
    let config = new NetPeerConfiguration "BusinessGame"
    do config.Port                      <- localPort
    do config.MaximumConnections        <- maxCons
    do config.LocalAddress              <- new IPAddress ((int64)0x0100007f) //NetUtility.Resolve("localhost")
    do config.AcceptIncomingConnections <- true

    //Enable Lidgren message types
    let messageTypes =
        [ Incoming.DiscoveryRequest
          Incoming.DiscoveryResponse
          Incoming.ConnectionApproval
          Incoming.StatusChanged
          Incoming.UnconnectedData
          Incoming.Data
          Incoming.VerboseDebugMessage
          Incoming.DebugMessage
          Incoming.WarningMessage
          Incoming.ErrorMessage        ]
    
    let rec enableMessageTypes list =
        match list with
        | x :: xs ->
            do config.EnableMessageType x
            enableMessageTypes xs
        | [] -> ()
    
    enableMessageTypes messageTypes

    //Start the peer object
    printfn "Starting peer"
    let peer = new NetPeer (config)
    peer.Start ()
    
    //Look for peers
    peer.DiscoverKnownPeer ("localhost", remotePort) |> ignore
    peer.DiscoverLocalPeers (remotePort)

    //Return the configuration and the peer so it can be saved in the state
    (config, peer)
    
let handleDebugMessage (message : NetIncomingMessage) =
    printfn "Debug: %s" (message.ReadString ())

let sendPeerInfo (peer : NetPeer) (ip : IPAddress) (port : int) =
    let peerMessage  = peer.CreateMessage ()
    peerMessage.Write ((int)NetMessage.PeerInformation)
    
    let ipBytes = ip.GetAddressBytes ()
    peerMessage.Write ((int)NetMessage.PeerInformation)
    peerMessage.Write (ipBytes.Length)
    peerMessage.Write (ipBytes)
    peerMessage.Write (port)
    
    if peer.Connections.Count > 0 then
        peer.SendMessage (peerMessage, peer.Connections, NetDeliveryMethod.ReliableOrdered, 0)
    else
        printfn "Couldn't send peer info, no connections!"

//Process received messages
let rec processIncomingMessages (peer : NetPeer) mailbox =
    let inbox = mailbox.Inbox
    let message = peer.ReadMessage ()
    let mail =
        match message with
        | null -> None
        | _ ->
            match message.MessageType with
            | Incoming.Data ->
                let messageType = message.ReadInt32 ()
                match (enum<NetMessage> messageType) with
                | NetMessage.PeerInformation ->
                    let byteLength = message.ReadInt32 ()
                    let ip         = new IPAddress (message.ReadBytes (byteLength))
                    let port       = message.ReadInt32 ()
                    let endPoint   = new IPEndPoint (ip, port)
                    
                    match peer.GetConnection (endPoint) with
                    | null ->
                        let localHash  = peer.Configuration.LocalAddress.GetHashCode ()
                        let localPort  = peer.Configuration.Port.GetHashCode ()
                        let remoteHash = endPoint.Address.GetHashCode ()
                        let remotePort = endPoint.Port.GetHashCode ()
                        if  (localHash <> remoteHash) || (localPort <> remotePort)  then
                            printfn "Initiating new connection to %s:%s"
                                (endPoint.Address.ToString()) (endPoint.Port.ToString ())
                            peer.Connect (endPoint) |> ignore
                    | _  -> ()
                    None
                    
                | _  ->
                    printfn "Unhandled message type: %A!" messageType
                    None
                
            | Incoming.VerboseDebugMessage
            | Incoming.DebugMessage
            | Incoming.WarningMessage
            | Incoming.ErrorMessage        ->
                handleDebugMessage message
                None
                
            | Incoming.DiscoveryRequest ->
                peer.SendDiscoveryResponse (null, message.SenderEndPoint)
                None
                        
            | Incoming.DiscoveryResponse ->
                peer.Connect (message.SenderEndPoint) |> ignore
                None
                        
            | Incoming.ConnectionApproval ->
                message.SenderConnection.Approve ()
                printfn "Sending peer info"
                sendPeerInfo peer message.SenderEndPoint.Address message.SenderEndPoint.Port
                None
                
            | Incoming.StatusChanged ->
                let id     = message.SenderConnection.RemoteUniqueIdentifier.ToString ()
                let status = enum<NetConnectionStatus> (message.ReadInt32 ())
                if status = NetConnectionStatus.Connected then
                    let reason = message.SenderConnection.RemoteHailMessage.ReadString ()
                    printfn "%s reports: %A - %s" id status reason
                None
                        
            | Incoming.UnconnectedData ->
                printfn "Unconnected data: %s" (message.ReadString ())
                None
            
            | _ ->
                printfn "Unhandled message type: %A! %s" message.MessageType (message.ReadString ())
                None
    
    if message <> null then
        peer.Recycle message
        match mail with
        | Some m ->
            let mailbox' = Mailbox.Receive mailbox (fst m) (snd m)
            processIncomingMessages peer mailbox'
        | None -> processIncomingMessages peer mailbox
    else
        mailbox

//Send messages to all peers
let processOutgoingMessages (peer : NetPeer) mailbox =
    let outbox = mailbox.Outbox
    if Map.isEmpty outbox then
        mailbox
    else
        match peer.Connections with
        | null -> mailbox
        | _ ->
            if peer.ConnectionsCount > 0 then
                (*
                List.iter
                    (fun elem ->
                         match elem with
                         | NewPlayerSlave (id, pos) ->
                             printfn "Sending player creation with ID %A" id
                             let message = peer.CreateMessage ()
                             message.Write (int MessageType.NewPlayerSlave)
                             message.Write (id)
                             message.Write (pos.X)
                             message.Write (pos.Y)
                             peer.SendMessage (message, peer.Connections, NetDeliveryMethod.ReliableOrdered, 0)
                         | Transform (id, pos, orientation, scale) ->
                             printfn "Sending position message! %A %A" id pos
                             let message = peer.CreateMessage ()
                             message.Write (int MessageType.Transform)
                             message.Write (id)
                             message.Write (pos.X)
                             message.Write (pos.Y)
                             message.Write (float32 orientation)
                             message.Write (float32 scale)
                             peer.SendMessage (message, peer.Connections, NetDeliveryMethod.ReliableOrdered, 0)
                         | _ ->
                             printfn "Unhandled net mail type: %A" elem) outbox
                { mailbox with Outbox = List.empty }
                *)
                mailbox
            else
                mailbox
