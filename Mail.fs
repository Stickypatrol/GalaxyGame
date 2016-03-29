(*
   Mailbox and mail functionality
*)

module Mail

//Used to identify the type of information contained in the message
type MessageType =
    | PeerInformation = 0
    | Create          = 1
    | Destroy         = 2
    | Position        = 3
    | Rotation        = 4

//A sample enumeration of possible types of targets
type MailTarget =
    | Global = 0
    | Entity = 1

//A unique identifier
type ID = int

//Sample of possible contents of a message
type Mail =
    | Create   of int
    | Destroy  of int
    | Position of double * double
    | Rotation of double

//Contains incoming and outgoing messages
type Mailbox =
    { Inbox  : Map<MailTarget * ID, Mail List>
      Outbox : Map<MailTarget * ID, Mail List> } with
    static member Zero =
        { Inbox  = Map.empty
          Outbox = Map.empty }
    static member Send mailbox target id mail =
        let existingMail =
            match Map.tryFind (target, id) mailbox.Outbox with
            | Some m -> m
            | None   -> List.empty
        { mailbox with Outbox = mailbox.Outbox |> Map.add (target, id) (mail :: existingMail) }
    static member SendUnique mailbox target id mail =
        match Map.tryFind (target, id) mailbox.Outbox with
        | Some m ->
            if List.exists (fun e -> e = mail) m then
                mailbox
            else
                Mailbox.Send mailbox target id mail
        | None ->
            Mailbox.Send mailbox target id mail
    static member Receive mailbox target id mail =
        let existingMail =
            match Map.tryFind (target, id) mailbox.Inbox with
            | Some m -> m
            | None   -> List.empty
        { mailbox with Inbox = mailbox.Inbox |> Map.add (target, id) (mail :: existingMail) }
    static member ReceiveUnique mailbox target id mail =
        match Map.tryFind (target, id) mailbox.Inbox with
        | Some m ->
            if List.exists (fun e -> e = mail) m then
                mailbox
            else
                Mailbox.Receive mailbox target id mail
        | None ->
            Mailbox.Receive mailbox target id mail
