(*
   Communication functionality, including networking
*)

module Mailbox

open UnityEngine

//All accepted types of messages
type Mail =
    | NewButton of int * Rect * string

//Incoming or outgoing
type MailboxType =
    | Inbox
    | Outbox

type Mailbox =
    { Inbox  : Map<int, Mail List>
      Outbox : Map<int, Mail List> } with
    static member Zero =
        { Inbox  = Map.empty
          Outbox = Map.empty }
    static member Send mailbox id mail =
        let existingMail =
            match Map.tryFind id mailbox.Outbox with
            | Some existingMail -> existingMail
            | None -> List.empty
        { mailbox with Outbox = mailbox.Outbox |> Map.add id (mail :: existingMail) }
    static member SendUnique mailbox id mail =
        match Map.tryFind id mailbox.Outbox with
        | Some m ->
            if List.exists (fun e -> e = mail) m then
                mailbox
            else
                Mailbox.Send mailbox id mail
        | None ->
            Mailbox.Send mailbox id mail
    static member Receive mailbox id mail =
        let existingMail =
            match Map.tryFind id mailbox.Inbox with
            | Some existingMail -> existingMail
            | None -> List.empty
        { mailbox with Inbox = mailbox.Inbox |> Map.add id (mail :: existingMail) }
    static member ReceiveUnique mailbox id mail =
        match Map.tryFind id mailbox.Inbox with
        | Some m ->
            if List.exists (fun e -> e = mail) m then
                mailbox
            else
                Mailbox.Receive mailbox id mail
        | None ->
            Mailbox.Receive mailbox id mail