(*
   Communication functionality, including networking
*)

module Mailbox

open UnityEngine
open Lens

//Local communication, some of these are still wrong
type Mail =
    | NewButton of int * Rect * string
    | NewCube   of Vector3
    | Translate of int * Vector3
    | Rotate    of int * Vector3
    | Scale     of int * Vector3
    | Destroy   of int

//Incoming or outgoing
type MailboxType =
    | Inbox
    | Outbox

type Mailbox =
    { Inbox  : Mail List
      Outbox : Mail List } with
    static member Zero =
        { Inbox  = List.empty
          Outbox = List.empty }
    static member inbox =
        { Get = fun (x : Mailbox) -> x.Inbox
          Set = fun v (x : Mailbox) -> {x with Inbox = v} }
    static member outbox =
        { Get = fun (x : Mailbox) -> x.Outbox
          Set = fun v (x : Mailbox) -> {x with Outbox = v} }
