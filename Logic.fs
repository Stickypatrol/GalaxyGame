(*
   Core game logic
*)

namespace Monaco

open UnityEngine
open Lidgren.Network
open Coroutine
open Lens
open Math
open Mailbox
open Network

//This component should be added to a Unity Game Controller object
type GameLogicComponent () =
    inherit MonoBehaviour ()

    //Connection settings
    [<SerializeField>]
    let mutable mode       = NetMode.Create
    
    [<SerializeField>]
    let mutable localPort  = 8888
    
    [<SerializeField>]
    let mutable remotePort = 9999
    
    [<SerializeField>]
    let mutable maxCons    = 128

    //The base game state
    let mutable state = State.Zero
    
    member this.Start () =
        state <- State.Initialize state mode localPort remotePort maxCons
        ()
        
    member this.Update () =
        state <- State.Update state Time.deltaTime
        ()
    (*
    if state.ExitFlag then
        Application.Quit ()*)

//Used to manage communication between the logic and game objects
and EntityComponent () =
    inherit MonoBehaviour ()

//Global game object container
and Entity<'w, 'fs, 'mailbox> =
    { Fields  : 'fs
      Rules   : List<'w -> 'fs -> float32 -> 'fs>
      Scripts : Coroutine<'w, 'mailbox, 'fs, unit> } with
    static member Create(fields, rules, scripts) =
        { Fields  = fields
          Rules   = rules
          Scripts = andPassMany_ scripts }
    member this.Update(world, mailbox, dt) =
        let fsRules = this.Rules |> List.fold (fun fs rule -> rule world fs dt) this.Fields
        let mailbox', fsScripts, scripts' = step_ (this.Scripts world mailbox fsRules)

        { Fields  = fsScripts
          Rules   = this.Rules
          Scripts = scripts' }, mailbox'

//Button data
and ButtonFields =
    { ID   : int
      Text : string } with
    static member Zero =
        { ID   = 0
          Text = "No text specified" }
    static member Rules =
        [ fun w fs dt -> fs ]
    static member SlaveRules =
        [ fun w fs dt -> fs ]
    static member Scripts =
        [ co { do! yield_ } |> repeat_ ]
    static member SlaveScripts =
        [ co { do! yield_ } |> repeat_ ]
    static member Render =
        co { do! yield_ } |> repeat_
    static member id =
        { Get = fun (x : ButtonFields) -> x.ID
          Set = fun v (x : ButtonFields) -> {x with ID = v} }
    static member text =
        { Get = fun (x : ButtonFields) -> x.Text
          Set = fun v (x : ButtonFields) -> {x with Text = v} }

//Button data
and CubeFields =
    { ID       : int
      Position : Vector3 } with
    static member Zero =
        { ID       = 0
          Position = Vector3.zero }
    static member TransformScript =
        co { do! yield_
             let! fs = getInnerState_
             let! mailbox = getOuterState_
             do! setOuterState_ ({ mailbox with
                                       Inbox = (Translate (fs.ID,
                                                           Vector3 (float32 1.0,
                                                                    float32 0.0,
                                                                    float32 0.0)) :: mailbox.Outbox) }) } |> repeat_
    static member Rules =
        [ fun w fs dt -> fs ]
    static member SlaveRules =
        [ fun w fs dt -> fs ]
    static member Scripts =
        [ CubeFields.TransformScript ]
    static member SlaveScripts =
        [ co { do! yield_ } |> repeat_ ]
    static member Render =
        co { do! yield_ } |> repeat_
    static member id =
        { Get = fun (x : CubeFields) -> x.ID
          Set = fun v (x : CubeFields) -> {x with ID = v} }
    static member position =
        { Get = fun (x : CubeFields) -> x.Position
          Set = fun v (x : CubeFields) -> {x with Position = v} }

//The global game state
and State =
    { Buttons          : Entity<State, ButtonFields, Mailbox> List
      Cubes            : Entity<State, CubeFields, Mailbox> List
      Mailbox          : Mailbox
      ExitFlag         : bool
      NetConfig        : NetPeerConfiguration Option
      NetPeer          : NetPeer Option } with
    static member Zero =
        let newButton () =
            NewButton (newID (), new Rect (float32 0.0,
                                           float32 0.0,
                                           float32 100.0,
                                           float32 100.0), "test")

        let newCube () =
            NewCube (Vector3.zero)
        
        { Buttons          = List.empty
          Cubes            = List.empty
          Mailbox          = { Inbox  = [ newCube () ]
                               Outbox = List.empty }
          ExitFlag         = false
          NetConfig        = None
          NetPeer          = None }
    static member Initialize s mode localPort remotePort maxCons =
        s
    static member Terminate s =
        s
    static member LoadAssets s =
        s
    static member StartNetworking s mode localPort remotePort maxCons =
        (*
        let config, peer = startNetworking mode localPort remotePort maxCons
        { s with
              NetConfig = Some config
              NetPeer   = Some peer }*)
        s
    static member StopNetworking s =
        (*
        match s.NetPeer with
        | Some peer -> peer.Shutdown ("Finished program")
        | _ -> ()
        { s with NetPeer = None }
        *)
        s
    static member SendMail s =
        (*
        match s.NetPeer with
        | Some peer -> { s with Mailbox = processOutgoingMessages peer s.Mailbox }
        | _ -> s
        *)
        s
    static member ReceiveMail s =
        (*
        match s.NetPeer with
        | Some peer -> { s with Mailbox = processIncomingMessages peer s.Mailbox }
        | _ -> s
        *)
        s
    static member Update s dt =
        if Input.GetKey (KeyCode.Escape) then
            { s with ExitFlag = true }
        else
            //Receive messages
            let sReceive = State.ReceiveMail s
            
            //Update existing entities
            let buttons', mailbox' =
                List.fold (fun (buttons, mailbox) (button : Entity<State, ButtonFields, Mailbox>) ->
                               let button', mailbox' = button.Update (sReceive, mailbox, dt)
                               ((button' :: buttons), mailbox')) (List.empty, sReceive.Mailbox) sReceive.Buttons
            
            //Update existing buttons
            let cubes', mailbox'' =
                List.fold (fun (cubes, mailbox) (cube : Entity<State, CubeFields, Mailbox>) ->
                               let cube', mailbox' = cube.Update (sReceive, mailbox, dt)
                               ((cube' :: cubes), mailbox')) (List.empty, mailbox') sReceive.Cubes
            
            //Send messages
            let sSend = State.SendMail { sReceive with Mailbox = mailbox'' }
            
            //Spawn new entities
            let newButtons, newCubes, inbox' =
                List.fold (fun (buttons, cubes, inbox) mail ->
                               match mail with
                               | NewButton (id, r, t) ->
                                   let idLens   = ButtonFields.id
                                   let textLens = ButtonFields.text
                                   let button =
                                       Entity<State, ButtonFields, Mailbox>.Create(
                                           ButtonFields.Zero |> idLens.Set (newID ())
                                                             |> textLens.Set t,
                                           ButtonFields.Rules,
                                           ButtonFields.Scripts)
                                   (button :: buttons, cubes, inbox)
                               | NewCube (pos) ->
                                   let id = newID ()
                                   //Create the local data for the object
                                   let idLens  = CubeFields.id
                                   let posLens = CubeFields.position
                                   let cube =
                                       Entity<State, CubeFields, Mailbox>.Create(
                                           CubeFields.Zero |> idLens.Set id
                                                           |> posLens.Set pos,
                                           CubeFields.Rules,
                                           CubeFields.Scripts)
                                   
                                   //Spawn the game object
                                   let gameObject = GameObject.CreatePrimitive (PrimitiveType.Cube)
                                   do gameObject.name <- id.ToString()
                                   do gameObject.transform.localPosition <- pos

                                   (buttons, cube :: cubes, inbox)
                               | _ -> (buttons, cubes, mail :: inbox)) (List.empty, List.empty, List.empty) sSend.Mailbox.Inbox
            
            //Destroy entities for each relevant message
            let buttons'' =
                List.fold (fun buttons elem ->
                               match elem with
                               | Destroy id ->
                                   List.filter (fun (p : Entity<State, ButtonFields, Mailbox>) -> id <> p.Fields.ID) buttons
                               | _ -> buttons) (buttons' @ newButtons) sSend.Mailbox.Inbox
            
            let cubes'' =
                List.fold (fun cubes elem ->
                               match elem with
                               | Destroy id ->
                                   List.filter (fun (p : Entity<State, CubeFields, Mailbox>) -> id <> p.Fields.ID) cubes
                               | _ -> cubes) (cubes' @ newCubes) sSend.Mailbox.Inbox

            //Transform game objects according to message
            List.iter (fun mail ->
                           match mail with
                           | Translate (id, vec) ->
                             let gameObject =
                                 try
                                     Some (GameObject.Find (id.ToString()))
                                 with
                                 | :? UnityException -> None
                             match gameObject with
                             | Some go ->
                                 do go.transform.Translate (vec)
                             | None -> ()
                           | _ -> ()) inbox'
            
            { sSend with
                  Buttons     = buttons''
                  Cubes       = cubes''
                  Mailbox     = { Inbox  = List.empty //inbox'
                                  Outbox = List.empty } }

    (*
        let connected () =
                match s.NetPeer with
                | Some peer ->
                    (peer.ConnectionsCount > 0)
                | _ -> false
            
            if not (connected ()) then
                //Keep checking for at least one connection
                let sReceive = State.ReceiveMail sInput
                let sSend = State.SendMail sInput
                sSend
*)

