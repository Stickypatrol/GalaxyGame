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
open Economy

//This component should be added to a Unity Game Controller object
type MonacoLogic () =
    inherit MonoBehaviour ()

    [<Header ("Connection settings")>]
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

    member this.LoadPrefabs filenames =
        let attemptLoad =
            fun acc filename ->
                let gameObject = Resources.Load<GameObject> ("Prefabs/" + filename)
                match gameObject with
                | null -> Debug.LogError ("Failed to load prefab " + filename); acc
                | _ -> (filename, gameObject) :: acc

        List.fold attemptLoad List.empty filenames |> Map.ofList

    member this.Start () =
        let p0 = Participant.Zero |> (participantHuman += 100) |> (participantHuman -= 10)
        Debug.Log (p0.Capital.Human.ToString ())

        state <- State.Initialize state mode localPort remotePort maxCons

        let prefabFilenames = [ "PrefabBoy"
                                "PrefabGirl"
                                "PrefabClock" ]
        state <- State.SetPrefabs state (this.LoadPrefabs prefabFilenames)

    member this.Update () =
        state <- State.Update Time.deltaTime state
        if state.ExitFlag then
            Application.Quit ()

    member this.OnGUI () =
        state <- State.OnGUI state

//Global game object container
and Entity<'w, 'fs, 'mailbox> =
    { Fields     : 'fs
      Rules      : List<'w -> 'fs -> float32 -> 'fs>
      Scripts    : Coroutine<'w, 'mailbox, 'fs, unit>
      GUIScripts : Coroutine<'w, 'mailbox, 'fs, unit> } with
    static member Create (fields, rules, scripts, guiScripts) =
        { Fields     = fields
          Rules      = rules
          Scripts    = andPassMany_ scripts
          GUIScripts = andPassMany_ guiScripts }

    member this.Update (world, mailbox, dt) =
        let fsRules = this.Rules |> List.fold (fun fs rule -> rule world fs dt) this.Fields
        let mailbox', fsScripts, scripts' = step_ (this.Scripts world mailbox fsRules)
        { this with
            Fields  = fsScripts
            Rules   = this.Rules
            Scripts = scripts' }, mailbox'

    member this.OnGUI (world, mailbox) =
        let mailbox', fs', guiScripts' = step_ (this.GUIScripts world mailbox this.Fields)
        { this with
            Fields     = fs'
            GUIScripts = guiScripts' }, mailbox'
//Game data
and GameFields =
    { ID         : int
      CurrentMap : string
      ActionMap  : Map<string, (string * Coroutine<Economy, Participant, string, unit>) List>
      Economy    : Economy }
    static member Zero =
        { ID         = 0
          CurrentMap = "Main"
          ActionMap  =
              [ ("Main", [ ("Financial capital",
                            co { do! setInnerState_ "Financial"})

                           ("Human capital",
                            co { do! setInnerState_ "Human"})

                           ("Social capital",
                            co { do! setInnerState_ "Social" }) ])

                ("Financial", [ ("Perform",
                                 co { do! setInnerState_ "Performance" })

                                ("Get a loan",
                                 co { let! (participant : Participant) = getOuterState_
                                      do! setOuterState_ { participant with
                                                               Capital = { participant.Capital with Financial = participant.Capital.Financial + 1000 }
                                                               Energy  = participant.Energy - 10 }
                                      do! setInnerState_ "Main" })

                                ("Invest",
                                 co { let! (participant : Participant) = getOuterState_
                                      do! setOuterState_ { participant with
                                                               Capital = { participant.Capital with Financial = participant.Capital.Financial - 100 }
                                                               Energy  = participant.Energy - 5 }
                                      do! setInnerState_ "Main" }) ])

                ("Human", [ ("Hire person",
                             co { let! (participant : Participant) = getOuterState_
                                  do! setOuterState_ { participant with
                                                           Capital = { participant.Capital with 
                                                                           Financial = participant.Capital.Financial - 200
                                                                           Human     = participant.Capital.Human + 100 }
                                                           Energy  = participant.Energy - 5 }
                                  do! setInnerState_ "Main" })

                            ("Fire person",
                             co { let! (participant : Participant) = getOuterState_
                                  do! setOuterState_ { participant with
                                                           Capital = { participant.Capital with
                                                                           Human  = participant.Capital.Human - 100
                                                                           Social = participant.Capital.Social - 10 }
                                                           Energy  = participant.Energy - 10 }
                                  do! setInnerState_ "Main" }) ])

                ("Social", [ ("Give presents to children",
                              co { let! (participant : Participant) = getOuterState_
                                   do! setOuterState_ { participant with
                                                            Capital = { participant.Capital with Social = participant.Capital.Social + 100 }
                                                            Energy  = participant.Energy - 10 }
                                   do! setInnerState_ "Main" })

                             ("Write an article on Facebook for teenagers",
                              co { let! (participant : Participant) = getOuterState_
                                   do! setOuterState_ { participant with
                                                            Capital = { participant.Capital with Social = participant.Capital.Social + 100 }
                                                            Energy  = participant.Energy - 10 }
                                   do! setInnerState_ "Main" })

                             ("Buy coffee for adolescents",
                              co { let! (participant : Participant) = getOuterState_
                                   do! setOuterState_ { participant with
                                                            Capital = { participant.Capital with Social = participant.Capital.Social + 100 }
                                                            Energy  = participant.Energy - 10 }
                                   do! setInnerState_ "Main" })

                             ("Be charitable to the elderly",
                              co { let! (participant : Participant) = getOuterState_
                                   do! setOuterState_ { participant with
                                                            Capital = { participant.Capital with Social = participant.Capital.Social + 100 }
                                                            Energy  = participant.Energy - 10 }
                                   do! setInnerState_ "Main" }) ])

                ("Performance", [ ("Perform on the street",
                                   co { let! (participant : Participant) = getOuterState_
                                        do! setOuterState_ { participant with
                                                                 Capital = { participant.Capital with
                                                                                 Financial = participant.Capital.Financial + 100
                                                                                 Social    = participant.Capital.Social + 150 }
                                                                 Energy  = participant.Energy - 20 }
                                        do! setInnerState_ "Main" })

                                  ("Perform at a club",     
                                   co { let! (participant : Participant) = getOuterState_
                                        do! setOuterState_ { participant with
                                                                 Capital = { participant.Capital with
                                                                                Financial = participant.Capital.Financial + 250
                                                                                Social    = participant.Capital.Social + 100 }
                                                                 Energy    = participant.Energy - 25 }
                                        do! setInnerState_ "Main" }) ]) ] |> Map.ofList
          Economy = { Economy.Zero with Participants = ([ 0, Participant.Zero ] |> Map.ofList) } }

    static member Rules =
        [ fun w fs dt -> fs ]

    static member Scripts =
        [ co { do! yield_ } |> repeat_ ]

    static member WaitOnceScript =
        co { do! wait_ 3.0 } |> times_ 1

    static member ActionsGUIScript =
        co { do! yield_
             let! w = getGlobalState_
             let! mailbox = getOuterState_
             let! (fs : GameFields) = getInnerState_
             let actions = Map.tryFind fs.CurrentMap fs.ActionMap
             match actions with
             | Some m ->
                let rec ProcessActions index list =
                    match list with
                    | (x : string * Coroutine<Economy, Participant, string, unit>) :: xs ->
                        if GUI.Button (new Rect (float32 Screen.width * float32 0.1,
                                                 float32 (double Screen.height * 0.9 - double index * (double Screen.height * 0.06)),
                                                 float32 Screen.width * float32 0.8,
                                                 float32 Screen.height * float32 0.05), fst x) then
                            Some (snd x)
                        else
                            ProcessActions (index + 1) xs
                    | [] -> None
                let newRoutine = ProcessActions 0 (List.rev m)
                match newRoutine with
                | Some r ->
                    let participant = Map.tryFind 0 fs.Economy.Participants
                    match participant with
                    | Some p ->
                        let participant, nextMap, r' = step_ (r fs.Economy p "")
                        do! setInnerState_ { fs with
                                                Economy    = { fs.Economy with Participants = Map.add 0 participant fs.Economy.Participants }
                                                CurrentMap = nextMap }
                    | None -> ()
                | None -> ()
             | None -> () } |> repeat_

    static member EconomyGUIScript =
        co { do! yield_
             let! fs = getInnerState_
             let participant = Map.tryFind 0 fs.Economy.Participants
             let financialCapital, humanCapital, socialCapital, energy =
                match participant with
                | Some p ->
                    (p |> participantFinancial.Get,
                     p |> participantHuman.Get,
                     p |> participantSocial.Get,
                     p.Energy)
                | None -> (-1, -1, -1, -1)
             GUI.Label (new Rect (float32 Screen.width * float32 0.05,
                                  float32 Screen.height * float32 0.05,
                                  float32 Screen.width * float32 0.3,
                                  float32 Screen.height * float32 0.15),
                        "Financial capital: " + (financialCapital.ToString ()) +
                        "\nHuman capital: "   + (humanCapital.ToString ()) +
                        "\nSocial capital: "  + (socialCapital.ToString ()) +
                        "\nEnergy: "          + (energy.ToString ()),
                        new GUIStyle (GUI.skin.box)) |> ignore } |> repeat_

    static member GUIScripts =
        [ GameFields.WaitOnceScript
          GameFields.ActionsGUIScript
          GameFields.EconomyGUIScript ]

    static member id =
        { Get = fun (x : GameFields) -> x.ID
          Set = fun v (x : GameFields) -> {x with ID = v} }

//Character data
and CharacterFields =
    { ID        : int
      Name      : string
      Direction : bool
      Transform : Transform Option } with
    static member Zero =
        { ID        = 0
          Name      = "Character"
          Direction = true
          Transform = None }
    static member TransformRule =
        fun w (fs : CharacterFields) dt ->
            match fs.Transform with
            | Some t ->
                if fs.Direction then
                    t.Translate (Vector3 (float32 1.0, float32 0.0, float32 0.0) * dt)
                else
                    t.Translate (Vector3 (float32 -1.0, float32 0.0, float32 0.0) * dt)
            | None -> ()
            fs
    static member DirectionScript =
        co { do! wait_ 2.0
             let! fs = getInnerState_
             do! setInnerState_ { fs with Direction = not fs.Direction } } |> repeat_
    static member Rules =
        [ CharacterFields.TransformRule ]
    static member Scripts =
        [ CharacterFields.DirectionScript ]
    static member GUIScripts =
        [ co { do! yield_ } |> repeat_ ]
    static member id =
        { Get = fun (x : CharacterFields) -> x.ID
          Set = fun v (x : CharacterFields) -> {x with ID = v} }
    static member name =
        { Get = fun (x : CharacterFields) -> x.Name
          Set = fun v (x : CharacterFields) -> {x with Name = v} }
    static member direction =
        { Get = fun (x : CharacterFields) -> x.Direction
          Set = fun v (x : CharacterFields) -> {x with Direction = v} }
    static member transform =
        { Get = fun (x : CharacterFields) -> x.Transform
          Set = fun v (x : CharacterFields) -> {x with Transform = v} }

//The global game state
and State =
    { Game       : Entity<State, GameFields, Mailbox>
      Characters : Entity<State, CharacterFields, Mailbox> List
      Mailbox    : Mailbox
      ExitFlag   : bool
      NetConfig  : NetPeerConfiguration Option
      NetPeer    : NetPeer Option
      Prefabs    : Map<string, GameObject> } with
    static member Zero =
        let newBoy ()  = NewCharacter ("Bob", "PrefabBoy",
                                       new Vector3(float32 1.0, float32 2.0, float32 0.0), true)
        let newGirl () = NewCharacter ("Alice", "PrefabGirl",
                                       new Vector3(float32 7.0, float32 2.0, float32 0.0), false)

        { Game       = Entity<State, GameFields, Mailbox>.Create
                           ({ GameFields.Zero with ID = newID () },
                            GameFields.Rules,
                            GameFields.Scripts,
                            GameFields.GUIScripts)
          Characters = List.empty
          Mailbox    = { Inbox  = [ 0, [ newBoy ()
                                         newGirl () ] ] |> Map.ofList
                         Outbox = Map.empty }
          ExitFlag   = false
          NetConfig  = None
          NetPeer    = None
          Prefabs    = Map.empty }

    static member Initialize s mode localPort remotePort maxCons =
        s

    static member Terminate s =
        s

    static member SetPrefabs s prefabs =
        { s with Prefabs = prefabs }

    static member StartNetworking s mode localPort remotePort maxCons =
        s

    static member StopNetworking s =
        s

    static member SendMail s =
        s

    static member ReceiveMail s =
        s

    static member UpdateEntities dt s =
        //Update existing entities
        let game', mailbox' = s.Game.Update (s, s.Mailbox, dt)

        let characters', mailbox'' =
            List.fold (fun (characters, mailbox) (character : Entity<State, CharacterFields, Mailbox>) ->
                           let character', mailbox' = character.Update (s, mailbox, dt)
                           ((character' :: characters), mailbox')) (List.empty, mailbox') s.Characters

        { s with
              Game       = game'
              Characters = characters'
              Mailbox    = mailbox'' }

    static member Emerge s =
        match Map.tryFind 0 s.Mailbox.Inbox with
        | Some stateInbox ->
            let newCharacters, inbox' =
                List.fold (fun (characters, inbox) mail ->
                     match mail with
                     | NewCharacter (name, prefab, pos, dir) ->
                         let id = newID ()
                         let transform =
                             match Map.tryFind prefab s.Prefabs with
                             | Some p ->
                                 let unityObject : UnityEngine.Object =
                                     GameObject.Instantiate (p, pos, Quaternion.identity)
                                 let go = unityObject :?> GameObject
                                 do go.name <- id.ToString ()
                                 Some go.transform
                             | None -> Debug.Log ("Couldn't find character transform!") ; None

                         //Create the local data for the object
                         let fs = { CharacterFields.Zero with
                                        ID        = id
                                        Name      = name
                                        Direction = dir
                                        Transform = transform }
                         let character = Entity<State, CharacterFields, Mailbox>.Create
                                             (fs,
                                              CharacterFields.Rules,
                                              CharacterFields.Scripts,
                                              CharacterFields.GUIScripts )

                         (character :: characters, inbox)
                     | _ -> (characters, mail :: inbox)) (List.empty, List.empty) stateInbox

            { s with
                  Characters = newCharacters @ s.Characters
                  Mailbox    = { s.Mailbox with Inbox = Map.add 0 inbox' s.Mailbox.Inbox } }
        | None -> s

    static member Cleanse s =
        match Map.tryFind 0 s.Mailbox.Inbox with
        | Some stateInbox ->
            let characters', inbox' =
                List.fold (fun (characters, inbox) mail ->
                    match mail with
                    | Destroy id ->
                        (List.filter (fun (e : Entity<State, CharacterFields, Mailbox>) ->
                             id <> e.Fields.ID) characters, inbox)
                    | _ -> (characters, mail :: inbox)) (s.Characters, List.empty) stateInbox
            { s with
                  Characters = characters'
                  Mailbox    = { s.Mailbox with Inbox = Map.add 0 inbox' s.Mailbox.Inbox } }
        | None -> s

    static member Update dt s =
        if Input.GetKey (KeyCode.Escape) then
            { s with ExitFlag = true }
        else
            s |> State.ReceiveMail
              |> State.UpdateEntities dt
              |> State.SendMail
              |> State.Emerge
              |> State.Cleanse

    static member OnGUI s =
        (*
        let gui' =
            List.fold (fun gui (character : Entity<State, CharacterFields, Mailbox, GUI>) ->
                           character.OnGUI (s, gui)) s.GUI s.Characters*)

        let game', mailbox' = s.Game.OnGUI (s, s.Mailbox)

        { s with
            Game    = game'
            Mailbox = mailbox' }

//GUI records
and GUI = GUIElement list
and GUIElement =
    | Box    of Rect * GUIContent
    | Button of Rect * string
