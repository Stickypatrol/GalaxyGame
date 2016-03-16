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
        state <- State.Initialize state mode localPort remotePort maxCons

        (*
        let prefabFilenames = [ "PrefabBoy"
                                "PrefabGirl"
                                "PrefabClock" ]
        state <- State.SetPrefabs state (this.LoadPrefabs prefabFilenames)
        *)

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

//The global game state
and State =
    { Mailbox    : Mailbox
      ExitFlag   : bool
      NetConfig  : NetPeerConfiguration Option
      NetPeer    : NetPeer Option
      Prefabs    : Map<string, GameObject> } with
    static member Zero =
        { Mailbox    = Mailbox.Zero
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
        s

    static member Emerge s =
        s

    static member Cleanse s =
        s

    static member Update dt s =
        if Input.GetKey (KeyCode.Escape) then
            { s with ExitFlag = true }
        else
            s

    static member OnGUI s =
        s
