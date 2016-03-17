(*
   Core game logic
*)

namespace Monaco

open UnityEngine
open Lidgren.Network
open Coroutine
open Auxiliary
open Mailbox
open Network
open System

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
        state //<- State.Initialize state mode localPort remotePort maxCons

    member this.Update () =
        state <- State.Update Time.deltaTime state
        if state.ExitFlag then
            Application.Quit ()

    member this.OnGUI () =
        state //<- State.OnGUI state

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
    {
      Planets     : List<Entity<State, PlanetFields, Mailbox>> //add the planets here
      Ships       : List<Entity<State, ShipFields, Mailbox>>  //add the ships here
      IDList      : List<int>
      Mailbox     : Mailbox
      Random      : Random
      ExitFlag    : bool
      NetConfig   : NetPeerConfiguration Option
      NetPeer     : NetPeer Option
      Prefabs     : Map<string, GameObject>
    } with
    static member Zero =
      {
        Planets     = []
        Ships       = []
        IDList      = []
        Mailbox     = Mailbox.Zero
        Random      = new Random()
        ExitFlag    = false
        NetConfig   = None
        NetPeer     = None
        Prefabs     = Map.empty
      }
    static member Example =
      {
        Planets     = [{};{}]
        Ships       = []
        IDList      = []
        Mailbox     = Mailbox.Zero
        Random      = new Random()
        ExitFlag    = false
        NetConfig   = None
        NetPeer     = None
        Prefabs     = Map.empty
      }
    static member Update dt s =
        if Input.GetKey (KeyCode.Escape) then
            { s with ExitFlag = true }
        else
            s
and PlanetFields =
    {
      ID          : int
      Owner       : int
      GameObject  : GameObject
      Attack      : int
      Defense     : int
      Research    : int
      Neighbours  : List<int>
    }with
    static member Zero (id:int) =
      let go = GameObject.CreatePrimitive(PrimitiveType.Cube)
      ignore <| go.AddComponent<CircleCollider2D>()
      ignore <| go.AddComponent<Transform>()
      {
        ID = id
        Owner = -1
        GameObject = go //make a transform and a collider of istrigger type
        Attack = 0
        Defense = 0
        Research = 0
        Neighbours = []
      }
and ShipFields =
    {
      ID          : int
      Owner       : int
      GameObject  : GameObject
      Attack      : float
      Target      : Option<int> //this target is where the troop is going or what its attacking IF ITS WITHIN ATTACK RANGE
    }with
    static member Zero (id:int) =
      let go = GameObject.CreatePrimitive(PrimitiveType.Cube)
      ignore <| go.AddComponent<CircleCollider2D>()
      ignore <| go.AddComponent<Transform>()
      {
        ID = id
        Owner = -1
        GameObject = go
        Attack = 0.0
        Target = None
      }
    static member CreateNew(x,y) =
      let go = GameObject.CreatePrimitive(PrimitiveType.Cube)
      ignore <| go.AddComponent<CircleCollider2D>()
      ignore <| go.AddComponent<Transform>()
      go.transform.Translate(x, y, 0.0f)
      {
        ID = id
        Owner = -1
        GameObject = go
        Attack = 0.0
        Target = None
      }