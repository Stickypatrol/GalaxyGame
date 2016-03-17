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
//Global game object container
and Entity<'w, 'fs, 'mailbox> =
    { Fields     : 'fs
      Rules      : List<'w -> 'fs -> float32 -> 'fs>
      Scripts    : Coroutine<'w, 'mailbox, 'fs, unit>} with
    static member Create (fields, rules, scripts) =
        { Fields     = fields
          Rules      = rules
          Scripts    = andPassMany_ scripts}

    member this.Update (world, mailbox, dt) =
        let fsRules = this.Rules |> List.fold (fun fs rule -> rule world fs dt) this.Fields
        let mailbox', fsScripts, scripts' = step_ (this.Scripts world mailbox fsRules)
        { this with
            Fields  = fsScripts
            Rules   = this.Rules
            Scripts = scripts' }, mailbox'
    static member PlanetZero () =
        {
          Fields = PlanetFields.Zero()
          Rules = PlanetFields.Rules
          Scripts = PlanetFields.Scripts
        }
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
    static member Example() =
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
    static member Update dt s =
        if Input.GetKey (KeyCode.Escape) then
            { s with ExitFlag = true }
        else
            s
and PlanetFields = //we need a way to generate a graph of Entity<State, PlanetFields, Mailbox> with a bunch of neighbours and such
    {
      ID          : int
      Owner       : int
      GameObject  : GameObject
      Attack      : int
      Defense     : int
      Research    : int
      Neighbours  : List<int>
    }with
    static member Zero() =
      let go = GameObject.CreatePrimitive(PrimitiveType.Cube)
      ignore <| go.AddComponent<CircleCollider2D>()
      ignore <| go.AddComponent<Transform>()
      {
        ID = -1
        Owner = -1
        GameObject = go //make a transform and a collider of istrigger type
        Attack = 0
        Defense = 0
        Research = 0
        Neighbours = []
      }
    static member Move p (x, y) =
      ignore <| p.GameObject.transform.Translate(Vector3(x,y,0.0f))
      p
    static member Rules =
      List<(State -> PlanetFields -> float32 -> PlanetFields)>.Empty
    static member Scripts =
      let x : Coroutine<State, Mailbox, PlanetFields, Unit> = co{do! yield_}
      x
and ShipFields =
    {
      ID          : int 
      Owner       : int
      GameObject  : GameObject
      Attack      : float
      Target      : Option<int> //this target is where the troop is going or what its attacking IF ITS WITHIN ATTACK RANGE
    }with
    static member Zero =
      let go = GameObject.CreatePrimitive(PrimitiveType.Cube)
      ignore <| go.AddComponent<CircleCollider2D>()
      ignore <| go.AddComponent<Transform>()
      {
        ID = -1
        Owner = -1
        GameObject = go
        Attack = 0.0
        Target = None
      }
    static member CreateAt(id, x,y) =
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