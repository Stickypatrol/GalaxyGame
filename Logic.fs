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
    static member CheckPointZero() =
      Entity<State, CheckpointFields, Mailbox>.Create((CheckpointFields.Zero()), CheckpointFields.Rules, CheckpointFields.Scripts)
    static member PingZero() =
      Entity<State, PingFields, Mailbox>.Create ((PingFields.Zero()),
                                                  PingFields.Rules,
                                                  PingFields.Scripts)

and CheckpointFields = //we need a way to generate a graph of Entity<State, PlanetFields, Mailbox> with a bunch of neighbours and such
    {
      ID          : int
      Owner       : int
      GameObject  : GameObject
      //optional Research    : int
      Neighbours  : List<int>
    }with
    static member Zero() =
      let go = GameObject.CreatePrimitive(PrimitiveType.Cube)
      ignore <| go.AddComponent<CircleCollider2D>()
      ignore <| go.AddComponent<Transform>()
      {
        ID = -1
        Owner = -1
        Neighbours = []
        GameObject = go //make a transform and a collider of istrigger type
      }
    static member Move p (x, y) =
      ignore <| p.GameObject.transform.Translate(Vector3(x,y,0.0f))
      p
    static member Rules =
      List<State -> CheckpointFields -> float32 -> CheckpointFields>.Empty//example stuff
    static member Scripts : Coroutine<State, Mailbox, CheckpointFields, Unit> list=
      [(co{return ()})]

and PingFields =
    {
      ID          : int 
      Owner       : int
      Velocity    : Vector2
      GameObject  : GameObject
    }with
    static member Zero() : PingFields =
      let go = GameObject.CreatePrimitive(PrimitiveType.Cube)
      ignore <| go.AddComponent<CircleCollider2D>()
      ignore <| go.AddComponent<Transform>()
      {
        ID = -1
        Owner = -1
        GameObject = go
        Velocity = Vector2.zero
      }
    static member Move p (v:Vector2) =
      ignore <| p.GameObject.transform.Translate(Vector3(v.x,v.y,0.0f))
      p
    static member Rules =
      [(fun s p dt -> PingFields.Move p p.Velocity)]
    static member Scripts : Coroutine<State, Mailbox, PingFields, Unit> list=
      [(co{return ()})]
    static member CreateAt(id, x,y) =
      let go = GameObject.CreatePrimitive(PrimitiveType.Cube)
      ignore <| go.AddComponent<CircleCollider2D>()
      ignore <| go.AddComponent<Transform>()
      go.transform.Translate(x, y, 0.0f)
      {
        ID = id
        Owner = -1
        GameObject = go
        Velocity = Vector2.zero
      }

//The global game state
and State =
    {
      CheckPoints : List<Entity<State, CheckpointFields, Mailbox>> //add the planets here
      Pings       : List<Entity<State, PingFields, Mailbox>>  //add the ships here
      IDList      : List<int>
      Mailbox     : Mailbox
      Random      : Random
      ExitFlag    : bool
      NetConfig   : NetPeerConfiguration Option
      NetPeer     : NetPeer Option
      Prefabs     : Map<string, GameObject> //we need some GUI shit in the state as well, sjors' responsibility
    } with
    static member Zero =
      //below is just some example stuff
      let baseCP = CheckpointFields.Zero()
      {
        CheckPoints = [Entity.Create(baseCP.GameObject.transform.Translate(Vector2(100.0f, -100.0f)), CheckpointFields.Rules, CheckpointFields.Scripts)]
        Pings       = []
        IDList      = []
        Mailbox     = Mailbox.Zero
        Random      = new Random()
        ExitFlag    = false
        NetConfig   = None
        NetPeer     = None
        Prefabs     = Map.empty
      }
    static member Example() = //this is only used for an example of setting up the game
      {
        CheckPoints = [(Entity<State,CheckpointFields, Mailbox>.CheckPointZero())]
        Pings       = []
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