(*
   Core game logic
*)

namespace Monaco

open UnityEngine
open Lidgren.Network
open Coroutine
open Auxiliary
open Mail
open Contact
open System

//This component should be added to a Unity Game Controller object
type MonacoLogic () =
    inherit MonoBehaviour ()

    [<Header ("Connection settings")>]
    [<SerializeField>]
    /// <summary>Mode</summary>
    let mutable mode       = NetMode.Create

    [<SerializeField>]
    /// <summary>Local port</summary>
    let mutable localPort  = 8888

    [<SerializeField>]
    /// <summary>Remote port</summary>
    let mutable remotePort = 9999

    [<SerializeField>]
    /// <summary>Maximum connections</summary>
    let mutable maxCons    = 128

    //The base game state
    let mutable state = State.Zero

    member this.LoadPrefabs filenames =
        let attemptLoad =
            fun acc filename ->
                let gameObject = Resources.Load<GameObject> ("Prefabs/" + filename)
                match gameObject with
                | null -> Debug.LogError ("Failed to load prefab " + filename); acc
                | _    -> (filename, gameObject) :: acc

        List.fold attemptLoad List.empty filenames |> Map.ofList

    member this.Start () =
        state <- {state with Prefabs = this.LoadPrefabs ["Checkpoint";]}

    member this.Update () =
        Debug.Log(state)
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
      GameObject  : Option<GameObject>
      //optional Research    : int
      Neighbours  : List<int>
    }with
    static member Zero() =
      {
        ID = -1
        Owner = -1
        Neighbours = []
        GameObject = None
      }
    static member Move p (v:Vector2) =
      match p.GameObject with
      | Some(go) -> go.transform.Translate(Vector3(v.x,v.y,0.0f))
                    p
      | None -> p
    static member Rules =
      List<State -> CheckpointFields -> float32 -> CheckpointFields>.Empty//example stuff
    static member Scripts : Coroutine<State, Mailbox, CheckpointFields, Unit> list=
      [(co{return ()})]

and PingFields =
    {
      ID          : int 
      Owner       : int
      Velocity    : Vector2
      GameObject  : Option<GameObject>
    }with
    member this.ApplyVelocity() =
      match this.GameObject with
      | Some(go) -> go.transform.Translate(this.Velocity.x, this.Velocity.y, 0.0f)
                    this
      | None -> this
    static member Zero() : PingFields =
      {
        ID = -1
        Owner = -1
        GameObject = None
        Velocity = Vector2.zero
      }
    static member Move p (v:Vector2) =
      match p.GameObject with
      | Some(go) -> go.transform.Translate(Vector3(v.x,v.y,0.0f))
                    p
      | None -> p
    static member Rules =
      [(fun s p dt -> p.ApplyVelocity())]
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
        GameObject = Some(go)
        Velocity = Vector2.zero
      }

//The global game state
and State =
    {
      CheckPoints : List<Entity<State, CheckpointFields, Mailbox>> //add the planets here
      Pings       : List<Entity<State, PingFields, Mailbox>>  //add the ships here
      IDList      : List<int>
      Networking  : ContactSession
      Random      : Random
      ExitFlag    : bool
      Prefabs     : Map<string, GameObject> //we need some GUI shit in the state as well, sjors' responsibility
    } with
    static member Zero =
      //below is just some example stuff
      {
        CheckPoints = []
        Pings       = []
        IDList      = []
        Networking  = ContactSession.Zero
        Random      = new Random()
        ExitFlag    = false
        Prefabs     = Map.empty
      }
    static member Example() = //this is only used for an example of setting up the game
        { State.Zero with
              CheckPoints = [ (Entity<State,CheckpointFields, Mailbox>.CheckPointZero());
                              (Entity<State,CheckpointFields, Mailbox>.CheckPointZero())] //this is how we make 2 entities at the start of the state of the game
        }
    static member Update dt s =
        if Input.GetKey (KeyCode.Escape) then
            { s with ExitFlag = true }
        else
            s
