(*
   Economy database and data manipulation logic
*)

module Economy

open Lens

//Data store for various kinds of capital
type Capital =
    { Financial : int
      Human     : int
      Social    : int } with
    static member Zero =
        { Financial = 0
          Human     = 0
          Social    = 0 }
    static member financial =
        { Get = fun x -> x.Financial
          Set = fun v x -> {x with Financial = v} }
    static member human =
        { Get = fun x -> x.Human
          Set = fun v x -> {x with Human = v} }
    static member social =
        { Get = fun x -> x.Social
          Set = fun v x -> {x with Social = v} }

//Data store for participants
type Participant =
    { Capital : Capital
      Energy  : int } with
    static member Zero =
        { Capital = Capital.Zero
          Energy  = 100 }
    static member capital =
        { Get = fun x -> x.Capital
          Set = fun v x -> {x with Capital = v} }
    static member energy =
        { Get = fun x -> x.Energy
          Set = fun v x -> {x with Energy = v} }

//Data store for the virtual economy
type Economy =
    { Participants : Map<int, Participant> } with
    static member Zero =
        { Participants = Map.empty }
    static member participants =
        { Get = fun x -> x.Participants
          Set = fun v x -> {x with Participants = v} }

//Lenses for easy data access and manipulation
let economyParticipants  = Economy.participants
let participantFinancial = Participant.capital >>| Capital.financial
let participantHuman     = Participant.capital >>| Capital.human
let participantSocial    = Participant.capital >>| Capital.social