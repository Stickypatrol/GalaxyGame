(*
   Common extra functionality
*)

module Auxiliary

let random = System.Random ((int) System.DateTime.Now.Ticks)
let newID () = hash (random.Next ())

type Error =
    | Exception of string

type ErrorResult<'success, 'failure> =
    | Success of 'success
    | Failure of 'failure