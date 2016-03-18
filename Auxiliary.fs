(*
   Elementary math
*)

module Auxiliary

let random = System.Random ((int) System.DateTime.Now.Ticks)
let newID () = hash (random.Next ())
