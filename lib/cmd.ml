open Async

type 'a t =
  | None
  | Batch of 'a t list
  | Call of (unit -> 'a Deferred.t)

let none : 'a t = None
let batch cmds = Batch cmds
let call f = Call f
let exit : 'a t = call (fun () -> return `Exit)
