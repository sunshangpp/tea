type 'a t =
  | None
  | Batch of 'a t list
  | Call of (unit -> 'a Async.Deferred.t)

val none : 'a t
val batch : 'a t list -> 'a t
val call : (unit -> 'a Async.Deferred.t) -> 'a t
val exit : [> `Exit ] t
