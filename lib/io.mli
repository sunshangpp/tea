open Async
open Notty

val keypress_reader_inline : unit -> [> `Key of Unescape.key ] Pipe.Reader.t
val keypress_reader_term : Notty_async.Term.t -> [> `Key of Unescape.key ] Pipe.Reader.t
val output_image : ?cap:Cap.t -> ?fd:Writer.t lazy_t -> image -> unit Deferred.t

val output_image_size
  :  ?cap:Cap.t
  -> ?fd:Writer.t lazy_t
  -> (int * int -> image)
  -> unit Deferred.t

val show_cursor : ?cap:Cap.t -> ?fd:Writer.t lazy_t -> bool -> unit Deferred.t

val move_cursor
  :  ?cap:Cap.t
  -> ?fd:Writer.t lazy_t
  -> [ `By of int * int | `Home | `To of int * int ]
  -> unit Deferred.t

val eol : image -> image
val clear : image -> unit Deferred.t
