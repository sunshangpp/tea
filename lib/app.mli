type ('args, 'model, 'msg) config =
  { init : 'args -> 'model * 'msg Cmd.t
  ; update : 'model -> 'msg -> 'model * 'msg Cmd.t
  ; view : 'model -> Notty.image
  }

val handle_cmd : 'a Async.Pipe.Writer.t -> 'a Cmd.t -> unit

val run
  :  ?mode:[< `Inline | `Fullscreen > `Inline ]
  -> (unit, 'model, [> `Exit | `Key of Notty.Unescape.key ]) config
  -> 'model Async.Deferred.t

val run_unit
  :  ?mode:[< `Inline | `Fullscreen > `Inline ]
  -> (unit, 'model, [> `Exit | `Key of Notty.Unescape.key ]) config
  -> unit Async.Deferred.t
