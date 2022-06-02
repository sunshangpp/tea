val last_id : int ref

type style =
  { frames : string array
  ; fps : Core.Time.Span.t
  }

type t =
  { id : int
  ; style : style
  ; img_attr : Notty.attr
  ; frame : int
  }

val line : style
val dot : style
val minidot : style
val jump : style
val pulse : style
val points : style
val globe : style
val moon : style
val monkey : style
val tick : t -> [> `Spinner_tick of int ] Cmd.t

val create
  :  ?style:
       [< `Dot
       | `Globe
       | `Jump
       | `Line
       | `MiniDot
       | `Monkey
       | `Moon
       | `Points
       | `Pulse > `Dot
       ]
  -> ?img_attr:Notty.attr
  -> unit
  -> t

val update : t -> [> `Spinner_tick of int ] -> t * [> `Spinner_tick of int ] Cmd.t
val view : t -> Notty.image
