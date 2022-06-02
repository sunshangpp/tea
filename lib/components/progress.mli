val empty_cell : string
val filled_cell : string

type t =
  { width : int
  ; img_attr : Notty.attr
  ; percent : int
  }

val create : ?width:int -> ?img_attr:Notty.attr -> unit -> t
val calibrated_percent : int -> int
val incr_percent_by : t -> int -> t
val view : t -> Notty.image
