val default_highlight_attr : Notty.attr

type t =
  { items : string array
  ; cursor : int
  ; highlight_attr : Notty.attr
  }

val create : items:string list -> ?highlight_attr:Notty.attr -> unit -> t
val selected : t -> string
val cursor_up : t -> t
val cursor_down : t -> t
val update : t -> [> `Key of Notty.Unescape.key ] -> t * 'a Cmd.t
val view : t -> Notty.image
