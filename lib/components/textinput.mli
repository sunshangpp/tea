val default_placeholder_attr : Notty.attr
val default_prompt_attr : Notty.attr
val default_text_attr : Notty.attr

type t =
  { text : string
  ; text_attr : Notty.attr
  ; placeholder : string
  ; placeholder_attr : Notty.attr
  ; prompt : string
  ; prompt_attr : Notty.attr
  ; cursor_pos : int
  ; focus : bool
  ; echo_mode : [ `Normal | `Password ]
  }

val focus : t -> t
val unfocus : t -> t
val toggle_focus : t -> t
val focused : t -> bool
val content : t -> string
val insert_char_before_cursor : t -> char -> t
val delete_char_before_cursor : t -> t
val delete_char_under_cursor : t -> t
val delete_all_before_cursor : t -> t
val move_cursor_left : t -> t
val move_cursor_right : t -> t
val move_cursor_to_beginning : t -> t
val move_cursor_to_end : t -> t

val create
  :  ?text_attr:Notty.attr
  -> ?prompt:string
  -> ?prompt_attr:Notty.attr
  -> ?placeholder:string
  -> ?placeholder_attr:Notty.attr
  -> ?focus:bool
  -> ?echo_mode:[ `Normal | `Password ]
  -> unit
  -> t

val update : t -> [> `Key of Notty.Unescape.key ] -> t * 'a Cmd.t
val placeholder_view : t -> Notty.image
val text_view : t -> Notty.image
val view : t -> Notty.image
