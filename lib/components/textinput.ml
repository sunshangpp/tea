open! Base
open! Core
open Notty

let default_placeholder_attr = A.(fg (gray 6))
let default_text_attr = A.(fg (rgb_888 ~r:255 ~g:95 ~b:175))
let default_prompt_attr = default_text_attr
let echo_password_char = '*'

type t =
  { text : string
  ; text_attr : A.t
  ; placeholder : string
  ; placeholder_attr : A.t
  ; prompt : string
  ; prompt_attr : A.t
  ; cursor_pos : int
  ; focus : bool
  ; echo_mode : [ `Normal | `Password ]
  }

let focus t = { t with focus = true }
let unfocus t = { t with focus = false }
let toggle_focus t = { t with focus = not t.focus }
let focused t = t.focus
let content t = t.text

let insert_char_before_cursor t c =
  let prefix = String.prefix t.text t.cursor_pos in
  let suffix = String.suffix t.text (String.length t.text - t.cursor_pos) in
  let text = prefix ^ Char.to_string c ^ suffix in
  let cursor_pos = t.cursor_pos + 1 in
  { t with text; cursor_pos }
;;

let delete_char_before_cursor t =
  let cursor_pos = Int.max 0 (t.cursor_pos - 1) in
  let prefix = String.prefix t.text cursor_pos in
  let suffix = String.suffix t.text (String.length t.text - t.cursor_pos) in
  let text = prefix ^ suffix in
  { t with text; cursor_pos }
;;

let delete_char_under_cursor t =
  let prefix = String.prefix t.text t.cursor_pos in
  let suffix =
    String.suffix t.text (Int.max 0 (String.length t.text - t.cursor_pos - 1))
  in
  let text = prefix ^ suffix in
  { t with text }
;;

let delete_all_before_cursor t =
  { t with
    text = String.suffix t.text (String.length t.text - t.cursor_pos)
  ; cursor_pos = 0
  }
;;

let move_cursor_left t = { t with cursor_pos = Int.max 0 (t.cursor_pos - 1) }

let move_cursor_right t =
  { t with cursor_pos = Int.min (t.cursor_pos + 1) (String.length t.text) }
;;

let move_cursor_to_beginning t = { t with cursor_pos = 0 }
let move_cursor_to_end t = { t with cursor_pos = String.length t.text }

let create
    ?(text_attr = default_text_attr)
    ?(prompt = "> ")
    ?(prompt_attr = default_prompt_attr)
    ?(placeholder = "")
    ?(placeholder_attr = default_placeholder_attr)
    ?(focus = false)
    ?(echo_mode = `Normal)
    ()
  =
  { cursor_pos = 0
  ; text = ""
  ; text_attr
  ; placeholder
  ; placeholder_attr
  ; prompt
  ; prompt_attr
  ; focus
  ; echo_mode
  }
;;

let update t msg =
  if t.focus
  then (
    match msg with
    | `Key (`ASCII c, []) -> insert_char_before_cursor t c, Cmd.none
    | `Key (`Backspace, _) -> delete_char_before_cursor t, Cmd.none
    | `Key (`Delete, _) -> delete_char_under_cursor t, Cmd.none
    | `Key (`ASCII 'U', [ `Ctrl ]) -> delete_all_before_cursor t, Cmd.none
    | `Key (`Arrow `Left, []) -> move_cursor_left t, Cmd.none
    | `Key (`Arrow `Right, []) -> move_cursor_right t, Cmd.none
    | `Key (`Home, []) | `Key (`ASCII 'A', [ `Ctrl ]) ->
      move_cursor_to_beginning t, Cmd.none
    | `Key (`End, []) | `Key (`ASCII 'E', [ `Ctrl ]) -> move_cursor_to_end t, Cmd.none
    | _ -> t, Cmd.none)
  else t, Cmd.none
;;

let placeholder_view t =
  let len = String.length t.placeholder
  and cursor_attr =
    if t.focus then A.(st reverse ++ t.text_attr) else t.placeholder_attr
  in
  if len = 0
  then I.(string cursor_attr " ")
  else (
    let cursor = String.sub t.placeholder ~pos:0 ~len:1
    and suffix = String.suffix t.placeholder (len - 1) in
    I.(string cursor_attr cursor <|> string t.placeholder_attr suffix))
;;

let text_view t =
  let len = String.length t.text in
  let text =
    match t.echo_mode with
    | `Normal -> t.text
    | `Password -> String.make len echo_password_char
  in
  let cursor_attr = if t.focus then A.(st reverse ++ t.text_attr) else A.empty in
  let text_attr = if t.focus then t.text_attr else A.empty in
  if t.cursor_pos < len
  then (
    let prefix = String.prefix text t.cursor_pos
    and cursor = String.sub text ~pos:t.cursor_pos ~len:1
    and suffix = String.suffix text (len - t.cursor_pos - 1) in
    I.(string text_attr prefix <|> string cursor_attr cursor <|> string text_attr suffix))
  else I.(string text_attr text <|> string cursor_attr " ")
;;

let view t =
  let prompt_attr = if t.focus then t.prompt_attr else A.empty in
  let prompt_view = I.(string prompt_attr t.prompt) in
  if String.length t.text = 0
  then I.(prompt_view <|> placeholder_view t)
  else I.(prompt_view <|> text_view t)
;;
