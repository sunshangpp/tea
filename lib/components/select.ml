open! Base
open! Core
open Notty

let default_highlight_attr = A.(fg (rgb_888 ~r:255 ~g:95 ~b:175))

type t =
  { items : string array
  ; cursor : int
  ; highlight_attr : A.t
  }

let create ~items ?(highlight_attr = default_highlight_attr) () =
  if List.length items = 0
  then invalid_arg "items must not be empty"
  else { items = List.to_array items; cursor = 0; highlight_attr }
;;

let selected t = t.items.(t.cursor)

let cursor_up t =
  let len = Array.length t.items in
  { t with cursor = (t.cursor + len - 1) % len }
;;

let cursor_down t =
  let len = Array.length t.items in
  { t with cursor = (t.cursor + len + 1) % len }
;;

let update t = function
  | `Key (`Arrow `Up, _) -> cursor_up t, Cmd.none
  | `Key (`Arrow `Down, _) -> cursor_down t, Cmd.none
  | _ -> t, Cmd.none
;;

let view t =
  t.items
  |> Array.mapi ~f:(fun i item ->
         let cursor = if i = t.cursor then "> " else "  " in
         let attr = if i = t.cursor then t.highlight_attr else A.empty in
         let idxed_item = Int.to_string (i + 1) ^ ". " ^ item in
         I.(string attr cursor <|> string attr idxed_item))
  |> Array.to_list
  |> I.vcat
;;
