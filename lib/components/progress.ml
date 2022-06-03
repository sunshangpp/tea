open! Base
open! Core
open! Async
open Notty

let empty_cell = "░"
let filled_cell = "█"

type t =
  { width : int
  ; img_attr : A.t
  ; percent : int
  }

let create ?(width = 80) ?(img_attr = A.empty) () = { width; img_attr; percent = 0 }
let calibrated_percent v = if v > 100 then 100 else if v < 0 then 0 else v
let incr_percent_by t delta = { t with percent = calibrated_percent (t.percent + delta) }

let view t =
  let percent = calibrated_percent t.percent in
  let percent_view = I.(string A.empty (Printf.sprintf " %3.0d%%" percent)) in
  let bar_width = max (t.width - I.width percent_view) 0 in
  let filled_width =
    Int.of_float Float.(round_up (of_int bar_width * (of_int percent /. 100.)))
  in
  let empty_width = bar_width - filled_width in
  let filled_bar =
    List.range 0 filled_width
    |> List.map ~f:(fun _ -> I.(string t.img_attr filled_cell))
    |> I.hcat
  and empty_bar =
    List.range 0 empty_width
    |> List.map ~f:(fun _ -> I.(string A.empty empty_cell))
    |> I.hcat
  in
  I.(filled_bar <|> empty_bar <|> percent_view)
;;
