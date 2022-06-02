open Base
open Core
open Async
open Tea
open Tea.Io
open Notty

type model =
  { select : Select.t
  ; chosen : bool
  }

let init () =
  let items = [ "Hamburgers"; "Pasta"; "Currywurst"; "Just Wine" ] in
  let select = Select.create ~items () in
  { select; chosen = false }, Cmd.none
;;

let update model msg =
  match msg with
  | `Key (`Escape, _) | `Key (`ASCII 'C', [ `Ctrl ]) -> model, Cmd.exit
  | `Key (`Enter, _) -> { model with chosen = true }, Cmd.exit
  | `Key _ ->
    let select, cmd = Select.update model.select msg in
    { model with select }, cmd
  | _ -> model, Cmd.none
;;

let view model =
  let greeting_view = I.(string A.empty "What do you want for dinner?") |> eol in
  let select_view = model.select |> Select.view |> eol in
  if not model.chosen
  then I.(greeting_view <-> select_view)
  else (
    let choice_view =
      I.(string A.empty (sprintf "%s? Sounds good!" (Select.selected model.select)))
    in
    I.(greeting_view <-> select_view <-> choice_view) |> eol)
;;

let run () = App.(run_unit { init; update; view })
let () = Command.async_spec ~summary:"select" Command.Spec.empty run |> Command_unix.run
