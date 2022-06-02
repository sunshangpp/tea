open Base
open Core
open Async
open Tea
open Tea.Io
open Notty

let styles = [| `Line; `Dot; `MiniDot; `Jump; `Pulse; `Points; `Globe; `Moon; `Monkey |]

type model =
  { idx : int
  ; spinner : Spinner.t
  }

let init_spinner idx =
  let style = styles.(idx) in
  let spinner = Spinner.create ~style ~img_attr:A.(fg lightcyan) () in
  { idx; spinner }, Spinner.tick spinner
;;

let init () = init_spinner 0

let update model msg =
  match msg with
  | `Key (`Escape, _) | `Key (`ASCII 'q', _) | `Key (`ASCII 'C', [ `Ctrl ]) ->
    model, Cmd.exit
  | `Key (`Arrow `Left, _) ->
    let idx = (model.idx - 1) % Array.length styles in
    init_spinner idx
  | `Key (`Arrow `Right, _) ->
    let idx = (model.idx + 1) % Array.length styles in
    init_spinner idx
  | _ ->
    let spinner, cmd = Spinner.update model.spinner msg in
    { model with spinner }, cmd
;;

let view model = model.spinner |> Spinner.view |> eol
let run () = App.(run_unit ~mode:`Fullscreen { init; update; view })
let () = Command.async_spec ~summary:"spinner" Command.Spec.empty run |> Command_unix.run
