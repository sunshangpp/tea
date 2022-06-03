open! Core
open! Async
open! Tea
open Tea.Io
open Notty
open Progress

type model = { progress : Progress.t }

let tick_after span = after span >>| fun () -> `Tick
let tick () = tick_after (Time.Span.of_sec 0.01)

let init () =
  { progress = Progress.create ~width:80 ~img_attr:A.(fg lightcyan) () }, Cmd.call tick
;;

let update model msg =
  match msg with
  | `Key (`Escape, _) | `Key (`ASCII 'q', _) | `Key (`ASCII 'C', [ `Ctrl ]) ->
    model, Cmd.exit
  | `Tick ->
    if model.progress.percent < 100
    then { progress = incr_percent_by model.progress 1 }, Cmd.call tick
    else model, Cmd.exit
  | _ -> model, Cmd.none
;;

let view model = model.progress |> Progress.view |> eol
let run () = App.(run_unit { init; update; view })
let () = Command.async_spec ~summary:"progress" Command.Spec.empty run |> Command_unix.run
