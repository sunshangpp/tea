open! Base
open! Core
open! Async
open Tea
open Io
open Notty
open Cohttp
open! Cohttp_async

type model =
  { choices : string array
  ; selected : bool array
  ; cursor : int
  ; quitting : bool
  ; statuses : int option array
  }

let uris =
  [ "http://www.google.com"; "https://www.facebook.com"; "https://www.example.com" ]
;;

let get_code i uris =
  let uri = uris.(i) in
  after (Time.Span.of_sec 1.)
  >>= fun () ->
  Client.get (Uri.of_string uri)
  >>| fun (resp, _) ->
  let code = resp |> Response.status |> Code.code_of_status in
  `UriStatus (i, code)
;;

let _get_code_randomly () =
  let uris = List.to_array uris in
  let len = Array.length uris in
  get_code (Random.int len) uris
;;

let init () =
  let choices = uris in
  ( { choices = choices |> List.to_array
    ; cursor = 0
    ; selected = Array.create ~len:(List.length choices) false
    ; quitting = false
    ; statuses = Array.create ~len:(List.length choices) None
    }
  , Cmd.none )
;;

let update model msg =
  let len = Array.length model.choices in
  match msg with
  | `Key (`Escape, _) | `Key (`ASCII 'q', _) -> { model with quitting = true }, Cmd.exit
  | `Key (`ASCII 'C', [ `Ctrl ]) -> { model with quitting = true }, Cmd.exit
  | `Key (`Enter, _) | `Key (`ASCII ' ', _) ->
    let selected = not model.selected.(model.cursor) in
    Array.set model.selected model.cursor selected;
    if selected
    then model, Cmd.call (fun () -> get_code model.cursor model.choices)
    else (
      Array.set model.statuses model.cursor None;
      model, Cmd.none)
  | `Key (`Arrow `Up, _) ->
    { model with cursor = (model.cursor + len - 1) % len }, Cmd.none
  | `Key (`Arrow `Down, _) -> { model with cursor = (model.cursor + 1) % len }, Cmd.none
  | `UriStatus (i, code) ->
    if model.selected.(i) then Array.set model.statuses i (Some code);
    model, Cmd.none
  | _ -> model, Cmd.none
;;

let view model =
  let status_report =
    if model.quitting
    then I.(string A.empty "quitting")
    else
      model.statuses
      |> Array.mapi ~f:(fun i v ->
             Option.map v ~f:(fun code ->
                 I.(string A.empty (Printf.sprintf "%s: %d" model.choices.(i) code))))
      |> Array.filter_map ~f:Fn.id
      |> Array.to_list
      |> I.vcat
  in
  let choices_img =
    model.choices
    |> Array.mapi ~f:(fun i str ->
           let cursor = if i = model.cursor then "> " else "  "
           and checked = if model.selected.(i) then "[x] " else "[ ] "
           and highlight =
             if model.selected.(i) then A.(fg (rgb_888 ~r:255 ~g:95 ~b:175)) else A.empty
           in
           I.(
             string highlight cursor <|> string highlight checked <|> string highlight str))
    |> Array.to_list
    |> I.vcat
    |> eol
  in
  I.(status_report <-> choices_img)
;;

let run () = App.(run_unit { init; update; view })
let () = Command.async_spec ~summary:"checkbox" Command.Spec.empty run |> Command_unix.run
