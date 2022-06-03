open! Core
open! Async
open! Tea
open Tea.Io
open Notty

type model =
  { username : Textinput.t (* ; focused_on_username: bool *)
  ; password : Textinput.t (* ; focused_on_username: bool *)
  }

let inquire_username model = { model with username = Textinput.focus model.username }

let inquire_password model =
  { username = Textinput.unfocus model.username
  ; password = Textinput.focus model.password
  }
;;

let inquire_completed model =
  { username = Textinput.unfocus model.username
  ; password = Textinput.unfocus model.password
  }
;;

let init () =
  let username = Textinput.create ~placeholder:"Username" () in
  let password = Textinput.create ~placeholder:"Password" ~echo_mode:`Password () in
  inquire_username { username; password }, Cmd.none
;;

let update model msg =
  match msg with
  | `Key (`Escape, _) | `Key (`ASCII 'C', [ `Ctrl ]) -> model, Cmd.exit
  | `Key (`Enter, _) ->
    let model =
      if Textinput.focused model.username
      then inquire_password model
      else inquire_completed model
    in
    model, Cmd.none
  | `Key _ as key_msg when Textinput.focused model.username ->
    let username, cmd = Textinput.update model.username key_msg in
    { model with username }, cmd
  | `Key _ as key_msg when Textinput.focused model.password ->
    let password, cmd = Textinput.update model.password key_msg in
    { model with password }, cmd
  | _ -> model, Cmd.none
;;

let view model =
  let username_view = model.username |> Textinput.view in
  let password_view = model.password |> Textinput.view in
  let v = I.(username_view <-> password_view) in
  if not (Textinput.focused model.password || Textinput.focused model.username)
  then I.(v <-> string A.empty "we got your credential") |> eol
  else v |> eol
;;

let run () =
  let%map model = App.(run ~mode:`Inline { init; update; view }) in
  print_endline ("your password is: " ^ Textinput.content model.password)
;;

let () =
  Command.async_spec ~summary:"textinput" Command.Spec.empty run |> Command_unix.run
;;
