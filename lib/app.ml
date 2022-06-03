open! Base
open Io
open Async
open Notty
open Notty_async

type ('args, 'model, 'msg) config =
  { init : 'args -> 'model * 'msg Cmd.t
  ; update : 'model -> 'msg -> 'model * 'msg Cmd.t
  ; view : 'model -> image
  }

let rec handle_cmd w = function
  | Cmd.None -> ()
  | Cmd.Batch cmds -> List.iter cmds ~f:(handle_cmd w)
  | Cmd.Call _ when Pipe.is_closed w -> ()
  | Cmd.Call thunk ->
    thunk ()
    >>> fun msg -> if not (Pipe.is_closed w) then Pipe.write_without_pushback w msg
;;

(* let run_inline (config : ('args, 'model, 'msg) config) = let keypress_reader =
   keypress_reader_inline () in let msg_reader, msg_writer = Pipe.create () in let
   event_reader = Pipe.interleave [ msg_reader; keypress_reader ] in let init_model,
   init_cmd = config.init () in let rec loop model cmd = handle_cmd msg_writer cmd; let
   img = config.view model in let%bind () = output_image img in let%bind event = Pipe.read
   event_reader in match event with | `Eof -> return model | `Ok msg -> (match msg with |
   `Exit -> Pipe.close_read event_reader; return model | msg -> let new_model, new_cmd =
   config.update model msg in let%bind () = clear img in loop new_model new_cmd) in
   let%bind () = show_cursor false in let%bind model = loop init_model init_cmd in let%map
   () = show_cursor true in model ;;

   let run_fullscreen (config : ('args, 'model, 'msg) config) = let%bind term =
   Term.create () in let keypress_reader = keypress_reader_term term in let msg_reader,
   msg_writer = Pipe.create () in let event_reader = Pipe.interleave [ msg_reader;
   keypress_reader ] in let init_model, init_cmd = config.init () in let rec loop model
   cmd = handle_cmd msg_writer cmd; let img = config.view model in let%bind () =
   Term.image term img in let%bind event = Pipe.read event_reader in match event with |
   `Eof -> return model | `Ok msg -> (match msg with | `Exit -> Pipe.close_read
   event_reader; return model | msg -> let new_model, new_cmd = config.update model msg in
   loop new_model new_cmd) in loop init_model init_cmd ;; *)

let run ?(mode = `Inline) (config : ('args, 'model, 'msg) config) =
  let init_model, init_cmd = config.init () in
  let init_img = config.view init_model in
  let msg_reader, msg_writer = Pipe.create () in
  let rec loop model cmd img event_reader render_image clear_image =
    handle_cmd msg_writer cmd;
    let%bind event = Pipe.read event_reader in
    match event with
    | `Eof -> return model
    | `Ok msg ->
      (match msg with
      | `Exit ->
        Pipe.close_read event_reader;
        return model
      | _ ->
        let new_model, new_cmd = config.update model msg in
        let new_img = config.view new_model in
        let%bind () =
          if not (I.equal img new_img)
          then clear_image img >>= fun () -> render_image new_img
          else return ()
        in
        loop new_model new_cmd new_img event_reader render_image clear_image)
  in
  match mode with
  | `Inline ->
    let keypress_reader = keypress_reader_inline () in
    let event_reader = Pipe.interleave [ msg_reader; keypress_reader ] in
    let render_image img = output_image img in
    let clear_image img = clear img in
    let%bind () = show_cursor false in
    let%bind () = render_image init_img in
    let%bind model =
      loop init_model init_cmd init_img event_reader render_image clear_image
    in
    let%map () = show_cursor true in
    model
  | `Fullscreen ->
    let%bind term = Term.create () in
    let keypress_reader = keypress_reader_term term in
    let event_reader = Pipe.interleave [ msg_reader; keypress_reader ] in
    let render_image img = Term.image term img in
    let clear_image _ = return () in
    let%bind () = render_image init_img in
    loop init_model init_cmd init_img event_reader render_image clear_image
;;

let run_unit ?(mode = `Inline) (config : ('args, 'model, 'msg) config) =
  Deferred.ignore_m (run ~mode config)
;;
