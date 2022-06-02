open! Base
open! Async
open Notty
open Notty_async

let bsize = 1024

(* copied from notty_async *)
let input_pipe ~nosig reader =
  let (`Revert revert) =
    let fd = Unix.Fd.file_descr_exn (Reader.fd reader) in
    Notty_unix.Private.setup_tcattr ~nosig fd
  in
  let flt = Notty.Unescape.create () in
  let ibuf = Bytes.create bsize in
  let r, w = Pipe.create () in
  let rec loop () =
    match Unescape.next flt with
    | #Unescape.event as r ->
      (* As long as there are events to read without blocking, dump them all into the
         pipe. *)
      if Pipe.is_closed w
      then return ()
      else (
        Pipe.write_without_pushback w r;
        loop ())
    | `End -> return ()
    | `Await ->
      (* Don't bother issuing a new read until the pipe has space to write *)
      let%bind () = Pipe.pushback w in
      (match%bind Reader.read reader ibuf with
      | `Eof -> return ()
      | `Ok n ->
        Unescape.input flt ibuf 0 n;
        loop ())
  in
  (* Some error handling to make sure that we call revert if the pipe fails *)
  let monitor = Monitor.create ~here:[%here] ~name:"Notty input pipe" () in
  don't_wait_for (Deferred.ignore_m (Monitor.get_next_error monitor) >>| revert);
  don't_wait_for (Scheduler.within' ~monitor loop);
  don't_wait_for (Pipe.closed r >>| revert);
  r
;;

(* convert event reader to keypress reader, filter out non keypress events such as
   resize/mouse *)
let keypress_reader events =
  let r, w = Pipe.create () in
  let rec loop () =
    let%bind event = Pipe.read events in
    match event with
    | `Eof -> return ()
    | `Ok msg ->
      (match msg with
      | `Key _ as k ->
        if Pipe.is_closed w
        then return ()
        else (
          Pipe.write_without_pushback w k;
          loop ())
      | _ -> loop ())
  in
  don't_wait_for (loop ());
  don't_wait_for (Pipe.closed r >>| fun () -> Pipe.close_read events);
  r
;;

let keypress_reader_inline () =
  let events = input_pipe ~nosig:true (force Reader.stdin) in
  keypress_reader events
;;

let keypress_reader_term term =
  let events = Term.events term in
  keypress_reader events
;;

include Notty_unix.Private.Gen_output (struct
  type fd = Writer.t lazy_t
  and k = unit Deferred.t

  let def = Writer.stdout

  let to_fd w =
    match Fd.with_file_descr (Writer.fd (force w)) Fn.id with
    | `Already_closed | `Error _ -> raise_s [%message "Couldn't obtain FD"]
    | `Ok x -> x
  ;;

  let write (lazy w) buf =
    let bytes = Buffer.contents_bytes buf in
    Writer.write_bytes w bytes ~pos:0 ~len:(Bytes.length bytes);
    Writer.flushed w
  ;;
end)

let clear img =
  let h, w = I.height img, I.width img in
  if h = 1
  then move_cursor (`By (-w, -h + 1))
  else
    move_cursor (`By (-w, -h + 1))
    >>= fun () -> output_image I.(void w h) >>= fun () -> move_cursor (`By (-w, -h + 1))
;;
