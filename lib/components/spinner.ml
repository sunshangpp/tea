open! Base
open! Core
open Async
open Notty

let last_id = ref 0

type style =
  { frames : string array
  ; fps : Time.Span.t
  }

type t =
  { id : int
  ; style : style
  ; img_attr : A.t
  ; frame : int
  }

let line = { frames = [| "|"; "/"; "-"; "\\" |]; fps = Time.Span.of_sec 0.1 }

let dot =
  { frames = [| "⣾ "; "⣽ "; "⣻ "; "⢿ "; "⡿ "; "⣟ "; "⣯ "; "⣷ " |]
  ; fps = Time.Span.of_sec 0.1
  }
;;

let minidot =
  { frames = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]
  ; fps = Time.Span.of_sec (1. /. 12.)
  }
;;

let jump =
  { frames = [| "⢄"; "⢂"; "⢁"; "⡁"; "⡈"; "⡐"; "⡠" |]; fps = Time.Span.of_sec 0.1 }
;;

let pulse = { frames = [| "█"; "▓"; "▒"; "░" |]; fps = Time.Span.of_sec 0.125 }

let points =
  { frames = [| "∙∙∙"; "●∙∙"; "∙●∙"; "∙∙●" |]; fps = Time.Span.of_sec (1. /. 7.) }
;;

let globe = { frames = [| "🌍"; "🌎"; "🌏" |]; fps = Time.Span.of_sec 0.25 }

let moon =
  { frames = [| "🌑"; "🌒"; "🌓"; "🌔"; "🌕"; "🌖"; "🌗"; "🌘" |]; fps = Time.Span.of_sec 0.125 }
;;

let monkey = { frames = [| "🙈"; "🙉"; "🙊" |]; fps = Time.Span.of_sec (1. /. 3.) }
let tick t = Cmd.call (fun () -> after t.style.fps >>| fun () -> `Spinner_tick t.id)

let create ?(style = `Dot) ?(img_attr = A.empty) () =
  let id = !last_id + 1
  and style =
    match style with
    | `Line -> line
    | `Dot -> dot
    | `MiniDot -> minidot
    | `Jump -> jump
    | `Pulse -> pulse
    | `Points -> points
    | `Globe -> globe
    | `Moon -> moon
    | `Monkey -> monkey
  and frame = 0 in
  last_id := id;
  { id; style; img_attr; frame }
;;

let update t = function
  | `Spinner_tick id when id = t.id ->
    let frame = (t.frame + 1) % Array.length t.style.frames in
    { t with frame }, tick t
  | _ -> t, Cmd.none
;;

let view t = I.(string t.img_attr t.style.frames.(t.frame))
