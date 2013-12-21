open Batteries.Buffer
module Unix = Batteries.Unix

type level = 
  | WARNING 
  | ERROR
  | INFO
  | DEBUG

let logmsg = create 1000

let get_type = function
  | WARNING -> "WARNING"
  | ERROR -> "ERROR"
  | INFO -> "INFO"
  | DEBUG -> "DEBUG"


let get_day = function
  | 0 -> "Sun"
  | 1 -> "Mon"
  | 2 -> "Tue"
  | 3 -> "Wed"
  | 4 -> "Thu"
  | 5 -> "Fri"
  | 6 -> "Sat"
  | _ -> ""

let get_mon = function
  | 0 -> "Dec"
  | 1 -> "Jan"
  | 2 -> "Feb"
  | 3 -> "Mar"
  | 4 -> "Apr"
  | 5 -> "May"
  | 6 -> "Jun"
  | 7 -> "Jul"
  | 8 -> "Aug"
  | 9 -> "Sep"
  | 10 -> "Oct"
  | 11 -> "Nov"
  | _ -> ""

type ochannel = out_channel option

let ochan = ref None 

let get_time buf level = 
  let time = Unix.localtime (Unix.time ()) in
  let () = add_string buf "[" in
  let () = add_string buf (get_day time.Unix.tm_wday) in
  let () = add_string buf " " in
  let () = add_string buf (get_mon time.Unix.tm_mon) in
  let () = add_string buf " " in
  let () = add_string buf (string_of_int time.Unix.tm_hour) in
  let () = add_string buf ":" in
  let () = add_string buf (string_of_int time.Unix.tm_min) in
  let () = add_string buf ":" in
  let () = add_string buf (string_of_int time.Unix.tm_sec) in
  let () = add_string buf " " in
  let () = add_string buf (string_of_int (1900 + time.Unix.tm_year)) in
  let () = add_string buf "] [" in
  let () = add_string buf level in
  let () = add_string buf "] " in
  ()

let open_chan () =
  if !ochan = None then
    ochan := Some (open_out "data.log")


let flush_log () =
  match !ochan with
  | Some x -> 
    let () = Printf.fprintf x "%s" (contents logmsg) in
    let () = clear logmsg in
    flush x
  | None -> ()

let close_chan () =
  match !ochan with
  | Some x -> 
      let () = flush_log () in
      let () = close_out x in 
      ochan := None
  | None -> ()

let log ?(level=INFO) ?(pr=false) msg =
  let () =
    if pr then
      prerr_endline msg
  in
  match !ochan with
  | Some _ ->
    let () =
      if length logmsg > 5000 then
        flush_log ()
    in
    let () = get_time logmsg (get_type level) in
    let () = add_string logmsg msg in
    add_string logmsg "\n"
  | None -> ()

let print () = 
  print_endline (contents logmsg)


