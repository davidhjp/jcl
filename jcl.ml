open Sexplib
open Std
open Sexp
open Javalib_pack
open Javalib
open JBasics
module Hashtbl = Batteries.Hashtbl

type class_data = {
  mutable _int : int;
  mutable _bool : int;
  mutable _byte : int;
  mutable _char : int;
  mutable _double : int;
  mutable _float : int;
  mutable _long : int;
  mutable _short : int;
  mutable _ref : int;
  mutable _arrayref : int} 
with sexp

let print_ds myds =
  let () = Hashtbl.iter (fun k v -> 
      print_endline k;
      output_hum stdout (sexp_of_class_data v); print_endline ""
    ) myds in
  ()


let rec get_ds fn cp myds =
  let classpath = class_path cp in
  let classname = make_cn fn in
  let classorinter = get_class classpath classname in
  let classorinter_string = (cn_name (get_name classorinter)) in
  let cd = Hashtbl.find_option myds classorinter_string in
  match cd with
  | None -> 
    let size_table = 
      {_int=0;_bool=0;_byte=0;_char=0;_double=0;_float=0;_long=0;_short=0;_ref=0;_arrayref=0} in
    let () = Hashtbl.add myds classorinter_string size_table in
    let () = cf_iter (fun x -> 
        match fs_type (x.cf_signature) with
        | TBasic x ->
          (match x with
           | `Int -> size_table._int <- succ size_table._int
           | `Bool -> size_table._bool <- succ size_table._bool
           | `Byte -> size_table._byte <- succ size_table._byte
           | `Char -> size_table._char <- succ size_table._char
           | `Double -> size_table._double <- succ size_table._double
           | `Float -> size_table._float <- succ size_table._float
           | `Long -> size_table._long <- succ size_table._long
           | `Short -> size_table._short <- succ size_table._short
          )
        | TObject x ->
          (match x with
           | TClass x ->
             let () = get_ds (cn_name x) cp myds in
             size_table._ref <- succ size_table._ref
           | TArray x ->
             size_table._arrayref <- succ size_table._arrayref
          )
      ) classorinter in
    ()
  | Some x -> print_endline ("INFO: class "^(cn_name (get_name classorinter))^" already parsed")

let () =
  let usage_msg = "Usage: jcl [OPTION] <full_class_name>" in
  let cp = ref "." in
  let cname = ref "" in
  let speclist = Arg.align [
      ("-cp", Arg.Set_string cp, "<file> Setting Java classpath") ] in
  let () = Arg.parse speclist (fun x -> cname := x) usage_msg in
  let () = if !cname = "" then
      let () = Arg.usage speclist usage_msg in
      exit 1 in
  let myds = Hashtbl.create 300 in
  try
    (*     print_endline !cp; *)
    let () = get_ds !cname !cp myds in
    let () = print_ds myds in
    ()
  with
  | JBasics.No_class_found x -> print_endline ("No class found : "^x)
