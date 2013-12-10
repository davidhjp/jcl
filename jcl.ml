open Sexplib
open Std
open Sexp
open Javalib_pack
open Javalib
open JBasics
module Hashtbl = Batteries.Hashtbl
module String = Batteries.String
module List = Batteries.List
module Buffer = Batteries.Buffer

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

type json_type = {
  mutable refsize : int;
  mutable ousize : int;
  mutable joohsize : int;
  mutable foh : int;

  mutable byte_size : int;
  mutable short_size : int;
  mutable int_size : int;
  mutable long_size : int;
  mutable float_size : int;
  mutable double_size : int;
  mutable boolean_size : int;
  mutable char_size : int;
  mutable ref_size : int;
  mutable arrayref_size : int
}
with sexp

let print_ds myds =
  let () = Hashtbl.iter (fun k v -> 
      print_endline k;
      output_hum stdout (sexp_of_class_data v); print_endline ""
    ) myds in
  ()


let rec get_ds classorinter myds jvm =
  let classorinter_string = (cn_name (get_name classorinter)) in
  let cd = Hashtbl.find_option myds classorinter_string in
  match cd with
  | None -> 
    let size_table = 
      {_int=0;_bool=0;_byte=0;_char=0;_double=0;_float=0;_long=0;_short=0;_ref=0;_arrayref=0} in
    let () = Hashtbl.add myds classorinter_string size_table in
    let () = f_iter (fun x -> 
        match fs_type (get_field_signature x) with
        | TBasic x ->
          (match x with
           | `Int -> size_table._int <-  size_table._int + jvm.int_size
           | `Bool -> size_table._bool <-  size_table._bool + jvm.boolean_size
           | `Byte -> size_table._byte <-  size_table._byte + jvm.byte_size
           | `Char -> size_table._char <-  size_table._char + jvm.char_size
           | `Double -> size_table._double <-  size_table._double + jvm.double_size
           | `Float -> size_table._float <-  size_table._float + jvm.float_size
           | `Long -> size_table._long <-  size_table._long + jvm.long_size
           | `Short -> size_table._short <-  size_table._short + jvm.short_size
          )
        | TObject x ->
          (match x with
           | TClass x ->
             size_table._ref <- size_table._ref + jvm.ref_size
           | TArray x ->
             size_table._arrayref <- size_table._arrayref + jvm.arrayref_size
          )
      ) classorinter in
    ()
  | Some x -> print_endline ("INFO: class "^(cn_name (get_name classorinter))^" already parsed")

let parse_jvm jvm_spc jvm =
  let ic = open_in jvm_spc in
  let pormat = Str.regexp ".+[ ]*:[ ]*[0-9]+[ ]*$" in 
  let () = 
    try
      while true do 
        let line = input_line ic in
        let s = Str.string_match pormat line 0 in
        if s = false then
          failwith (jvm_spc^": invalid format - "^line)
        else
          let slist = Str.split (Str.regexp "[: ]+") line in
          let () = 
            match slist with
            | [a;b] -> 
              (match String.lowercase a with
               | "referencesize" -> jvm.refsize <- int_of_string b
               | "oneunitsize" -> jvm.ousize <- int_of_string b
               | "jvmobjectoverheadsize" -> jvm.joohsize <- int_of_string b
               | "frameoverhead" -> jvm.foh <- int_of_string b
               | "byte_size" -> jvm.byte_size <- int_of_string b
               | "short_size" -> jvm.short_size <- int_of_string b
               | "int_size" -> jvm.int_size <- int_of_string b
               | "long_size" -> jvm.long_size <- int_of_string b
               | "float_size" -> jvm.float_size <- int_of_string b
               | "double_size" -> jvm.double_size <- int_of_string b
               | "boolean_size" -> jvm.boolean_size <- int_of_string b
               | "char_size" -> jvm.char_size <- int_of_string b
               | "ref_size" -> jvm.ref_size <- int_of_string b
               | "arrayref_size" -> jvm.arrayref_size <- int_of_string b
               | _ -> failwith (jvm_spc^": invalid format - "^line)
              )
            | _ -> failwith (jvm_spc^": invalid format - "^line)
          in
          ()
      done
    with
    | End_of_file -> close_in ic
  in
  ()

let make_json jvm myds =
  let ofile = "data.json" in
  let oc = open_out ofile in
  let b = Buffer.create 100 in
  let add str = Buffer.add_string b str in
  let newline () = Buffer.add_string b "\n" in
  let () = add "{\n" in
  let () = 
    let () = add "PrimordialTypeSizes : [{}]\n" in
    let () = add "ReferenceSize : " in 
    let () = add (string_of_int jvm.refsize) in
    let () = newline () in
    let () = add "OneUnitSize : " in
    let () = add (string_of_int jvm.ousize) in
    let () = newline () in
    let () = add "JvmObjectOverheadSize : " in
    let () = add (string_of_int jvm.joohsize) in
    let () = newline () in
    let () = add "FrameOverhead : " in
    let () = add (string_of_int jvm.foh) in
    newline ()
  in
  let () = add "}\n" in
  let () = Printf.fprintf oc "%s" (Buffer.contents b) in
  let () = flush stdout in
  close_out oc

let () =
  let usage_msg = 
    "Usage: jcl <filename>"
  in
  let jvm_spec = ref "" in
  let speclist = Arg.align [
      ("-jvm", Arg.Set_string jvm_spec, "<file> "^
                                        "     JVM configuration file");
    ] in
  let flist = ref [] in
  let () = Arg.parse speclist (fun x -> flist := x :: !flist ) usage_msg in
  let () = if List.is_empty !flist then
      let () = Arg.usage speclist usage_msg in
      exit 1 in

  let jvm = {refsize=4;ousize=4;joohsize=4;foh=4;
             byte_size=1;short_size=2;int_size=4;long_size=8;float_size=4;
             double_size=8;boolean_size=1;char_size=2;
             ref_size=4; arrayref_size=4
            } in
  let () = 
    if !jvm_spec <> "" then
      let () = parse_jvm !jvm_spec jvm in
      ()
  in

  let myds = Hashtbl.create 300 in
  let () = List.iter (fun fn -> 
      iter ~debug:false (fun x -> get_ds x myds jvm) fn) !flist in 
  let () = print_ds myds in

  let () = make_json jvm myds in
  ()
