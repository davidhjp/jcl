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
  mutable _arrayref : int;
} 
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
  others : (string, int) Std.Hashtbl.t
}
with sexp

let print_ds myds =
  let ofile = "data.out" in
  let oc = open_out ofile in
  let () = Hashtbl.iter (fun k v -> 
      Printf.fprintf oc "%s\n(_int:%d _bool:%d _byte:%d _char:%d _double:%d _float:%d\n_long:%d _short:%d _ref:%d _arrayref:%d)\n" 
          k v._int v._bool v._byte v._char v._double v._float v._long v._short v._ref v._arrayref
    ) myds in
  let () = flush stdout in
  close_out oc

let rec get_value = function
  | TBasic x ->
    (match x with
     | `Int -> "I"
     | `Bool -> "Z"
     | `Byte -> "B"
     | `Char -> "C"
     | `Double -> "D"
     | `Float -> "F"
     | `Long -> "J"
     | `Short -> "S"
    )
  | TObject x ->
    get_array_string x
and get_array_string = function
  | TArray x -> "["^(get_value x)
  | TClass x -> "L"^(cn_name x)

let rec get_all_fields cp l = function
  | JClass x -> 
    let l = ref l in
    (match x.c_super_class with
     | Some clazz -> 
       (try
          let super_class = get_class cp clazz in
          let () = f_iter (fun x -> l := x :: !l ) super_class in
(*           let () = print_endline ("super : "^(cn_name (get_name super_class))) in *)
          get_all_fields cp !l super_class
        with
        | No_class_found x -> raise (No_class_found ("Unable to find "^x^" in classpath check -cp option?")))
     | None -> !l
    )
  | _ -> failwith ("This must be alwasy JClass")

let rec get_ds clazz myds jvm used_arrays cp =
  let clazz_string = (cn_name (get_name clazz)) in
  let cd = Hashtbl.find_option myds clazz_string in
  match cd with
  | None -> 
    let size_table = 
      {_int=0;_bool=0;_byte=0;_char=0;_double=0;_float=0;_long=0;_short=0;_ref=0;_arrayref=0;} in
    let () = Hashtbl.add myds clazz_string size_table in
(*     print_endline ("**** getting super of "^clazz_string); *)
    let all_fields = get_all_fields cp [] clazz in
    let all_fields = f_fold (fun x all -> x :: all ) clazz all_fields in
(*
    print_endline ("all fields : ");
    let () = List.iter (fun x -> 
        print_endline (fs_name (get_field_signature x ))
      ) all_fields in
*)
    let () = List.iter (fun x -> 
      if not(is_static_field x) then
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
             (* May be need to exclude static type? *)
             size_table._ref <- size_table._ref + jvm.ref_size
           | (TArray x) as arr ->
             let array_string = ("\""^ get_array_string arr ^"\"")in
             let num = (match Hashtbl.find_option jvm.others array_string with
                 | Some x -> x
                 | None ->
                   let () = 
                     if (function | None -> true | _ -> false ) (Hashtbl.find_option used_arrays array_string) then
                       let () = prerr_endline ("WARNING: Missing type '"^array_string^"' in the jvm file - "^
                                               "automatically setting its size to 1") in
                       Hashtbl.add used_arrays array_string 1 in
                   1
               ) in 
             size_table._arrayref <- size_table._arrayref + num
          )
          (* Need to deal with static fields later *)
      ) all_fields in
    ()
  | Some x -> print_endline ("INFO: class "^(cn_name (get_name clazz))^" already parsed")

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
               (*                | "arrayref_size" -> jvm.arrayref_size <- int_of_string b *)
               | _ -> 
                 (match Hashtbl.find_option jvm.others a  with
                  | None -> Hashtbl.add jvm.others a (int_of_string b)
                  | Some num -> failwith ("Duplicated entries : "^a)
                 )
              )
            | _ -> failwith (jvm_spc^": invalid format - "^line)
          in
          ()
      done
    with
    | End_of_file -> close_in ic
  in
  ()

let make_json jvm myds used_arrays =
  let ofile = "data.json" in
  let oc = open_out ofile in
  let b = Buffer.create 100 in
  let add str = Buffer.add_string b str in
  let newline () = Buffer.add_string b "\n" in
  let acc_data cdata = 
    cdata._int + cdata._bool + cdata._byte + cdata._char + cdata._double +
    cdata._float + cdata._long + cdata._short + cdata._ref + cdata._arrayref
  in
  let () = add "{\n" in
  let () = 
    let () = add "PrimordialTypeSizes : [{}],\n" in
    let () = add "ReferenceSize : " in 
    let () = add (string_of_int jvm.refsize) in
    let () = add "," in 
    let () = newline () in
    let () = add "OneUnitSize : " in
    let () = add (string_of_int jvm.ousize) in
    let () = add "," in 
    let () = newline () in
    let () = add "JvmObjectOverheadSize : " in
    let () = add (string_of_int jvm.joohsize) in
    let () = add "," in 
    let () = newline () in
    let () = add "FrameOverhead : " in
    let () = add (string_of_int jvm.foh) in
    let () = add "," in 
    let () = newline () in
    let () = add "ApplicationTypeSizes : [{\n" in
    let () = Buffer.add_buffer b 
        (Hashtbl.fold 
           (fun name size buf -> 
              let () = Buffer.add_string buf name in
              let () = Buffer.add_string buf ": " in
              let () = Buffer.add_string buf (string_of_int size) in
              let () = Buffer.add_string buf ",\n" in
              buf
           ) used_arrays (Buffer.create 500)) in
    let () = Buffer.add_buffer b 
        (Hashtbl.fold 
           (fun name size buf -> 
              if Buffer.length buf <> 0 then
                Buffer.add_string buf ",\n";
              let () = Buffer.add_string buf name in
              let () = Buffer.add_string buf ": " in
              let () = Buffer.add_string buf (string_of_int (acc_data size)) in
              buf
           ) myds (Buffer.create 500)) in
    let () = add "}]\n" in
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
  let cp = ref "" in
  let speclist = Arg.align [
      ("-jvm", Arg.Set_string jvm_spec, "<file> "^
                                        "     JVM configuration file");
      ("-cp", Arg.Set_string cp, "<classpath> "^
                                        "     Setting classpath");
    ] in
  let flist = ref [] in
  let () = Arg.parse speclist (fun x -> flist := x :: !flist ) usage_msg in
  let () = if List.is_empty !flist then
      let () = Arg.usage speclist usage_msg in
      exit 1 in

  let jvm = {refsize=4;ousize=4;joohsize=4;foh=4;
             byte_size=1;short_size=2;int_size=4;long_size=8;float_size=4;
             double_size=8;boolean_size=1;char_size=2;
             ref_size=4; others=(Hashtbl.create 10)
            } in
  let () = 
    if !jvm_spec <> "" then
      let () = parse_jvm !jvm_spec jvm in
      ()
  in

  let myds = Hashtbl.create 300 in
  let used_arrays = Hashtbl.create 10 in 
  let clazz_path = class_path !cp in
  let () = List.iter (fun fn -> 
      iter ~debug:false (fun x -> (function | JClass _ -> get_ds x myds jvm used_arrays clazz_path
                                            | JInterface _ -> ()) x
        ) fn) !flist in 
  let () = close_class_path clazz_path in
  let () = print_ds myds in
  let () = make_json jvm myds used_arrays in
  ()
