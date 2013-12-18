open Sexplib
open Std
open Sexp
open Sawja_pack
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

type jtype = {
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
  mutable objheader_size : int;
  mutable arrayheader_size : int;
  mutable align_size : int;
  others : (string, int) Std.Hashtbl.t
}
with sexp

let rec get_value ?(type_only=false) = function
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
    get_array_string ~type_only x
and get_array_string ?(type_only=false) = function
  | TArray x -> if type_only then (get_value ~type_only x) else "["^(get_value ~type_only x)
  | TClass x -> if type_only then (cn_name x) else "L"^(cn_name x)

let rec get_array_type_size jvm = function
  | TBasic x ->
    (match x with
     | `Int ->    jvm.int_size
     | `Bool ->   jvm.long_size
     | `Byte ->   jvm.byte_size
     | `Char ->   jvm.char_size
     | `Double -> jvm.double_size
     | `Float ->  jvm.float_size
     | `Long ->   jvm.long_size
     | `Short ->  jvm.short_size
    )
  | TObject x ->
    get_array_type_size_rec jvm x
and get_array_type_size_rec jvm = function
  | TArray x -> get_array_type_size jvm x
  | TClass x -> jvm.ref_size


let print_ds myds =
  let ofile = "data.out" in
  let oc = open_out ofile in
  let () = Hashtbl.iter (fun k v -> 
      Printf.fprintf oc "%s\n(_int:%d _bool:%d _byte:%d _char:%d _double:%d _float:%d\n_long:%d _short:%d _ref:%d _arrayref:%d)\n" 
        k v._int v._bool v._byte v._char v._double v._float v._long v._short v._ref v._arrayref
    ) myds in
  let () = flush stdout in
  close_out oc


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

let rec get_ds ?(entry_point=false) clazz myds jvm cp used_arrays unrsv_arrays  =
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
               if not entry_point then(
                 let array_string = get_array_string arr  in
                 match 
                   (Hashtbl.find_option unrsv_arrays array_string,Hashtbl.find_option used_arrays array_string)
                 with
                 | (None,None) ->
                   (match(Hashtbl.find_option jvm.others array_string) with
                    | None ->
                      let () = prerr_endline ("WARNING: Missing type '"^array_string^"' in the jvm file - "^
                                              "automatically setting its size to 1") in
                      Hashtbl.add unrsv_arrays array_string 1
                    | Some x ->
                      Hashtbl.add used_arrays array_string (Int32.of_int x)
                   )
                 | _ -> ()
               );
               size_table._arrayref <- size_table._arrayref + jvm.ref_size
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
               | "objheader_size" -> jvm.objheader_size <- int_of_string b
               | "arrayheader_size" -> jvm.arrayheader_size <- int_of_string b
               | "align_size" -> jvm.align_size <- int_of_string b
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

let get_data_sizes tot align_size =
  let modv = tot mod align_size in
  if modv = 0 then
    tot
  else
    align_size - modv + tot

let make_json jvm myds used_arrays unrsv_arrays nopack =
  let ofile = "data.json" in
  let oc = open_out ofile in
  let b = Buffer.create 100 in
  let add str = Buffer.add_string b str in
  let newline () = Buffer.add_string b "\n" in
  (* Function for accumulating data type sizes *)
  let acc_data cdata jvm nopack =
    let tot = cdata._int + cdata._bool + cdata._byte + cdata._char + cdata._double +
              cdata._float + cdata._long + cdata._short + cdata._ref + cdata._arrayref +
              jvm.objheader_size in
    (* Padding matters *)
    match nopack with
    | false ->
      get_data_sizes tot jvm.align_size
    | true -> 
      tot
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
    let () = add "_comment1 : \"Resolved Array Types : \",\n" in
    let () = Buffer.add_buffer b 
        (Hashtbl.fold 
           (fun name size buf -> 
              let () = Buffer.add_string buf ("\""^name^"\"") in
              let () = Buffer.add_string buf ": " in
              let () = Buffer.add_string buf (Int32.to_string size) in
              let () = Buffer.add_string buf ",\n" in
              buf
           ) used_arrays (Buffer.create 500)) in
    let () = add "_comment2 : \"Unresolved Array Types : \",\n" in
    let () = Buffer.add_buffer b 
        (Hashtbl.fold 
           (fun name size buf -> 
              let () = Buffer.add_string buf ("\""^name^"\"") in
              let () = Buffer.add_string buf ": " in
              let () = Buffer.add_string buf (string_of_int size) in
              let () = Buffer.add_string buf ",\n" in
              buf
           ) unrsv_arrays (Buffer.create 500)) in
    let () = add "_comment3 : \"Classes : \",\n" in
    let () = Buffer.add_buffer b 
        (Hashtbl.fold 
           (fun name size buf -> 
              if Buffer.length buf <> 0 then
                Buffer.add_string buf ",\n";
              let () = Buffer.add_string buf name in
              let () = Buffer.add_string buf ": " in
              let () = Buffer.add_string buf (string_of_int (acc_data size jvm nopack)) in
              buf
           ) myds (Buffer.create 500)) in
    let () = add "}]\n" in
    newline ()
  in
  let () = add "}\n" in
  let () = Printf.fprintf oc "%s" (Buffer.contents b) in
  let () = flush stdout in
  close_out oc

let compute_array_size header tsize exprlist =
  let is_const = List.for_all (function | JBir.Const (`Int _) -> true | _ -> false ) exprlist in
  let tsize = Int32.of_int tsize in
  let header = Int32.of_int header in
  let prev_val = ref Int32.zero in
  if is_const then
    let l = List.mapi (fun i x ->
        match x with
        | JBir.Const (`Int c) when i = 0 ->
          let () = prev_val := c in
          Int32.add (Int32.mul c tsize) header
        | JBir.Const (`Int c) ->
          let v = Int32.add (Int32.mul c tsize) header in
          let v = Int32.mul v !prev_val in
          let () = prev_val := c in
          v
        | _ -> failwith "Unexpected error while computing array size"                 
      ) exprlist
    in
    Some (List.fold_left Int32.add Int32.zero l)
  else
    None

let get_arrays header_size cp jclazz jvm used_arrays unresolved_arrays =
  let (ptra,cl) = JRTA.parse_program cp 
      (make_cms (Javalib.get_name jclazz) JProgram.main_signature) in
  let pbir = JProgram.map_program2
      (fun _ -> JBir.transform ~bcv:false ~ch_link:false ~formula:false ~formula_cmd:[] )
      (Some (fun code pp ->  (Ptmap.find pp (JBir.pc_bc2ir code)))
      ) ptra in
  let () = JProgram.iter (fun node -> 
      JProgram.cm_iter (fun cm -> 
          match cm.cm_implementation with
          | Native -> ()
          | Java x -> 
            let instr = Lazy.force x in
            let co = JBir.code instr in
            Array.iter 
              (
                function 
                | (JBir.NewArray (a,b,c) ) as minstr -> 
                  let jtype = get_value ~type_only:true b in
                  let signature = (String.make (List.length c) '[') ^ jtype in 
                  print_endline signature;
                  print_endline "new array"; 
                  print_endline (JBir.print_instr minstr);
                  (match Hashtbl.find_option unresolved_arrays signature with
                   | None -> 
                     let size = get_array_type_size jvm b in
                     let size = compute_array_size jvm.arrayheader_size size c in
                     begin
                       match size with
                       | Some x ->
                         begin 
                           match Hashtbl.find_option used_arrays signature with
                           | None ->
                             let num = 
                               match(Hashtbl.find_option jvm.others signature) with
                               | None ->
                                 x
                               | Some x ->
                                 Int32.of_int x
                             in
                             print_endline ("ADDED "^(Int32.to_string num));
                             Hashtbl.add used_arrays signature num
                           | Some p when p < x -> 
                             print_endline ("REPLACED "^(Int32.to_string p)^" to "^(Int32.to_string x) );
                             Hashtbl.replace used_arrays signature x
                           | Some p when p > x -> 
                             print_endline ("LESS "^(Int32.to_string p)^" > "^(Int32.to_string x) );
                           | _ -> ()
                         end;
                       | None ->
                         let () = prerr_endline ("WARNING: Could not resolve size of '"^signature^"' - Automatically setting it to 1 \n\
                                                                                                   please set it manually in the jvm file") in
                         let () = Hashtbl.remove used_arrays signature in
                         Hashtbl.replace unresolved_arrays signature 1
                     end;
                   | _ -> 
                     print_endline ("NOTRESOLVED : "^signature);
                     ()
                  );
                | _ -> ()
              ) co
        ) node
    ) pbir in
  ()

let () =
  let usage_msg = 
    "Usage: jcl <filename>"
  in
  let jvm_spec = ref "" in
  let cp = ref "" in
  let nopack = ref false in
  let entry_point = ref "" in
  let speclist = Arg.align [
      ("-jvm", Arg.Set_string jvm_spec, 
       "<file>       JVM configuration file");
      ("-cp", Arg.Set_string cp, 
       "<classpath>  Setting classpath");
      ("-main", Arg.Set_string entry_point, 
       "<file>       Class file which contains main method.\n\
       \                       This option is used to analyze maximum\n\
       \                       size of array types used in the program.");
      ("-nopack", Arg.Set nopack, 
       "<bool>       Do not pack memory space (default: false)");
    ] in
  let flist = ref [] in
  let () = Arg.parse speclist (fun x -> flist := x :: !flist ) usage_msg in
  let () = if List.is_empty !flist then
      let () = Arg.usage speclist usage_msg in
      exit 1 in

  (* 
   * All memory allocations are aligned to addresses that are 
   * divisible by 8 (bytes).
   *
   * Data type sizes according to JVM spec (in bytes)
   * Refsize   : 4
   * byte      : 1
   * short     : 2
   * int       : 4
   * long      : 8
   * float     : 4
   * double    : 8
   * boolean   : 1
   * char      : 2
   * objheader : 8 (32-bit, default), 12 (64-bit)
   * alignment : 8 
   *)
  let jvm = {
    refsize           = 4;
    ousize            = 4;
    joohsize          = 4;
    foh               = 4;
    byte_size         = 1;
    short_size        = 2;
    int_size          = 4;
    long_size         = 8;
    float_size        = 4;
    double_size       = 8;
    boolean_size      = 1;
    char_size         = 2;
    ref_size          = 4; 
    objheader_size    = 8; 
    arrayheader_size  = 8 + 4 (* Object header + length-field *);
    align_size        = 8; 
    others=(Hashtbl.create 10)
  } in
  let () = 
    if !jvm_spec <> "" then
      let () = parse_jvm !jvm_spec jvm in
      ()
  in
  (* Do not pack header regardless whether the option was given *)
  let () = jvm.arrayheader_size  <- get_data_sizes jvm.arrayheader_size  jvm.align_size in
  let () = 
    if !nopack then
      begin
        jvm.byte_size         <- get_data_sizes jvm.byte_size         jvm.align_size;
        jvm.short_size        <- get_data_sizes jvm.short_size        jvm.align_size;
        jvm.int_size          <- get_data_sizes jvm.int_size          jvm.align_size;
        jvm.long_size         <- get_data_sizes jvm.long_size         jvm.align_size;
        jvm.float_size        <- get_data_sizes jvm.float_size        jvm.align_size;
        jvm.double_size       <- get_data_sizes jvm.double_size       jvm.align_size;
        jvm.boolean_size      <- get_data_sizes jvm.boolean_size      jvm.align_size;
        jvm.char_size         <- get_data_sizes jvm.char_size         jvm.align_size;
        jvm.ref_size          <- get_data_sizes jvm.ref_size          jvm.align_size;
        jvm.objheader_size    <- get_data_sizes jvm.objheader_size    jvm.align_size;
      end
  in

  let myds = Hashtbl.create 300 in
  let used_arrays = Hashtbl.create 10 in 
  let unrsv_arrays = Hashtbl.create 10 in 
  let clazz_path = class_path !cp in
  let ep = !entry_point <> "" in
  let () = List.iter (fun fn -> 
      iter ~debug:false (fun x -> (function | JClass _ -> get_ds x ~entry_point:ep myds jvm clazz_path used_arrays unrsv_arrays
                                            | JInterface _ -> ()) x
        ) fn) !flist in 
  let () = close_class_path clazz_path in
  let () = print_ds myds in

  let () =
    if ep then
      let () = iter ~debug:false (fun x -> (function | JClass _ -> get_arrays jvm.arrayheader_size !cp x jvm used_arrays unrsv_arrays
                                                     | JInterface _ -> ()) x
        ) !entry_point in 
      ()
  in
  print_endline "resolved : ";
  Hashtbl.iter (fun k v -> 
      print_endline (k^" : "^(Int32.to_string v));
    ) used_arrays ;
  print_endline "unresolved : ";
  Hashtbl.iter (fun k v -> 
      print_endline (k^" : "^(string_of_int v));
    ) unrsv_arrays ;

  let () = make_json jvm myds used_arrays unrsv_arrays !nopack in

  ()

