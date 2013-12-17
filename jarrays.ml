open Sawja_pack
open Javalib_pack
open Javalib
open JBasics
open JBir
open JProgram
open Jclutil
module Array = Batteries.Array
module Option = Batteries.Option

let get_arrays header_size cp jclazz =
  let (ptra,cl) = JRTA.parse_program cp 
      (make_cms (Javalib.get_name jclazz) JProgram.main_signature) in
  let pbir = JProgram.map_program2
      (fun _ -> JBir.transform ~bcv:false ~ch_link:false ~formula:false ~formula_cmd:[] )
      (Some (fun code pp ->  (Ptmap.find pp (JBir.pc_bc2ir code)))
      ) ptra in
  let () = iter (fun node -> 
      let () = cm_iter (fun cm -> 
          match cm.cm_implementation with
          | Native -> ()
          | Java x -> 
            let instr = Lazy.force x in
            let co = code instr in
            let () = Array.iter 
                (
                  function 
                  | (NewArray (a,b,c) ) as x -> 
                    let ss = get_value b in
                    print_endline ss;
                    print_endline "new array";
                    print_endline (print_instr x)
                  | _ -> ()
                ) co in
            ()

        ) node in
      ()
    ) pbir in
  ()

