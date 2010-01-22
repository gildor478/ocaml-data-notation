
(** Main for tests
   
    @author Sylvain Le Gall
  *)

open OUnit;;
open TestCommon;;
open FileUtil;;

let dbug = 
  ref false

let _res: test_result list = 
  run_test_tt_main
    ("odn">:::
     [
       "oasis-examples" >::
       (bracket 
          (fun () ->
             let pwd = 
               Sys.getcwd ()
             in
               Sys.chdir "tests/data/oasis-examples";
               pwd)
          (fun old_cwd ->
             let odn_path =
               Filename.concat old_cwd "_build/src"
             in
             let pa_odn_cma =
               Filename.concat odn_path "pa_odn.cma"
             in
             let command str =
               if !dbug then
                 prerr_endline ("Running: "^str);
               Sys.command str
             in
               if !dbug then
                 begin
                   let _i : int =
                     command
                       ("camlp4o /usr/lib/ocaml/type-conv/pa_type_conv.cmo "^
                        pa_odn_cma^" Camlp4OCamlPrinter.cmo OASISTypes.ml")
                   in
                     ()
                 end;
               
               assert_equal
                 ~msg:"Exit code"
                 ~printer:string_of_int
                 0
                 (command
                    ("ocamlfind ocamlc -g -o test -I "^odn_path^
                     " -package type-conv.syntax -syntax camlp4o -ppopt "^
                     pa_odn_cma^" odn.cma PropList.ml OASISTypes.ml main.ml")))
          (fun old_cwd ->
             rm 
               (filter 
                  (Or
                     (Has_extension "cmi",
                      Has_extension "cmo"))
                  (ls "."));
             rm ["test"];
             Sys.chdir old_cwd))
     ])
;;
