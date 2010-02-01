
(** Main for tests
   
    @author Sylvain Le Gall
  *)

open OUnit;;
open TestCommon;;
open FileUtil;;
open ODN;;

let dbug = 
  ref false

let _res: test_result list = 
  let assert_command ?(exit_code=0) str =
    assert_equal 
      ~msg:("Running: "^str)
      ~printer:string_of_int
      exit_code
      (Sys.command str)
  in
  let odn_path =
    Filename.concat (Sys.getcwd ()) "_build/src"
  in
  let pa_odn_cma =
    Filename.concat odn_path "pa_odn.cma"
  in
  let pa_noodn_cma =
    Filename.concat odn_path "pa_noodn.cma"
  in
    run_test_tt_main
      ("odn">:::
       [
         "pure-odn" >::
         (fun () ->
            assert_equal 
              ~msg:"variant-simple"
              "Test"
              (string_of_odn (VRT("Test", [])));
            assert_equal
              ~msg:"variant-module"
              "Test"
              (string_of_odn 
                 ~opened_modules:["MyTest"]  
                 (VRT("MyTest.Test", []))));

         "oasis-examples" >::
         (bracket 
            (fun () ->
               let pwd = 
                 Sys.getcwd ()
               in
                 Sys.chdir "tests/data/oasis-examples";
                 pwd)
            (fun _ ->
               if !dbug then
                 assert_command
                   ("camlp4o /usr/lib/ocaml/type-conv/pa_type_conv.cmo "^
                    pa_odn_cma^" Camlp4OCamlPrinter.cmo OASISTypes.ml");
               
               assert_command
                 ("ocamlfind ocamlc -g -o test -I "^odn_path^
                  " -package type-conv.syntax -syntax camlp4o -ppopt "^
                  pa_odn_cma^" odn.cma PropList.ml OASISTypes.ml main.ml"))
            (fun old_cwd ->
               rm 
                 (filter 
                    (Or
                       (Has_extension "cmi",
                        Has_extension "cmo"))
                    (ls "."));
               rm ["test"];
               Sys.chdir old_cwd));
     
         "oasis-example no odn" >::
         (bracket
            (fun () ->
               Filename.temp_file "ocaml-data-notation" ".stdout")
            (fun fn ->
               let () = 
                 (* Create a file without odn in it *)
                 assert_command 
                   ("camlp4o /usr/lib/ocaml/type-conv/pa_type_conv.cmo "^
                    pa_noodn_cma^" Camlp4OCamlPrinter.cmo "^
                    "tests/data/oasis-examples/OASISTypes.ml > "^fn)
               in
               let regexps = 
                 [
                   Str.regexp_string "TYPE_CONV_PATH";
                   Str.regexp "with  *odn";
                 ]
               in
               let chn =
                 open_in fn
               in
               let assert_not_match regexp str =
                 try
                   let _i : int =
                     Str.search_forward regexp str 0
                   in
                     assert_failure 
                       (Printf.sprintf 
                          "Found '%s' in string '%s'" 
                          (Str.matched_string str) 
                          str)
                 with Not_found ->
                   ()
               in
                 try
                   while true do 
                     let ln =
                       input_line chn
                     in
                       List.iter 
                         (fun rgxp -> assert_not_match rgxp ln)
                         regexps
                   done
                 with End_of_file ->
                   close_in chn)
            (fun fn ->
               rm [fn]))
       ])

;;
