(********************************************************************************)
(*  ODN: Dump data using OCaml notation                                         *)
(*                                                                              *)
(*  Copyright (C) 2009-2010, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

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
  let odn_path =
    Filename.concat (Sys.getcwd ()) (Filename.concat "_build" "src")
  in
  let pa_odn_cma =
    Filename.concat odn_path "pa_odn.cma"
  in
  let pa_noodn_cma =
    Filename.concat odn_path "pa_noodn.cma"
  in
  let pa_type_conv_cmo = 
    Filename.concat TestConf.pkg_type_conv_dir "pa_type_conv.cmo"
  in
  let test_files (dir, fns) = 
    let nm =
      match fns with 
        | [fn] ->
            dir^"/"^fn
        | lst ->
            dir^"/("^(String.concat "|" fns)^")" 
    in
      nm >::
      (bracket 
         (fun () ->
            let pwd = 
              Sys.getcwd ()
            in
              Sys.chdir dir;
              pwd)
         (fun _ ->
            if !dbug then
              begin
                List.iter 
                  (fun fn ->
                     print_endline ("File "^fn^": ");
                     assert_command
                       "camlp4o" 
                       [pa_type_conv_cmo; pa_odn_cma; 
                        "Camlp4OCamlPrinter.cmo"; fn];
                     assert_command
                       "camlp4o"
                       [pa_type_conv_cmo; pa_odn_cma;
                        "Camlp4AstLifter.cmo"; "Camlp4OCamlPrinter.cmo"; 
                        fn])
                  fns
              end;
            
            assert_command
              "ocamlfind"
              (["ocamlc"; "-g"; "-o"; "test"; "-I"; odn_path;
                "-package"; "type-conv.syntax"; "-syntax";  "camlp4o";
                "-ppopt"; pa_odn_cma; "odn.cma"] @ fns)))
         (fun old_cwd ->
            rm 
              (filter 
                 (Or
                    (Has_extension "cmi",
                     Has_extension "cmo"))
                 (ls "."));
            rm ["test"];
            Sys.chdir old_cwd);
  in
    run_test_tt_main
      ~set_verbose:(fun b -> dbug := b)
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

         test_files
           ("tests/data/oasis-examples",
            ["PropList.ml"; "OASISTypes.ml"; "main.ml"]);

         test_files
           ("tests/data/",
            ["tuples.ml"]);
     
         "oasis-example no odn" >::
         (fun () ->
            let regexps = 
              [
                Str.regexp_string "TYPE_CONV_PATH";
                Str.regexp "with  *odn";
              ]
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
            let foutput strm = 
              (* Check that output doesn't contain regexp defined
               * before
               *)
              let buff = 
                Buffer.create 13
              in
              let check_buffer () = 
                let ln =
                  Buffer.contents buff
                in
                  List.iter 
                    (fun rgxp -> assert_not_match rgxp ln)
                    regexps;
                  Buffer.clear buff
              in
                Stream.iter 
                  (function
                     | '\n' ->
                         check_buffer ()
                     | c ->
                         Buffer.add_char buff c)
                  strm;
                check_buffer ()
            in

              (* Create a file without odn in it *)
              assert_command 
                ~foutput
                "camlp4o" 
                [pa_type_conv_cmo; pa_noodn_cma; 
                 "Camlp4OCamlPrinter.cmo";
                 "tests/data/oasis-examples/OASISTypes.ml"]
         );
         
         test_files
            ("tests/data/",
             ["polyvariants.ml"]);
       ])
;;
