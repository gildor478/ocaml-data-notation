(******************************************************************************)
(* ocaml-data-notation: Store data using OCaml notation                       *)
(*                                                                            *)
(* Copyright (C) 2009-2011, OCamlCore SARL                                    *)
(* Copyright (C) 2013, Sylvain Le Gall                                        *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)

(** Main for tests
    @author Sylvain Le Gall
  *)

open OUnit2
open TestCommon
open FileUtil
open ODN

let camlp4o = Conf.make_exec "camlp4o"
let ocamlfind = Conf.make_exec "ocamlfind"

let odn_path_default =
  Filename.concat (Sys.getcwd ()) (Filename.concat "_build" "src")

let odn_path =
  Conf.make_string "odn_path" odn_path_default
    "Directory containing pa_odn.cma and pa_noodn.cma."

let pa_type_conv_cma =
 Filename.concat TestConf.pkg_type_conv_dir "pa_type_conv.cma"

let test_files (dir, fns) =
  let nm =
    match fns with
      | [fn] ->
          dir^"/"^fn
      | lst ->
          dir^"/("^(String.concat "|" fns)^")"
  in
    nm >::
    (fun test_ctxt ->
       let odn_path = odn_path test_ctxt in
       let pa_odn_cma = Filename.concat odn_path "pa_odn.cma" in
       let tmpdn = bracket_tmpdir test_ctxt in
       let () =
         (* Copy files to temporary directory. *)
         FileUtil.cp
           (List.map (fun fn -> in_testdata_dir test_ctxt [dir; fn]) fns)
           tmpdn
       in
       let fns' =
         List.map (Filename.concat tmpdn) fns
       in
       let target_program = Filename.concat tmpdn "test" in
       List.iter
         (fun fn ->
            assert_command ~ctxt:test_ctxt
              (camlp4o test_ctxt)
              [pa_type_conv_cma; pa_odn_cma;
               "Camlp4OCamlPrinter.cmo"; fn];
            assert_command ~ctxt:test_ctxt
              (camlp4o test_ctxt)
              [pa_type_conv_cma; pa_odn_cma;
               "Camlp4AstLifter.cmo"; "Camlp4OCamlPrinter.cmo";
               fn])
         fns';

         assert_command ~ctxt:test_ctxt
           (ocamlfind test_ctxt)
           (["ocamlc"; "-g"; "-o"; target_program;
             "-I"; odn_path; "-I"; tmpdn;
             "-package"; "type_conv"; "-syntax";  "camlp4o";
             "-ppopt"; pa_odn_cma; "odn.cma"] @ fns'))

let () =
  run_test_tt_main
    ("odn">:::
     [
       "pure-odn" >::
       (fun test_ctxt ->
          assert_equal
            ~msg:"variant-simple"
            "Test"
            (string_of_odn (VRT("Test", [])));
          assert_equal
            ~msg:"variant-module"
            "Test"
            (string_of_odn
               ~opened_modules:["MyTest"]
               (VRT("MyTest.Test", [])));
          assert_equal
            ~msg:"long-record"
            ~printer:(fun s -> s)
            "{foo = 123456; bar = 123456; baz = 123456; foo1 = 123456; \
              foo2 = 1; foo3 = 1}"
            (string_of_odn
               ~opened_modules:["Bar"]
               (REC("Bar",
                    [
                      "foo", INT 123456;
                      "bar", INT 123456;
                      "baz", INT 123456;
                      "foo1", INT 123456;
                      "foo2", INT 1;
                      "foo3", INT 1;
                    ])));
       ());

       test_files
         ("oasis-examples",
          ["PropList.ml"; "OASISTypes.ml"; "main.ml"]);

       test_files ("", ["tuples.ml"]);

       "oasis-example no odn" >::
       (fun test_ctxt  ->
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
          let lst = ref [] in
          let foutput strm =
            let buff = Buffer.create 13 in
            (* Check that output doesn't contain regexp defined
             * before
             *)
            let flush_buffer () =
              lst := Buffer.contents buff :: !lst;
              Buffer.clear buff
            in
              Stream.iter
                (function
                   | '\n' -> flush_buffer ()
                   | c -> Buffer.add_char buff c)
                strm;
              flush_buffer ()
          in
          let pa_noodn_cma =
            Filename.concat (odn_path test_ctxt) "pa_noodn.cma"
          in

            (* Create a file without odn in it *)
            assert_command ~ctxt:test_ctxt ~foutput
              (camlp4o test_ctxt)
              [pa_type_conv_cma; pa_noodn_cma;
               "Camlp4OCamlPrinter.cmo";
               (in_testdata_dir test_ctxt ["oasis-examples"; "OASISTypes.ml"])];

            List.iter
              (fun line ->
                 List.iter
                  (fun rgxp -> assert_not_match rgxp line)
                  regexps)
              (List.rev !lst)
       );

       test_files ("", ["polyvariants.ml"]);
     ])
