(* AUTOBUILD_START *)
(* DO NOT EDIT (digest: d805496b8bd157a66f944620a8a5b644) *)
module BaseEnvLight = struct
# 0 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseEnvLight.ml"
  
  (** Simple environment, allowing only to read values
    *)
  
  module MapString = Map.Make(String)
  
  type t = string MapString.t
  
  (** Environment default file 
    *)
  let default_filename =
    Filename.concat 
      (Filename.dirname Sys.argv.(0))
      "setup.data"
  
  (** Load environment.
    *)
  let load ?(allow_empty=false) ?(filename=default_filename) () =
    if Sys.file_exists filename then
      begin
        let chn =
          open_in_bin filename
        in
        let rmp =
          ref MapString.empty
        in
          begin
            try 
              while true do 
                Scanf.fscanf chn "%s = %S\n" 
                  (fun nm vl -> rmp := MapString.add nm vl !rmp)
              done;
              ()
            with End_of_file ->
              ()
          end;
          close_in chn;
          !rmp
      end
    else if allow_empty then
      begin
        MapString.empty
      end
    else
      begin
        failwith 
          (Printf.sprintf 
             "Unable to load environment, the file '%s' doesn't exist."
             filename)
      end
  
  (** Get a variable that evaluate expression that can be found in it (see
      {!Buffer.add_substitute}.
    *)
  let var_get name env =
    let rec var_expand str =
      let buff =
        Buffer.create ((String.length str) * 2)
      in
        Buffer.add_substitute 
          buff
          (fun var -> 
             try 
               var_expand (MapString.find var env)
             with Not_found ->
               failwith 
                 (Printf.sprintf 
                    "No variable %s defined when trying to expand %S."
                    var 
                    str))
          str;
        Buffer.contents buff
    in
      var_expand (MapString.find name env)
end


# 82 "myocamlbuild.ml"
module OCamlbuildFindlib = struct
# 0 "/home/gildor/programmation/ocaml-autobuild/src/ocamlbuild/OCamlbuildFindlib.ml"
  (** OCamlbuild extension, copied from 
    * http://brion.inria.fr/gallium/index.php/Using_ocamlfind_with_ocamlbuild
    * by N. Pouillard and others
    *
    * Updated on 2009/02/28
    *
    * Modified by Sylvain Le Gall 
    *)
  open Ocamlbuild_plugin
  
  (* these functions are not really officially exported *)
  let run_and_read = 
    Ocamlbuild_pack.My_unix.run_and_read
  
  let blank_sep_strings = 
    Ocamlbuild_pack.Lexers.blank_sep_strings
  
  let split s ch =
    let x = 
      ref [] 
    in
    let rec go s =
      let pos = 
        String.index s ch 
      in
        x := (String.before s pos)::!x;
        go (String.after s (pos + 1))
    in
      try
        go s
      with Not_found -> !x
  
  let split_nl s = split s '\n'
  
  let before_space s =
    try
      String.before s (String.index s ' ')
    with Not_found -> s
  
  (* this lists all supported packages *)
  let find_packages () =
    List.map before_space (split_nl & run_and_read "ocamlfind list")
  
  (* this is supposed to list available syntaxes, but I don't know how to do it. *)
  let find_syntaxes () = ["camlp4o"; "camlp4r"]
  
  (* ocamlfind command *)
  let ocamlfind x = S[A"ocamlfind"; x]
  
  let dispatch =
    function
      | Before_options ->
          (* by using Before_options one let command line options have an higher priority *)
          (* on the contrary using After_options will guarantee to have the higher priority *)
          (* override default commands by ocamlfind ones *)
          Options.ocamlc     := ocamlfind & A"ocamlc";
          Options.ocamlopt   := ocamlfind & A"ocamlopt";
          Options.ocamldep   := ocamlfind & A"ocamldep";
          Options.ocamldoc   := ocamlfind & A"ocamldoc";
          Options.ocamlmktop := ocamlfind & A"ocamlmktop"
                                  
      | After_rules ->
          
          (* When one link an OCaml library/binary/package, one should use -linkpkg *)
          flag ["ocaml"; "link"] & A"-linkpkg";
          
          (* For each ocamlfind package one inject the -package option when
           * compiling, computing dependencies, generating documentation and
           * linking. *)
          List.iter 
            begin fun pkg ->
              flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
            end 
            (find_packages ());
  
          (* Like -package but for extensions syntax. Morover -syntax is useless
           * when linking. *)
          List.iter begin fun syntax ->
          flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "infer_interface"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          end (find_syntaxes ());
  
          (* The default "thread" tag is not compatible with ocamlfind.
           * Indeed, the default rules add the "threads.cma" or "threads.cmxa"
           * options when using this tag. When using the "-linkpkg" option with
           * ocamlfind, this module will then be added twice on the command line.
           *                        
           * To solve this, one approach is to add the "-thread" option when using
           * the "threads" package using the previous plugin.
           *)
          flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"])
  
      | _ -> 
          ()
  
end

module OCamlbuildBase = struct
# 0 "/home/gildor/programmation/ocaml-autobuild/src/ocamlbuild/OCamlbuildBase.ml"
  
  (** Base functions for writing myocamlbuild.ml
      @author Sylvain Le Gall
    *)
  
  open Ocamlbuild_plugin
  
  type dir = string
  type name = string
  
  type t =
      {
        lib_ocaml: (name * dir list) list;
        lib_c:     (name * dir) list; 
      }
  
  let dispatch_combine lst =
    fun e ->
      List.iter 
        (fun dispatch -> dispatch e)
        lst 
  
  let dispatch t = 
    function
      | Before_options ->
          let env = 
            BaseEnvLight.load ~filename:(Pathname.basename BaseEnvLight.default_filename) ()
          in
          let no_trailing_dot s =
            if String.length s >= 1 && s.[0] = '.' then
              String.sub s 1 ((String.length s) - 1)
            else
              s
          in
            List.iter
              (fun (opt, var) ->
                 opt := no_trailing_dot (BaseEnvLight.var_get var env))
              [
                Options.ext_obj, "ext_obj";
                Options.ext_lib, "ext_lib";
                Options.ext_dll, "ext_dll";
              ]
  
      | After_rules -> 
          (* Declare OCaml libraries *)
          List.iter 
            (function
               | lib, [] ->
                   ocaml_lib lib;
               | lib, dir :: tl ->
                   ocaml_lib ~dir:dir lib;
                   List.iter 
                     (fun dir -> 
                        flag 
                          ["ocaml"; "use_"^lib; "compile"] 
                          (S[A"-I"; P dir]))
                     tl)
            t.lib_ocaml;
  
          (* Declare C libraries *)
          List.iter
            (fun (lib, dir) ->
                 (* Handle C part of library *)
                 flag ["link"; "library"; "ocaml"; "byte"; "use_lib"^lib]
                   (S[A"-dllib"; A("-l"^lib); A"-cclib"; A("-l"^lib)]);
  
                 flag ["link"; "library"; "ocaml"; "native"; "use_lib"^lib]
                   (S[A"-cclib"; A("-l"^lib)]);
                      
                 flag ["link"; "program"; "ocaml"; "byte"; "use_lib"^lib]
                   (S[A"-dllib"; A("dll"^lib)]);
  
                 (* When ocaml link something that use the C library, then one
                    need that file to be up to date.
                  *)
                 dep  ["link"; "ocaml"; "use_lib"^lib] 
                   [dir/"lib"^lib^"."^(!Options.ext_lib)];
  
                 (* Setup search path for lib *)
                 flag ["link"; "ocaml"; "use_"^lib] 
                   (S[A"-I"; P(dir)]);
            )
            t.lib_c
      | _ -> 
          ()
  
  let dispatch_default t =
    dispatch_combine 
      [
        dispatch t;
        OCamlbuildFindlib.dispatch;
      ]
end


# 287 "myocamlbuild.ml"
let package_default =
  {
     OCamlbuildBase.lib_ocaml =
       [
          ("src/pa_noodn", ["src"]);
          ("src/pa_odn", ["src"]);
          ("src/odn", ["src"])
       ];
     lib_c = [];
     }
  ;;

let dispatch_default = OCamlbuildBase.dispatch_default package_default;;

(* AUTOBUILD_STOP *)
Ocamlbuild_plugin.dispatch dispatch_default;;
