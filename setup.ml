#!/usr/bin/ocamlrun ocaml
(* AUTOBUILD_START *)
(* DO NOT EDIT (digest: 004ff52ba910db93ec3c0af0359c5e8d) *)
module CommonGettext =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/common/CommonGettext.ml"
  
  (** Gettext interface for Autobuild
    *)
  
  let s_ str = 
    str
  ;;
  
  let f_ (str : ('a, 'b, 'c) format) =
    str
  ;;
end;;

module PropList =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/common/PropList.ml"
  
  (** Property list 
      @author Sylvain Le Gall
    *)
  
  open CommonGettext;;
  
  type name_t = string
  ;;
  
  let no_context f =
    fun ?context s -> f s
  ;;
  
  exception Not_set of name_t;;
  exception No_printer of name_t;;
  exception Unknown_field of name_t * name_t;;
  
  let string_of_exception =
    function
      | Not_set nm ->
          Printf.sprintf (f_ "Value %s is not set") nm
      | No_printer nm ->
          Printf.sprintf (f_ "No default printer for value %s") nm
      | Unknown_field (nm, schm) ->
          Printf.sprintf (f_ "Field %s is not defined in schema %s") nm schm
      | e ->
          raise e
  ;;
  
  module Data =
  struct
  
    type t = 
        (name_t, unit -> unit) Hashtbl.t
  
    let create () =
      Hashtbl.create 13
  
  end
  ;;
  
  module Schema = 
  struct
  
    type ('ctxt, 'extra) value_t =
        {
          get:   Data.t -> string;
          set:   Data.t -> ?context:'ctxt  -> string -> unit;
          help:  (unit -> string) option;
          extra: 'extra;
        }
  
    type ('ctxt, 'extra) t =
        {
          name:      name_t;
          fields:    (name_t, ('ctxt, 'extra) value_t) Hashtbl.t;
          presets:   (name_t, 'ctxt option * string) Hashtbl.t;
          order:     name_t Queue.t;
          name_norm: string -> string;
        }
  
    let create ?(case_insensitive=false) nm = 
      {
        name      = nm;
        fields    = Hashtbl.create 13;
        presets   = Hashtbl.create 13;
        order     = Queue.create ();
        name_norm = 
          (if case_insensitive then 
             String.lowercase
           else
             fun s -> s);
      }
  
    let add t nm set get extra help = 
      let key = 
        t.name_norm nm
      in
  
      (* If available, set preset values *)
      let update_preset data =
        if Hashtbl.mem t.presets nm then
          begin
            let context, v = 
              Hashtbl.find t.presets nm
            in
              Hashtbl.remove t.presets nm;
              set data ?context v
          end
      in
  
      (* Set preset value before any other *)
      let set data ?context x =
        update_preset data;
        set data ?context x
      in
  
      (* Before get, set preset value *)
      let get data =
        update_preset data;
        get data
      in
  
        if Hashtbl.mem t.fields key then
          failwith 
            (Printf.sprintf 
               (f_ "Field '%s' is already defined in schema '%s'")
               nm t.name);
        Hashtbl.add 
          t.fields 
          key 
          {
            set   = set; 
            get   = get; 
            help  = help;
            extra = extra;
          };
        Queue.add nm t.order 
  
    let mem t nm =
      Hashtbl.mem t.fields nm 
  
    let find t nm = 
      try
        Hashtbl.find t.fields (t.name_norm nm)
      with Not_found ->
        raise (Unknown_field (nm, t.name))
  
    let get t data nm =
      (find t nm).get 
        data
  
    let set t data nm ?context x =
      (find t nm).set 
        data 
        ?context 
        x
  
    let preset t data nm ?context x = 
      Hashtbl.add t.presets nm (context, x)
  
    let fold f acc t =
      Queue.fold 
        (fun acc k ->
           let v =
             find t k
           in
             f acc k v.extra v.help)
        acc 
        t.order
  
    let iter f t =
      fold 
        (fun () -> f)
        ()
        t
  
  end
  ;;
  
  module Field =
  struct
  
    type ('ctxt, 'value, 'extra) t =
        {
          set:    Data.t -> 'value -> unit;
          get:    Data.t -> 'value;
          sets:   Data.t -> ?context:'ctxt -> string -> unit;
          gets:   Data.t -> string;
          help:   (unit -> string) option;
          extra:  'extra;
        }
  
    let new_id = 
      let last_id =
        ref 0
      in
        fun () -> incr last_id; !last_id
  
    let create ?schema ?name ?parse ?print ?default ?update ?help extra =
      (* Default value container *)
      let v = 
        ref None 
      in
  
      (* If name is not given, create unique one *)
      let nm = 
        match name with 
          | Some s -> s
          | None -> Printf.sprintf "_anon_%d" (new_id ())
      in
  
      (* Last chance to get a value: the default *)
      let default () = 
        match default with 
          | Some d -> d
          | None -> raise (Not_set nm) 
      in
  
      (* Get data *)
      let get data =
        (* Get value *)
        try 
          (Hashtbl.find data nm) ();
          match !v with 
            | Some x -> x 
            | None -> default ()
        with Not_found ->
          default ()
      in
  
      (* Set data *)
      let set data x = 
        let x = 
          match update with 
            | Some f ->
                begin
                  try 
                    f (get data) x
                  with Not_set _ ->
                    x
                end
            | None ->
                x
        in
          Hashtbl.replace 
            data 
            nm 
            (fun () -> v := Some x) 
      in
  
      (* Parse string value, if possible *)
      let parse =
        match parse with 
          | Some f -> 
              f
          | None ->
              fun ?context s ->
                failwith 
                  (Printf.sprintf 
                     (f_ "Cannot parse field '%s' when setting value %S")
                     nm
                     s)
      in
  
      (* Set data, from string *)
      let sets data ?context s =
        set data (parse ?context s)
      in
  
      (* Output value as string, if possible *)
      let print =
        match print with
          | Some f ->
              f
          | None ->
              fun _ -> raise (No_printer nm)
      in
  
      (* Get data, as a string *)
      let gets data =
        print (get data)
      in
  
        begin 
          match schema with 
            | Some t ->
                Schema.add t nm sets gets extra help
            | None ->
                ()
        end;
  
        {
          set   = set;
          get   = get;
          sets  = sets;
          gets  = gets;
          help  = help;
          extra = extra;
        }
  
    let fset data t x = 
      t.set data x
  
    let fget data t =
      t.get data
  
    let fsets data t ?context s =
      t.sets data ?context s
  
    let fgets data t =
      t.gets data 
  
  end
  ;;
  
  module FieldRO =
  struct
  
    let create ?schema ?name ?parse ?print ?default ?update ?help extra =
      let fld = 
        Field.create ?schema ?name ?parse ?print ?default ?update ?help extra
      in
        fun data -> Field.fget data fld
  
  end;;
end;;


# 333 "setup.ml"
module BaseEnvLight =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseEnvLight.ml"
  
  (** Simple environment, allowing only to read values
    *)
  
  module MapString = Map.Make(String);;
  
  type t = string MapString.t;;
  
  (** Environment default file 
    *)
  let default_filename =
    Filename.concat 
      (Filename.dirname Sys.argv.(0))
      "setup.data"
  ;;
  
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
  ;;
  
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
  ;;
end;;

module BaseEnv =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseEnv.ml"
  
  (** Read-only environment
      @author Sylvain Le Gall
    *)
  
  (** Origin of the variable, if a variable has been already set
      with a higher origin, it won't be set again
    *)
  type origin_t = 
    | ODefault     (** Default computed value *)
    | OGetEnv      (** Extracted from environment, using Sys.getenv *)
    | OFileLoad    (** From loading file setup.data *)
    | OCommandLine (** Set on command line *)
  ;;
  
  (** Command line handling for variable 
    *)
  type cli_handle_t =
    (** No command line argument *)
    | CLINone
    (** Build using variable name and help text *)
    | CLIAuto
     (** Use prefix --with- *)
    | CLIWith
    (** Use --enable/--disable *)
    | CLIEnable
    (** Fully define the command line arguments *)
    | CLIUser of (Arg.key * Arg.spec * Arg.doc) list
  ;;
  
  (** Variable type
    *)
  type definition_t =
      {
        hide:       bool;
        dump:       bool;
        cli:        cli_handle_t;
        arg_help:   string option;
        group:      string option;
      }
  ;;
  
  (** Schema for environment 
    *)
  let schema =
    PropList.Schema.create "environment"
  ;;
  
  (** Environment data 
    *)
  let env = 
    PropList.Data.create ()
  ;;
  
  (** Expand variable that can be found in string. Variable follow definition of
    * variable for {!Buffer.add_substitute}.
    *)
  let rec var_expand str =
    let buff =
      Buffer.create ((String.length str) * 2)
    in
      Buffer.add_substitute 
        buff
        (fun var -> 
           try 
             var_get var 
           with PropList.Unknown_field (_, _) ->
             failwith 
               (Printf.sprintf 
                  "No variable %s defined when trying to expand %S."
                  var 
                  str))
        str;
      Buffer.contents buff
  
  (** Get variable 
    *)
  and var_get name =
    let vl = 
      (* TODO: catch exception that can be raised here at upper level *)
      PropList.Schema.get schema env name
    in
      var_expand vl
  ;;
  
  (** Protect a variable content, to avoid expansion
    *)
  let var_protect vl = 
    let buff = 
      Buffer.create (String.length vl)
    in
      String.iter
        (function 
           | '$' -> Buffer.add_string buff "\\$"
           | c   -> Buffer.add_char   buff c)
        vl;
      Buffer.contents buff
  ;;
  
  (** Define a variable 
    *)
  let var_define 
        ?(hide=false) 
        ?(dump=true) 
        ?short_desc
        ?(cli=CLINone)
        ?arg_help
        ?group 
        name
        dflt =
  
    let default =
      (ODefault, dflt)
      ::
      (try 
         [OGetEnv, lazy (Sys.getenv name)] 
       with Not_found ->
         [])
  
    in
  
    let extra = 
      {
        hide     = hide;
        dump     = dump;
        cli      = cli;
        arg_help = arg_help;
        group    = group;
      }
    in
  
    (* Try to find a value that can be defined 
     *)
    let rec var_get_low = 
      let rec higher_priority o1 v1 =
        function
          | (o2, v2) :: tl ->
              if o1 < o2 then
                begin
                  try 
                    higher_priority o2 (Lazy.force v2) tl 
                  with Not_found ->
                    higher_priority o1 v1 tl
                end
              else
                higher_priority o1 v1 tl
          | [] ->
              v1
      in
  
        function
          | (o, v) :: tl -> 
              begin
                try 
                  higher_priority o (Lazy.force v) tl
                with Not_found ->
                  var_get_low tl 
              end
          | [] ->
              failwith 
                (Printf.sprintf 
                   "Variable %s is not set"
                   name)
    in
  
    let help =
      match short_desc with 
        | Some s -> Some (fun () -> s)
        | None -> None
    in
  
    let var_get_lst = 
      PropList.FieldRO.create
        ~schema
        ~name
        ~parse:(fun ?(context=ODefault) s -> [context, lazy s])
        ~print:var_get_low
        ~default
        ~update:(fun x old_x -> x @ old_x)
        ?help
        extra
    in
  
      fun () ->
        var_expand (var_get_low (var_get_lst env))
  ;;
  
  (** Define a variable or redefine it
    *)
  let var_redefine 
        ?hide
        ?dump
        ?short_desc
        ?cli
        ?arg_help
        ?group 
        name 
        dflt =
    if PropList.Schema.mem schema name then
      begin
        fun () -> 
          var_get name 
      end
    else
      begin
        var_define 
          ?hide
          ?dump
          ?short_desc
          ?cli
          ?arg_help
          ?group 
          name 
          dflt
      end
  ;;
  
  (** Well-typed ignore for var_define 
    *)
  let var_ignore (e : unit -> string) = 
    ()
  ;;
  
  (** Display all variable 
    *)
  let print_hidden =
    var_define 
      ~hide:true
      ~dump:false
      ~cli:CLIAuto
      ~arg_help:"Print even non-printable variable. (debug)"
      "print_hidden"
      (lazy "false")
  ;;
  
  (** Get all variable
    *)
  let var_all () =
    List.rev
      (PropList.Schema.fold
         (fun acc nm def _ -> 
            if not def.hide || bool_of_string (print_hidden ()) then
              nm :: acc
            else
              acc)
         []
         schema)
  ;;
  
  (** Environment default file 
    *)
  let default_filename =
    BaseEnvLight.default_filename
  ;;
  
  (** Initialize environment.
    *)
  let load ?(allow_empty=false) ?(filename=default_filename) () =
    if Sys.file_exists filename then
      (
        let chn =
          open_in_bin filename
        in
        let st =
          Stream.of_channel chn
        in
        let line =
          ref 1
        in
        let st_line = 
          Stream.from
            (fun _ ->
               try
                 match Stream.next st with 
                   | '\n' -> incr line; Some '\n'
                   | c -> Some c
               with Stream.Failure -> None)
        in
        let lexer = 
          Genlex.make_lexer ["="] st_line
        in
        let rec read_file () =
          match Stream.npeek 3 lexer with 
            | [Genlex.Ident nm; Genlex.Kwd "="; Genlex.String value] ->
                Stream.junk lexer; 
                Stream.junk lexer; 
                Stream.junk lexer;
                PropList.Schema.preset schema env nm ~context:OFileLoad value;
                read_file ()
            | [] ->
                ()
            | _ ->
                failwith 
                  (Printf.sprintf 
                     "Malformed data file '%s' line %d"
                     filename !line)
        in
          read_file ();
          close_in chn
      )
    else if not allow_empty then
      (
        failwith 
          (Printf.sprintf 
             "Unable to load environment, the file '%s' doesn't exist."
             filename)
      )
  ;;
  
  (** Save environment on disk.
    *)
  let dump ?(filename=default_filename) () = 
    let chn =
      open_out_bin filename
    in
      PropList.Schema.iter
        (fun nm def _ ->
           if def.dump then
             begin
               try 
                 let value =
                   PropList.Schema.get 
                     schema 
                     env 
                     nm
                 in
                   Printf.fprintf chn "%s = %S\n" nm value
               with PropList.Not_set _ ->
                 ()
             end)
        schema;
      close_out chn
  ;;
  
  (** Display environment to user.
    *)
  let print () =
    let printable_vars =
      PropList.Schema.fold
        (fun acc nm def short_descr_opt -> 
           if not def.hide || bool_of_string (print_hidden ()) then
             begin
               let value = 
                 PropList.Schema.get 
                   schema
                   env
                   nm
               in
               let txt = 
                 match short_descr_opt with 
                   | Some s -> s ()
                   | None -> nm
               in
                 (txt, value) :: acc
             end
           else
             acc)
        []
        schema
    in
    let max_length = 
      List.fold_left max 0
        (List.rev_map String.length
           (List.rev_map fst printable_vars))
    in
    let dot_pad str =
      String.make ((max_length - (String.length str)) + 3) '.'
    in
  
    print_newline ();
    print_endline "Configuration: ";
    print_newline ();
    List.iter 
      (fun (name,value) -> 
         Printf.printf "%s: %s %s\n" name (dot_pad name) value)
      printable_vars;
    Printf.printf "%!";
    print_newline ();
  ;;
  
  (** Default command line arguments 
    *)
  let args () =
    let tr_arg str =
      let buff =
        Buffer.create (String.length str)
      in
        String.iter 
          (function 
             | '_' | ' ' | '\n' | '\r' | '\t' -> Buffer.add_char buff '-'
             | c -> Buffer.add_char buff c
          )
          str;
        Buffer.contents buff
    in
      [
        "--override",
         Arg.Tuple
           (
             let rvr = ref ""
             in
             let rvl = ref ""
             in
               [
                 Arg.Set_string rvr;
                 Arg.Set_string rvl;
                 Arg.Unit 
                   (fun () -> 
                      PropList.Schema.set  
                        schema
                        env
                        ~context:OCommandLine 
                        !rvr
                        !rvl)
               ]
           ),
        "var+val  Override any configuration variable.";
  
      ]
      @
      List.flatten 
        (PropList.Schema.fold
          (fun acc name def short_descr_opt ->
             let var_set s = 
               PropList.Schema.set 
                 schema
                 env
                 ~context:OCommandLine 
                 name
                 s
             in
  
             let arg_name = 
               tr_arg name
             in
  
             let hlp =
               match short_descr_opt with 
                 | Some txt -> txt ()
                 | None -> ""
             in
  
             let arg_hlp =
               match def.arg_help with 
                 | Some s -> s
                 | None   -> "str"
             in
  
             let value = 
               PropList.Schema.get
                 schema
                 env
                 name
             in
  
             let args = 
               match def.cli with 
                 | CLINone -> 
                     []
                 | CLIAuto -> 
                     [
                       "--"^arg_name,
                       Arg.String var_set,
                       arg_hlp^" "^hlp^" ["^value^"]"
                     ]
                 | CLIWith ->
                     [
                       "--with-"^arg_name,
                       Arg.String var_set,
                       arg_hlp^" "^hlp^" ["^value^"]"
                     ]
                 | CLIEnable ->
                     [
                       "--enable-"^arg_name,
                       Arg.Unit (fun () -> var_set "true"),
                       " "^hlp^(if value = "true" then " [default]" else "");
  
                       "--disable-"^arg_name,
                       Arg.Unit (fun () -> var_set "false"),
                       " "^hlp^(if value <> "true" then " [default]" else "");
                     ]
                 | CLIUser lst ->
                     lst
             in
               args :: acc)
           []
           schema)
  ;;
  
end;;

module BaseUtils =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseUtils.ml"
  
  
  (** Set for String 
    *)
  module SetString = Set.Make(String)
  
  
  (** Add a list to a SetString
    *)
  let set_string_add_list st lst =
    List.fold_left 
      (fun acc e -> SetString.add e acc)
      st
      lst
  ;;
  
  (** Build a set out of list 
    *)
  let set_string_of_list =
    set_string_add_list
      SetString.empty
  ;;
  
  
  (** Remove trailing whitespace *)
  let strip_whitespace str =
    let strlen = 
      String.length str
    in
    let is_whitespace =
      function 
        | ' ' | '\t' | '\r' | '\n' -> true
        | _ -> false
    in 
    let skip_beg =
      let idx =
        ref 0
      in
        while !idx < strlen && is_whitespace str.[!idx] do 
          incr idx 
        done;
        !idx
    in
    let skip_end =
      let idx = 
        ref ((String.length str) - 1)
      in
        while !idx >= 0 && is_whitespace str.[!idx] do
          decr idx
        done;
        !idx
    in
      if skip_beg <= skip_end then
        String.sub str skip_beg (skip_end - skip_beg + 1)
      else
        ""
  ;;
  
  (** Split a string, separator not included
    *)
  let split sep str =
    let str_len =
      String.length str
    in
    let rec split_aux acc pos =
      if pos < str_len then
        (
          let pos_sep = 
            try
              String.index_from str pos sep
            with Not_found ->
              str_len
          in
          let part = 
            String.sub str pos (pos_sep - pos) 
          in
          let acc = 
            part :: acc
          in
            if pos_sep >= str_len then
              (
                (* Nothing more in the string *)
                List.rev acc
              )
            else if pos_sep = (str_len - 1) then
              (
                (* String end with a separator *)
                List.rev ("" :: acc)
              )
            else
              (
                split_aux acc (pos_sep + 1)
              )
        )
      else
        (
          List.rev acc
        )
    in
      split_aux [] 0
  ;;
  
end;;

module BaseMessage =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseMessage.ml"
  
  (** Message to user
      @author Sylvain Le Gall
    *)
  
  let verbose =
    ref true
  ;;
  
  (** Print a warning message 
    *)
  let warning str =
    if !verbose then
      prerr_endline str
  ;;
  
  (** Print an error message and exit.
    *)
  let error str =
    if !verbose then 
      prerr_endline str;
    exit 1
  ;;
  
  (** Print information message.
    *)
  let info str = 
    if !verbose then
      Printf.printf "%s\n%!" str
  ;;
  
  (** Print begin of line when checking for a feature.
    *)
  let checking str =
    if !verbose then
      Printf.printf "checking for %s... %!" str
  ;;
  
  (** Print end of line when checking for a feature.
    *)
  let result str =
    if !verbose then
      Printf.printf "%s\n%!" str
  ;;
  
  (** Print result and return it.
    *)
  let result_wrap str =
    result str;
    str
  ;;
  
end;;

module BaseExec =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseExec.ml"
  
  (** Running commands 
      @author Sylvain Le Gall
    *)
  
  (** Run a command 
    *)
  let run cmd args =
    let cmdline =
      String.concat " " (cmd :: args)
    in
      BaseMessage.info 
        (Printf.sprintf "Running command '%s'" cmdline);
      match Sys.command cmdline with 
        | 0 ->
            ()
        | i ->
            failwith 
              (Printf.sprintf 
                 "Command '%s' terminated with error code %d"
                 cmdline i)
  ;;
  
  (** Run a command and returns its output
    *)
  let run_read_output cmd args =
    let fn = 
      Filename.temp_file "ocaml-autobuild" ".txt"
    in
    let () = 
      try
        run cmd (args @ [">"; fn])
      with e ->
        Sys.remove fn;
        raise e
    in
    let chn =
      open_in fn
    in
    let routput =
      ref []
    in
      (
        try
          while true do 
            routput := (input_line chn) :: !routput
          done
        with End_of_file ->
          ()
      );
      close_in chn;
      Sys.remove fn;
      List.rev !routput
  ;;
  
  (** Run a command and returns only first line 
    *)
  let run_read_one_line cmd args = 
    match run_read_output cmd args with 
      | [fst] -> 
          fst
      | lst -> 
          failwith 
            (Printf.sprintf
               "Command return unexpected output %S"
               (String.concat "\n" lst))
  ;;
  
end;;

module BaseFileUtil =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseFileUtil.ml"
  
  (** {1 File operation (install, which...)
    *)
  
  (** Find a file among all provided alternatives
    *)
  let find_file paths exts = 
  
    (* Cardinal product of two list *)
    let ( * ) lst1 lst2 = 
      List.flatten 
        (List.map 
           (fun a -> 
              List.map 
                (fun b -> a,b) 
                lst2) 
           lst1)
    in
  
    let rec combined_paths lst = 
      match lst with
        | p1 :: p2 :: tl ->
            let acc = 
              (List.map 
                 (fun (a,b) -> Filename.concat a b) 
                 (p1 * p2))
            in
              combined_paths (acc :: tl)
        | [e] ->
            e
        | [] ->
            []
    in
  
    let alternatives =
      List.map 
        (fun (p,e) -> 
           if String.length e > 0 && e.[0] <> '.' then
             p ^ "." ^ e
           else
             p ^ e) 
        ((combined_paths paths) * exts)
    in
      try 
        List.find Sys.file_exists alternatives
      with Not_found ->
        failwith 
          (Printf.sprintf 
             "Cannot find any of the files: %s"
             (String.concat ", " 
                (List.map 
                   (Printf.sprintf "%S")
                   alternatives)))
  ;;
  
  (** Find real filename of an executable
    *)
  let which prg =
    let path_sep =
      match Sys.os_type with 
        | "Win32" ->
            ';'
        | _ ->
            ':'
    in
    let path_lst =
      BaseUtils.split 
        path_sep 
        (Sys.getenv "PATH")
    in
    let exec_ext = 
      match Sys.os_type with 
        | "Win32" ->
            "" 
            :: 
            (BaseUtils.split 
               path_sep 
               (Sys.getenv "PATHEXT"))
        | _ ->
            [""]
    in
      find_file [path_lst; [prg]] exec_ext;  
  ;;
  
  (** Copy a file 
    *)
  let cp src tgt = 
    match Sys.os_type with 
      | "Win32" ->
          BaseExec.run "copy" [src; tgt]
      | _ ->
          BaseExec.run "cp" [src; tgt]
  ;;
  
  (** Create a directory
    *)
  let mkdir tgt =
    match Sys.os_type with 
      | "Win32" ->
          BaseExec.run "md" [tgt]
      | _ ->
          BaseExec.run "mkdir" [tgt]
  ;;
  
  (** Remove a directory
    *)
  let rmdir tgt =
    if Sys.readdir tgt = [||] then
      (
        match Sys.os_type with 
          | "Win32" ->
              BaseExec.run "rd" [tgt]
          | _ ->
              BaseExec.run "rm" ["-r"; tgt]
      )
  ;;
end;;

module BaseExpr =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseExpr.ml"
  
  (** Conditional expression like in OASIS.
      @author Sylvain Le Gall
    *)
  
  open BaseEnv;;
  
  type t =
    | Bool of bool
    | Not of t
    | And of t * t
    | Or of t * t
    (* TODO: use a var here *)
    | Flag of string
    | Test of string * string
  ;;
  
  type 'a choices = (t * 'a) list
  ;;
  
  (** Evaluate expression *)
  let rec eval =
    function
      | Bool b ->
          b
  
      | Not e -> 
          not (eval e)
  
      | And (e1, e2) ->
          (eval e1) && (eval e2)
  
      | Or (e1, e2) -> 
          (eval e1) || (eval e2)
  
      | Flag nm ->
          let v =
            var_get nm
          in
            assert(v = "true" || v = "false");
            (v = "true")
  
      | Test (nm, vl) ->
          let v =
            var_get nm
          in
            (v = vl)
  ;;
  
  let choose lst =
    let rec choose_aux = 
      function
        | (cond, vl) :: tl ->
            if eval cond then 
              vl 
            else
              choose_aux tl
        | [] ->
            failwith 
              "No result for a choice list"
    in
      choose_aux (List.rev lst)
  ;;
  
  let singleton e = 
    [Bool true, e]
  ;;
  
end;;

module BaseArgExt =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseArgExt.ml"
  
  (** Handle command line argument
      @author Sylvain Le Gall
    *)
  
  open BaseEnv;;
  
  let parse argv args =
      (* Simulate command line for Arg *)
      let current =
        ref 0
      in
  
        try
          Arg.parse_argv
            ~current:current
            (Array.concat [[|"none"|]; argv])
            (Arg.align args)
            (fun str -> 
               failwith 
                 ("Don't know what to do with arguments: '"^str^"'"))
            "configure options:"
        with Arg.Help txt | Arg.Bad txt ->
          BaseMessage.error txt
  ;;
  
end;;

module BaseVersion =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseVersion.ml"
  
  (** Version comparisons
      @author Sylvain Le Gall
    *)
  
  type t = string
  ;;
  
  type comparator = 
    | VGreater of t
    | VEqual of t
    | VLesser of t
    | VOr of  comparator * comparator
    | VAnd of comparator * comparator
  ;;
  
  (** Compare versions
    *)
  let version_compare v1 v2 =
    let is_digit c =
      '0' <= c && c <= '9'
    in
  
    let buff =
      Buffer.create (String.length v1)
    in
  
    let rec extract_filter test (v, start, len) = 
      if start < len && test v.[start] then
        (
          Buffer.add_char buff v.[start];
          extract_filter test (v, start + 1, len)
        )
      else
        (
          let res =
            Buffer.contents buff
          in
            Buffer.clear buff;
            res, (v, start, len)
        )
    in
    let extract_int vpos =
      let str, vpos =
        extract_filter is_digit vpos
      in
        int_of_string str, vpos
    in
    let extract_non_int vpos =
      extract_filter 
        (fun c -> not (is_digit c)) 
        vpos
    in
    let rec compare_aux ((v1,start1,len1) as vpos1) ((v2,start2,len2) as vpos2) = 
      if start1 < len1 && start2 < len2 then
        (
          if is_digit v1.[start1] && is_digit v2.[start2] then
            (
              let i1, vpos1 =
                extract_int vpos1
              in
              let i2, vpos2 =
                extract_int vpos2
              in
                match i1 - i2 with
                  | 0 -> compare_aux vpos1 vpos2
                  | n -> n
            )
          else
            (
              let str1, vpos1 =
                extract_non_int vpos1
              in
              let str2, vpos2 =
                extract_non_int vpos2
              in
                match String.compare str1 str2 with
                  | 0 -> compare_aux vpos1 vpos2
                  | n -> n
            )
        )
      else 
        (
          len1 - len2 
        )
    in
      compare_aux 
        (v1, 0, (String.length v1))
        (v2, 0, (String.length v2))
  ;;
  
  (** Apply version comparator expression
    *)
  let rec comparator_apply v op =
    match op with
      | VGreater cversion ->
          (version_compare v cversion) > 0
      | VLesser cversion ->
          (version_compare v cversion) < 0
      | VEqual cversion ->
          (version_compare v cversion) = 0
      | VOr (op1, op2) ->
          (comparator_apply v op1) || (comparator_apply v op2)
      | VAnd (op1, op2) ->
          (comparator_apply v op1) && (comparator_apply v op2)
  ;;
  
end;;

module BaseCheck =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseCheck.ml"
  
  (** {1 Checking for particular features} 
    *)
  
  open BaseEnv;;
  
  (** Look for a program among a list of alternative program
    * the first found is returned. 
    *)
  let prog_best prg prg_lst =
    var_redefine
      prg 
      (lazy 
         (let alternate = 
            List.fold_left 
              (fun res e ->
                 match res with 
                   | Some _ -> 
                       res
                   | None ->
                       try
                         Some (BaseFileUtil.which e)
                       with Not_found ->
                         None)
              None
              prg_lst
          in
            match alternate with
              | Some prg -> prg
              | None -> raise Not_found))
  ;;
  
  (** Check the presence of a particular program.
    *)
  let prog prg =
    prog_best prg [prg]
  ;;
  
  (** Check the presence of a program or its native version
    *)
  let prog_opt prg = 
    prog_best prg [prg^".opt"; prg]
  ;;
  
  let ocamlfind = prog "ocamlfind";;
  
  (** Check version, following Sys.ocaml_version convention
    *)
  let version 
        var_prefix 
        (str_cmp, cmp, var_cmp) 
        fversion 
        () = 
    (* Really compare version provided *)
    let var = 
      var_prefix^"_version_"^var_cmp
    in
      var_redefine 
        ~hide:true 
        var
        (lazy
           (let version =
              match fversion () with 
                | "[Distributed with OCaml]" ->
                    begin
                      try 
                        (var_get "ocaml_version")
                      with Not_found ->
                        BaseMessage.warning 
                          "Variable ocaml_version not defined, fallback to default";
                        Sys.ocaml_version
                    end
                | res ->
                    res
            in
              prerr_endline version;
              if BaseVersion.comparator_apply version cmp then
                version
              else
                failwith 
                  (Printf.sprintf
                     "Cannot satisfy version constraint on %s: %s (version: %s)"
                     var_prefix
                     str_cmp
                     version)))
        ()
  ;;
  
  (** Check for findlib package
    *)
  let package ?version_comparator pkg () =
    let var =
      let buff = 
        Buffer.create ((String.length pkg) + 4)
      in
        Buffer.add_string buff "pkg_";
        String.iter
          (fun c ->
             if ('a' <= c && c <= 'z') 
               || 
                ('A' <= c && c <= 'Z') 
               || 
                ('0' <= c && c <= '9')
               ||
                c = '_' then
               Buffer.add_char buff c
             else
               Buffer.add_char buff '_')
          pkg;
        Buffer.contents buff
    in
    let findlib_dir pkg = 
      let dir = 
        BaseExec.run_read_one_line
          (ocamlfind ())
          ["query"; "-format"; "%d"; pkg]
      in
        if Sys.is_directory dir then
          dir
        else
          failwith
            (Printf.sprintf
               "When looking for findlib package %s, \
                directory %s return doesn't exist"
               pkg dir)
    in
    let findlib_version pkg =
      BaseExec.run_read_one_line 
        (ocamlfind ())
        ["query"; "-format"; "%v"; pkg]
    in
    let vl =
      var_redefine
        var
        (lazy (findlib_dir pkg))
        ()
    in
      (
        match version_comparator with 
          | Some ver_cmp ->
              var_ignore
                (version 
                   var
                   ver_cmp
                   (fun _ -> findlib_version pkg))
          | None -> 
              ()
      );
      vl
  ;;
  
  (** Run checks *)
  let run checks =
    List.iter
      (fun chk -> 
         let _s : string = 
           chk ()
         in 
           ())
      checks
  ;;
end;;

module BaseOCamlcConfig =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseOCamlcConfig.ml"
  
  (** Read output of command ocamlc -config and transform it
    * into enviornment variable
    *)
  
  open BaseEnv;;
  
  module SMap = Map.Make(String);;
  
  let ocamlc = BaseCheck.prog_opt "ocamlc";;
  
  let ocamlc_config_map =
    (* Map name to value for ocamlc -config output 
       (name ^": "^value) 
     *)
    let rec split_field mp lst = 
      match lst with 
        | line :: tl ->
            let mp =
              try
                let pos_semicolon =
                  String.index line ':'
                in
                  if pos_semicolon > 1 then            
                    (
                      let name =
                        String.sub line 0 pos_semicolon 
                      in
                      let linelen =
                        String.length line
                      in
                      let value =
                        if linelen > pos_semicolon + 2 then
                          String.sub 
                            line 
                            (pos_semicolon + 2) 
                            (linelen - pos_semicolon - 2)
                        else
                          ""
                      in
                        SMap.add name value mp
                    )
                  else
                    (
                      mp
                    )
              with Not_found ->
                (
                  mp
                )
            in
              split_field mp tl
        | [] ->
            mp
    in
  
      var_redefine
        "ocamlc_config_map"
        ~hide:true
        ~dump:false
        (lazy
           (var_protect
              (Marshal.to_string
                 (split_field 
                    SMap.empty
                    (BaseExec.run_read_output 
                       (ocamlc ()) ["-config"]))
                 [])))
  ;;
  
  let var_define nm =
    (* Extract data from ocamlc -config *)
    let avlbl_config_get () = 
      Marshal.from_string 
        (ocamlc_config_map ())
        0
    in
    let nm_config =
      match nm with 
        | "ocaml_version" -> "version"
        | _ -> nm
    in
      var_redefine
        nm 
        (lazy
          (try
              let map =
                avlbl_config_get ()
              in
              let value = 
                SMap.find nm_config map
              in
                value
            with Not_found ->
              failwith
                (Printf.sprintf 
                   "Cannot find field '%s' in '%s -config' output"
                   nm
                   (ocamlc ()))))
  ;;
  
end;;

module BaseStandardVar =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseStandardVar.ml"
  
  (** Most standard variables for OCaml 
      @author Sylvain Le Gall
    *)
  
  open BaseCheck;;
  open BaseEnv;;
  
  (** {2 Paths} *)
  
  (**/**)
  let p name hlp dflt = 
    var_define
      ~short_desc:hlp 
      ~cli:CLIAuto 
      ~arg_help:"dir" 
      name 
      (lazy dflt) 
  
  let (/) = Filename.concat
  
  (**/**)
  
  let prefix = 
    p "prefix"
      "install architecture-independent files dir"
      (match Sys.os_type with
         | "Win32" ->
             "%PROGRAMFILES%\\$pkg_name"
         | _ ->
             "/usr/local")
        
  let exec_prefix = 
    p "exec_prefix"
      "Install architecture-dependent files in dir"
      "$prefix"
  
  let bindir =
    p "bindir"
      "User executables"
      ("$exec_prefix"/"bin")
  
  let sbindir =
    p "sbindir"
      "System admin executables"
      ("$exec_prefix"/"sbin")
  
  let libexecdir =
    p "libexecdir"
      "Program executables"
      ("$exec_prefix"/"libexec")
  
  let sysconfdir =
    p "sysconfdir"
      "Read-only single-machine data"
      ("$prefix"/"etc")
  
  let sharedstatedir =
    p "sharedstatedir"
      "Modifiable architecture-independent data"
      ("$prefix"/"com")
  
  let localstatedir =
    p "localstatedir"
      "Modifiable single-machine data"
      ("$prefix"/"var")
  
  let libdir =
    p "libdir"
      "Object code libraries"
      ("$exec_prefix"/"lib")
  
  let datarootdir =
    p "datarootdir"
      "Read-only arch.-independent data root"
      ("$prefix"/"share")
  
  let datadir =
    p "datadir"
      "Read-only architecture-independent data"
      "$datarootdir"
  
  let infodir =
    p "infodir"
      "Info documentation"
      ("$datarootdir"/"info")
  
  let localedir =
    p "localedir"
      "Locale-dependent data"
      ("$datarootdir"/"locale")
  
  let mandir =
    p "mandir"
      "Man documentation"
      ("$datarootdir"/"man")
  
  let docdir =
    p "docdir"
      "Documentation root"
      ("$datarootdir"/"doc"/"$pkg_name")
  
  let htmldir =
    p "htmldir"
      "HTML documentation"
      "$docdir"
  
  let dvidir =
    p "dvidir"
      "DVI documentation"
      "$docdir"
  
  let pdfdir =
    p "pdfdir"
      "PDF documentation"
      "$docdir"
  
  let psdir =
    p "psdir"
      "PS documentation"
      "$docdir"
  
  (** {2 Programs} *)
  
  let ocamlfind  = BaseCheck.ocamlfind
  let ocamlc     = BaseOCamlcConfig.ocamlc
  let ocamlopt   = prog_opt "ocamlopt"
  let ocamlbuild = prog "ocamlbuild"
  
  
  (** {2 OCaml config variable} *) 
  
  let c = BaseOCamlcConfig.var_define 
  
  let ocaml_version            = c "version"
  let standard_library_default = c "standard_library_default"
  let standard_library         = c "standard_library"
  let standard_runtime         = c "standard_runtime"
  let ccomp_type               = c "ccomp_type"
  let bytecomp_c_compiler      = c "bytecomp_c_compiler"
  let native_c_compiler        = c "native_c_compiler"
  let architecture             = c "architecture"
  let model                    = c "model"
  let system                   = c "system"
  let ext_obj                  = c "ext_obj"
  let ext_asm                  = c "ext_asm"
  let ext_lib                  = c "ext_lib"
  let ext_dll                  = c "ext_dll"
  let os_type                  = c "os_type"
  let default_executable_name  = c "default_executable_name"
  let systhread_supported      = c "systhread_supported"
  
  (** {2 ...} *)
  
  (** Check what is the best target for platform (opt/byte)
    *)
  let ocamlbest =
    var_define
      "ocamlbest"
      (lazy 
         (try
            let _s : string = 
              ocamlopt ()
            in
              "native"
          with Not_found ->
            let _s : string = 
              ocamlc ()
            in
             "byte"))
  
  (** Compute the default suffix for program (target OS dependent)
    *)
  let suffix_program =
    var_define
      "suffix_program"
      (lazy
         (match os_type () with 
            | "Win32" -> ".exe" 
            | _ -> ""
         ))
  
  (** {2 Variables from OASIS package} 
    *)
  
  (**/**)
  let rpkg = 
    ref None
  
  let pkg_get () =
    match !rpkg with 
      | Some pkg -> pkg
      | None -> 
          failwith 
            "OASIS Package is not set"
  (**/**)
  
  let pkg_name = 
    var_define
      ~short_desc:"Package name"
      "pkg_name"
      (lazy (fst (pkg_get ())))
  
  let pkg_version =
    var_define
      ~short_desc:"Package version"
      "pkg_version"
      (lazy (snd (pkg_get ())))
  
  (** Initialize some variables 
    *)
  let init pkg = 
    rpkg := Some pkg
  
end;;

module BaseFileAB =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseFileAB.ml"
  
  (** File .ab which content will be replaced by environment variable
     
      This is the same kind of file as .in file for autoconf, except we
      use the variable definition of ${!Buffer.add_substitute}. This is 
      the default file to be generated by configure step (even for 
      autoconf, except that it produce a master file before).
  
      @author Sylvain Le Gall
    *)
  
  open BaseEnv;;
  
  let to_filename fn =
    if not (Filename.check_suffix fn ".ab") then
      BaseMessage.warning 
        (Printf.sprintf 
           "File '%s' doesn't have '.ab' extension"
           fn);
    Filename.chop_extension fn
  ;;
  
  (** Replace variable in file %.ab to generate %
    *)
  let replace fn_lst =
    let buff =
      Buffer.create 13
    in
      List.iter
        (fun fn ->
           let chn_in =
             open_in fn
           in
           let chn_out =
             open_out (to_filename fn)
           in
             (
               try
                 while true do
                  Buffer.add_string buff (var_expand (input_line chn_in));
                  Buffer.add_char buff '\n'
                 done
               with End_of_file ->
                 ()
             );
             Buffer.output_buffer chn_out buff;
             Buffer.clear buff;
             close_in chn_in;
             close_out chn_out)
        fn_lst
  ;;
end;;

module BaseLog =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseLog.ml"
  
  (** Maintain a DB of what has been done
      @author Sylvain Le Gall
    *)
  
  open BaseUtils;;
  
  let default_filename =
    Filename.concat 
      (Filename.dirname BaseEnv.default_filename)
      "setup.log"
  ;;
  
  let load () = 
    if Sys.file_exists default_filename then
      (
        let chn = 
          open_in default_filename
        in
        let rec read_aux acc =
          try 
            (
              read_aux 
                (Scanf.fscanf chn "%S %S\n" 
                   (fun e d ->  (e, d) :: acc))
            )
          with End_of_file ->
            (
              close_in chn;
              List.rev acc
            )
        in
          read_aux []
      )
    else
      (
        []
      )
  ;;
  
  let register event data =
    let chn_out =
      open_out_gen [Open_append; Open_creat; Open_text] 0o644 default_filename
    in
      Printf.fprintf chn_out "%S %S\n" event data;
      close_out chn_out
  ;;
  
  let unregister event data =
    let lst = 
      load ()
    in
    let chn_out =
      open_out default_filename
    in
      List.iter 
        (fun (e, d) ->
           if e <> event || d <> data then
             Printf.fprintf chn_out "%S %S\n" event data)
        lst;
      close_out chn_out
  ;;
  
  let filter events =
    let st_events =
      List.fold_left
        (fun st e -> 
           SetString.add e st)
        SetString.empty
        events
    in
      List.filter 
        (fun (e, _) -> SetString.mem e st_events)
        (load ())
  ;;
end;;

module BaseTest =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseTest.ml"
  
  (** Run test
      @author Sylvain Le Gall
    *)
  
  open BaseExpr;;
  open BaseEnv;;
  
  type t = 
      {
        test_name:               string;
        test_command:            string * string list;
        test_working_directory:  string option;
        test_run:                bool choices;
        test_plugin:             string -> string list -> float;
      }
  ;;
  
  let test lst extra_args =
    let one_test t =
      if BaseExpr.choose t.test_run then
        begin
          let () = 
            BaseMessage.info 
              (Printf.sprintf "Running test '%s'" t.test_name)
          in
          let cwd = 
            Sys.getcwd ()
          in
          let back_cwd () = 
            BaseMessage.info 
              (Printf.sprintf "Changing directory to '%s'" cwd);
            Sys.chdir cwd
          in
            try 
              let () = 
                match t.test_working_directory with 
                  | Some dir -> 
                      BaseMessage.info 
                        (Printf.sprintf "Changing directory to '%s'" dir);
                      Sys.chdir dir
                  | None -> ()
              in
              let cmd, args = 
                t.test_command
              in
              let failure_percent =
                t.test_plugin
                  (var_expand cmd)
                  (List.map 
                     var_expand
                     (args @ (Array.to_list extra_args)))
              in
                back_cwd ();
                failure_percent
            with e ->
              begin
                back_cwd ();
                raise e
              end
        end
      else
        begin
          BaseMessage.info 
            (Printf.sprintf "Skipping test '%s'" t.test_name);
          0.0
        end
    in
    let res =
      List.map
        one_test
        lst
    in
    let n = 
      float_of_int (List.length res)
    in
    let failure_percent =
      List.fold_left
        (fun r e -> r +. (e /. n))
        0.0
        res
    in
      (if failure_percent > 0.0 then
         BaseMessage.warning 
       else
         BaseMessage.info)
        (Printf.sprintf 
           "Tests had a %.2f%% failure rate"
           (100. *. failure_percent))
  ;;
  
end;;

module BaseSetup =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/base/BaseSetup.ml"
  
  (** Entry point for ocaml-autobuild
      @author Sylvain Le Gall
    *)
  
  open BaseEnv;;
  open BaseExpr;;
  
  type action_fun = string array -> unit;;
  
  type flag = 
      {
        flag_description:  string option;
        flag_default:      bool choices;
      }
  ;;
  
  type package =
      {
        name:     string;
        version:  string;
        files_ab: string list;
        flags:    (string * flag) list;
      }
  ;;
  
  type t =
      {
        configure:       action_fun;
        build:           action_fun;
        doc:             action_fun list;
        test:            BaseTest.t list;
        install:         action_fun;
        uninstall:       action_fun;
        (* TODO: use lists *)
        clean:           unit -> unit;
        distclean:       unit -> unit;
        package:         package;
      }
  ;;
  
  let distclean t =
    (* Call clean *)
    t.clean ();
  
    (* Remove generated file *)
    List.iter
      (fun fn ->
         if Sys.file_exists fn then
           (BaseMessage.info 
              (Printf.sprintf "Remove '%s'" fn);
            Sys.remove fn))
      (BaseEnv.default_filename 
       :: 
       BaseLog.default_filename
       ::
       (List.rev_map BaseFileAB.to_filename t.package.files_ab));
    t.distclean ()
  ;;
  
  let configure t args = 
    (* Run configure *)
    t.configure args;
  
    (* Replace data in file *)
    BaseFileAB.replace t.package.files_ab
  ;;
  
  let setup t = 
    try
      let act =
        ref (fun _ -> 
               failwith
                 (Printf.sprintf
                    "No action defined, run '%s %s -help'"
                    Sys.executable_name
                    Sys.argv.(0)))
  
      in
      let extra_args =
        ref []
      in
      let allow_empty_env = 
        ref false
      in
      let arg_rest ?(configure=false) lst =
        Arg.Tuple
          [
            Arg.Rest (fun str -> extra_args := str :: !extra_args);
            Arg.Unit 
              (fun () ->
                 allow_empty_env := configure;
                 act :=
                 (let args =
                    !extra_args 
                  in
                    fun () ->
                      List.iter
                        (fun f -> 
                           f (Array.of_list (List.rev args)))
                        lst);
                 extra_args := []); 
          ]
      in
      let arg_clean a =
        Arg.Unit 
          (fun () -> 
             allow_empty_env := true; 
             act := (fun () -> a ()));
      in
  
        Arg.parse 
          [
            "-configure",
            arg_rest ~configure:true [configure t],
            "[options*] Configure build process.";
  
            "-build",
            arg_rest [t.build],
            "[options*] Run build process.";
  
            "-doc",
            arg_rest t.doc,
            "[options*] Build documentation.";
  
            "-test",
            arg_rest [BaseTest.test t.test],
            "[options*] Build and run tests.";
  
            "-install",
            arg_rest [t.install],
            "[options*] Install library, data, executable and documentation.";
  
            "-uninstall",
            arg_rest [t.uninstall],
            "[options*] Uninstall library, data, executable and documentation.";
  
            "-clean",
            arg_clean t.clean,
            "[options*] Clean build environment.";
  
            "-distclean",
            arg_clean (fun () -> distclean t),
            "[options*] Clean build and configure environment.";
          ]
          (fun str -> failwith ("Don't know what to do with "^str))
          "Setup and run build process current package\n";
  
        (* Build initial environment *)
        load ~allow_empty:!allow_empty_env ();
  
        (** Initialize flags *)
        List.iter 
          (fun (nm, {flag_description = hlp; flag_default = choices}) ->
             let apply ?short_desc () = 
               var_ignore
                 (var_define
                    ~cli:CLIAuto
                    ?short_desc
                    nm
                    (lazy (string_of_bool (choose choices))))
             in
               match hlp with 
                 | Some hlp ->
                     apply ~short_desc:hlp ()
                 | None ->
                     apply ())
          t.package.flags;
  
        BaseStandardVar.init (t.package.name, t.package.version);
  
        !act ()
  
    with e ->
      BaseMessage.error (Printexc.to_string e);
  ;;
  
end;;


# 2390 "setup.ml"
module InternalConfigure =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/internal/InternalConfigure.ml"
  
  (** Configure using ocaml-autobuild internal scheme
      @author Sylvain Le Gall
    *)
  
  open BaseEnv;;
  open BaseExpr;;
  
  (** Configure build using provided series of check to be done
    * and then output corresponding file.
    *)
  let configure cond_checks argv =
  
    (* Parse command line *)
    BaseArgExt.parse argv (args ());
  
    (* Do some check *)
    List.iter
      (fun (cond, checks) ->
         if BaseExpr.choose cond then
           BaseCheck.run checks)
      cond_checks;
  
    dump ();
    print ()
  ;;
  
end;;

module InternalInstall =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/internal/InternalInstall.ml"
  
  (** Install using ocaml-autobuild internal scheme
      @author Sylvain Le Gall
    *)
  
  open BaseEnv;;
  open BaseStandardVar;;
  
  type comp_type =
    | Byte
    | Native
    | Best 
  ;;
  
  type library =
      {
        lib_modules:         string list;
        lib_extra:           string list;
  
        lib_name:            string;
        lib_path:            string;
        lib_install:         bool BaseExpr.choices;
        lib_c_sources:       bool;
        lib_compiled_object: comp_type;
        lib_data_files:      (string * string option) list;
      }
  ;;
  
  type executable =
      {
        exec_filename:        string;
        exec_custom:          bool;
  
        exec_name:            string;
        exec_path:            string;
        exec_install:         bool BaseExpr.choices;
        exec_c_sources:       bool;
        exec_compiled_object: comp_type; 
        exec_data_files:      (string * string option) list;
      }
  ;;
  
  let srcdir =
    var_define
      "srcdir"
      (lazy ".")
  ;;
  
  let builddir =
    var_define
      "builddir"
      (lazy (Filename.concat (srcdir ()) "_build"))
  ;;
  
  let dllfn path name = 
    Filename.concat path ("dll"^name^(ext_dll ()))
  ;;
  
  let libfn path name =
    Filename.concat path ("lib"^name^(ext_lib ()))
  ;;
  
  let exec_hook =
    ref (fun exec -> exec)
  ;;
  
  let lib_hook =
    ref (fun lib -> lib)
  ;;
  
  let install_file_ev = 
    "install-file"
  ;;
  
  let install_dir_ev =
    "install-dir"
  ;;
  
  let install_findlib_ev =
    "install-findlib"
  ;;
  
  let install libs execs argv =
    
    let rootdirs =
      [srcdir (); builddir ()]
    in
  
    let ( * ) lst1 lst2 = 
      List.flatten 
        (List.map 
           (fun a -> 
              List.map 
                (fun b -> a,b) 
                lst2) 
           lst1)
    in
  
    let make_filename =
      function
        | [] -> "" 
        | hd :: tl  -> List.fold_left Filename.concat hd tl
    in
  
    let make_module nm = 
      [String.capitalize nm; String.uncapitalize nm]
    in
  
    let find_file f lst = 
      let alternatives =
        List.map (fun e -> make_filename (f e)) lst
      in
        try 
          List.find Sys.file_exists alternatives
        with Not_found ->
          failwith 
            (Printf.sprintf 
               "Cannot find any of the files: %s"
               (String.concat ", " 
                  (List.map 
                     (Printf.sprintf "%S")
                     alternatives)))
    in
  
    let find_build_file fn =
      find_file
        (fun rootdir -> [rootdir; fn])
        rootdirs
    in
  
    let is_native comp_type =
      match comp_type with 
        | Best -> (ocamlbest ()) = "native" 
        | Byte -> false
        | Native -> true
    in
  
    let install_file src_file envdir = 
      let tgt_dir = 
        envdir ()
      in
      let tgt_file =
        Filename.concat 
          tgt_dir
          (Filename.basename src_file)
      in
        (* Check that target directory exist *)
        if not (Sys.file_exists tgt_dir) then
          (
            BaseMessage.info 
              (Printf.sprintf 
                 "Creating directory '%s'"
                 tgt_dir);
            BaseFileUtil.mkdir tgt_dir;
            BaseLog.register install_dir_ev tgt_dir
          );
  
        (* Really install files *)
        BaseMessage.info 
          (Printf.sprintf 
             "Copying file '%s' to '%s'"
             src_file
             tgt_file);
        BaseFileUtil.cp src_file tgt_file;
        BaseLog.register install_file_ev tgt_file
    in
  
    let install_data path files_targets = 
      List.iter
        (fun (src, tgt_opt) ->
           let real_srcs = 
             let real_src = 
               Filename.concat path src
             in
             (* Glob the src expression *)
             let filename = 
               Filename.basename real_src
             in
               if String.contains filename '*' then
                 (
                   let ext = 
                     match BaseUtils.split '.' filename with 
                       | [a; b] when a = "*" -> 
                           "."^b
                       | _ ->
                           failwith 
                             (Printf.sprintf 
                                "Invalid file wildcard in '%s'"
                                src)
                   in
                   let ext_len =
                     String.length ext
                   in
                   let dirname =
                     Filename.dirname real_src
                   in
                   let res =
                     Array.fold_left
                       (fun acc fn ->
                          try 
                            let fn_ext = 
                              String.sub 
                                fn 
                                ((String.length fn) - ext_len) 
                                ext_len
                            in
                              if fn_ext = ext then
                                (Filename.concat dirname fn) :: acc
                              else
                                acc
                          with Invalid_argument "String.sub" ->
                            acc)
                       []
                       (Sys.readdir dirname)
                   in
                     if res = [] then
                       failwith 
                         (Printf.sprintf 
                            "Wildcard '%s' doesn't match any files"
                            src);
                     res
                 )
               else
                 (
                   [real_src]
                 )
           in
             List.iter 
               (fun fn -> 
                  install_file 
                    fn 
                    (fun () -> 
                       match tgt_opt with 
                         | Some s -> var_expand s
                         | None -> var_expand "$datarootdir/$pkg_name")) 
               real_srcs)
             
        files_targets
    in
  
    let install_lib lib = 
      let lib =
        !lib_hook lib
      in
      let install =
        BaseExpr.choose lib.lib_install
      in
        if install then
          (
            let find_lib_file fn =
              find_file
                (fun rootdir -> [rootdir; lib.lib_path; fn])
                rootdirs
            in
  
            let module_to_cmi modul =
              find_file 
                 (fun (rootdir, fn) -> [rootdir; lib.lib_path; (fn^".cmi")])
                 (rootdirs * (make_module modul))
            in
  
            let module_to_header modul =
              assert(modul <> "");
              find_file 
                 (fun ((rootdir, fn), ext) -> [rootdir; lib.lib_path; fn^ext])
                 (rootdirs * (make_module modul) * [".mli"; ".ml"])
            in
              
            let files =
              List.flatten
                (
                  [
                    find_lib_file "META";
                    find_lib_file (lib.lib_name^".cma");
                  ]
                  :: 
                  (if is_native lib.lib_compiled_object then
                     (
                       try 
                         [
                           find_lib_file (lib.lib_name^".cmxa");
                           find_lib_file (lib.lib_name^(ext_lib ()));
                         ]
                       with Failure txt ->
                         BaseMessage.warning 
                           (Printf.sprintf
                              "Cannot install native library %s: %s"
                              lib.lib_name
                              txt);
                         []
                     )
                   else
                     []
                  )
                  ::
                  lib.lib_extra
                  ::
                  (if lib.lib_c_sources then
                     [
                       find_build_file (libfn lib.lib_path lib.lib_name);
                     ]
                   else
                     [])
                  ::
                  (* Some architecture doesn't allow shared library (Cygwin, AIX) *)
                  (if lib.lib_c_sources then
                     (try 
                       [
                         find_build_file (dllfn lib.lib_path lib.lib_name);
                       ]
                      with Failure txt ->
                        if (os_type ()) <> "Cygwin" then
                          BaseMessage.warning
                            (Printf.sprintf
                               "Cannot install C static library %s: %s"
                               lib.lib_name
                               txt);
                        [])
                   else
                     [])
                  ::
                  (
                    List.rev_map
                      (fun modul -> [module_to_cmi modul; module_to_header modul])
                      lib.lib_modules
                  )
                )
            in
              BaseMessage.info 
                (Printf.sprintf
                   "Installing findlib library '%s'"
                   lib.lib_name);
              BaseExec.run "ocamlfind" ("install" :: lib.lib_name :: files);
              BaseLog.register install_findlib_ev lib.lib_name;
              install_data lib.lib_path lib.lib_data_files;
          )
    in
  
    let install_exec exec =
      let exec =
        !exec_hook exec
      in
        if BaseExpr.choose exec.exec_install then
          (
            let () = 
              install_file
                (find_build_file
                   (exec.exec_filename^(suffix_program ())))
                bindir;
              install_data exec.exec_path exec.exec_data_files 
            in
              if exec.exec_c_sources && 
                 not exec.exec_custom && 
                 not (is_native exec.exec_compiled_object) then
                (
                  install_file
                    (find_build_file
                       (dllfn exec.exec_path exec.exec_name))
                    libdir
                )
              else
                ()
          )
    in
  
      List.iter install_lib libs;
      List.iter install_exec execs
  ;;
  
  (* Uninstall already installed data *)
  let uninstall argv =
    List.iter 
      (fun (ev, data) ->
         if ev = install_file_ev then
           (
             if Sys.file_exists data then
               (
                 BaseMessage.info
                   (Printf.sprintf 
                      "Removing file '%s'"
                      data);
                 Sys.remove data
               )
           )
         else if ev = install_dir_ev then
           (
             if Sys.file_exists data && Sys.is_directory data then
               (
                 if Sys.readdir data = [||] then
                   (
                     BaseMessage.info
                       (Printf.sprintf 
                          "Removing directory '%s'"
                          data);
                     BaseFileUtil.rmdir data
                   )
                 else
                   (
                     BaseMessage.warning 
                       (Printf.sprintf
                          "Directory '%s' is not empty (%s)"
                          data
                          (String.concat 
                             ", " 
                             (Array.to_list 
                                (Sys.readdir data))))
                   )
               )
           )
         else if ev = install_findlib_ev then
           (
             BaseMessage.info
               (Printf.sprintf
                  "Removing findlib library '%s'"
                  data);
             BaseExec.run (ocamlfind ()) ["remove"; data]
           )
         else
           (
             failwith (Printf.sprintf "Unknown log event '%s'" ev)
           );
         BaseLog.unregister ev data)
      (* We process event in reverse order *)
      (List.rev 
         (BaseLog.filter 
            [install_file_ev; 
             install_dir_ev;
             install_findlib_ev;]))
  ;;
  
end;;


# 2859 "setup.ml"
module OCamlbuildBuild =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/ocamlbuild/OCamlbuildBuild.ml"
  
  (** Runtime support for autobuild/OCamlbuild
      @author Sylvain Le Gall
    *)
  
  open BaseStandardVar;;
  
  type target =
    | Std of string
    | CLibrary  of string * string
    | Rename of string * string
  ;;
  
  type t = 
    (target BaseExpr.choices) list
  ;;
  
  let cond_targets_hook =
    ref (fun lst -> lst)
  ;;
  
  let build cond_targets argv =
    (* Fix special arguments depending on environment *)
    let env_args =
      List.flatten
        [
          if (os_type ()) = "Win32" then
            [
              "-classic-display"; 
              "-no-log"; 
              "-install-lib-dir"; 
              (Filename.concat (standard_library ()) "ocamlbuild")
            ] 
          else
            [];
      
          if (ocamlbest ()) = "byte" || (os_type ()) = "Win32" then
            [
              "-byte-plugin" 
            ]
          else
            [];
        ]
    in
  
    let ocamlbuild_run rtargets = 
      let args = 
        List.rev_append rtargets (Array.to_list argv)
      in
        BaseExec.run (ocamlbuild ()) (env_args @ args)
    in
  
    let in_build_dir fn =
      Filename.concat "_build" fn
    in
  
    let last_rtargets =
      List.fold_left
        (fun acc (choices, tgt) ->
           if BaseExpr.choose choices then 
             match tgt with 
               | Std nm -> 
                   nm :: acc
               | CLibrary (dir, nm) ->
                   (dir^"/lib"^nm^(ext_lib ())) 
                   ::
                   (dir^"/dll"^nm^(ext_dll ()))
                   ::
                   acc
               | Rename (src, tgt) ->
                   ocamlbuild_run (src :: acc);
                   BaseFileUtil.cp 
                     (in_build_dir src) 
                     (in_build_dir tgt);
                   []
           else
             acc)
        []
        (!cond_targets_hook cond_targets)
    in
      if last_rtargets <> [] then
        ocamlbuild_run last_rtargets
  ;;
  
  let clean () = 
    (* TODO use ocamlbuild *)
    BaseExec.run "ocamlbuild" ["-clean"]
  ;;
  
end;;


# 2955 "setup.ml"
module CustomUtils =
struct
# 1 "/home/gildor/programmation/ocaml-autobuild/src/custom/CustomUtils.ml"
  
  (** Utilities for Custom plugin.
      @author Sylvain Le Gall
    *)
  
  open BaseEnv;;
  
  let run_and_replace cmd args extra_args =
    BaseExec.run 
      (var_expand cmd)
      (List.map 
         var_expand
         (args @ (Array.to_list extra_args)))
  ;;
  
  let run_and_replace_test cmd args =
    try
      BaseExec.run cmd args;
      0.0
    with Failure _ ->
      1.0
  ;;
  
end;;


# 2985 "setup.ml"
let setup () =
  BaseSetup.setup
    {
      BaseSetup.configure =
        (InternalConfigure.configure
           [
             ([(BaseExpr.Bool true, true)], [BaseCheck.prog "ocamlbuild"]);
             ([(BaseExpr.Bool true, true)],
               [
                 BaseCheck.package
                   ~version_comparator:(">= 1.6.10",
                                         BaseVersion.VOr
                                           (BaseVersion.VGreater "1.6.10",
                                             BaseVersion.VEqual "1.6.10"),
                                         "ge_1_6_10")
                   "type-conv";
                 BaseCheck.package "camlp4.lib";
                 BaseCheck.package "camlp4.quotations.o"
               ]);
             ([(BaseExpr.Bool true, true)], []);
             ([(BaseExpr.Bool true, true)],
               [BaseCheck.package "oUnit"; BaseCheck.package "fileutils"]);
             ([(BaseExpr.Bool true, true)],
               [
                 BaseCheck.version
                   "ocaml"
                   (">= 3.11.1",
                     BaseVersion.VOr
                       (BaseVersion.VGreater "3.11.1",
                         BaseVersion.VEqual "3.11.1"),
                     "ge_3_11_1")
                   BaseStandardVar.ocaml_version
               ])
           ]);
      BaseSetup.build =
        (OCamlbuildBuild.build
           [
             ([(BaseExpr.Bool true, true)], OCamlbuildBuild.Std "src/odn.cma");
             ([
                (BaseExpr.Bool true, true);
                (BaseExpr.Test ("ocamlbest", "byte"), false)
              ],
               OCamlbuildBuild.Std "src/odn.cmxa");
             ([(BaseExpr.Bool true, true)],
               OCamlbuildBuild.Std "src/pa_odn.cma");
             ([
                (BaseExpr.Bool true, true);
                (BaseExpr.Test ("ocamlbest", "byte"), false)
              ],
               OCamlbuildBuild.Std "src/pa_odn.cmxa");
             ([(BaseExpr.Bool true, true)],
               OCamlbuildBuild.Rename ("tests/test.byte", "tests/test"))
           ]);
      BaseSetup.test =
        ([
           {
             BaseTest.test_name = ("main");
             BaseTest.test_command = (("_build/tests/test", []));
             BaseTest.test_working_directory = (None);
             BaseTest.test_run = ([(BaseExpr.Bool true, true)]);
             BaseTest.test_plugin = (CustomUtils.run_and_replace_test);
             }
         ]);
      BaseSetup.doc = ([]);
      BaseSetup.install =
        (InternalInstall.install
           [
             {
               InternalInstall.lib_name = ("pa_odn");
               InternalInstall.lib_install =
                 ([(BaseExpr.Bool true, true); (BaseExpr.Bool true, true)]);
               InternalInstall.lib_modules = (["Pa_odn"]);
               InternalInstall.lib_path = ("src");
               InternalInstall.lib_extra = ([]);
               InternalInstall.lib_c_sources = (false);
               InternalInstall.lib_compiled_object = (InternalInstall.Best);
               InternalInstall.lib_data_files = ([]);
               };
             {
               InternalInstall.lib_name = ("odn");
               InternalInstall.lib_install =
                 ([(BaseExpr.Bool true, true); (BaseExpr.Bool true, true)]);
               InternalInstall.lib_modules = (["ODN"]);
               InternalInstall.lib_path = ("src");
               InternalInstall.lib_extra = ([]);
               InternalInstall.lib_c_sources = (false);
               InternalInstall.lib_compiled_object = (InternalInstall.Best);
               InternalInstall.lib_data_files = ([]);
               }
           ]
           [
             {
               InternalInstall.exec_path = ("tests");
               InternalInstall.exec_name = ("test");
               InternalInstall.exec_filename = ("tests/test");
               InternalInstall.exec_install =
                 ([(BaseExpr.Bool true, true); (BaseExpr.Bool true, false)]);
               InternalInstall.exec_c_sources = (false);
               InternalInstall.exec_custom = (false);
               InternalInstall.exec_compiled_object = (InternalInstall.Byte);
               InternalInstall.exec_data_files = ([]);
               }
           ]);
      BaseSetup.uninstall = (InternalInstall.uninstall);
      BaseSetup.clean = (fun () -> OCamlbuildBuild.clean ());
      BaseSetup.distclean = (fun () -> ());
      BaseSetup.package =
        ({
           BaseSetup.name = ("ocaml-data-notation");
           BaseSetup.version = ("0.0.1");
           BaseSetup.files_ab = ([]);
           BaseSetup.flags = ([]);
           });
      }
  ;;
(* AUTOBUILD_STOP *)
setup ();;
