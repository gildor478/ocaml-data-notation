#!/usr/bin/ocamlrun ocaml
(* OASIS_START *)
(* DO NOT EDIT (digest: b13fb436e2a81b1994c7ce3a0199514c) *)
module CommonGettext = struct
# 0 "/home/gildor/programmation/oasis/src/common/CommonGettext.ml"
  
  (** Gettext interface
    *)
  
  let s_ str = 
    str
  
  let f_ (str : ('a, 'b, 'c) format) =
    str
end

module PropList = struct
# 0 "/home/gildor/programmation/oasis/src/common/PropList.ml"
  
  (** Property list 
      @author Sylvain Le Gall
    *)
  
  open CommonGettext
  
  type name_t = string
  
  let no_context f =
    fun ?context s -> f s
  
  exception Not_set of name_t
  exception No_printer of name_t
  exception Unknown_field of name_t * name_t
  
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
  
  module Data =
  struct
  
    type t = 
        (name_t, unit -> unit) Hashtbl.t
  
    let create () =
      Hashtbl.create 13
  
# 40 "/home/gildor/programmation/oasis/src/common/PropList.ml"
  end
  
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
  
  module FieldRO =
  struct
  
    let create ?schema ?name ?parse ?print ?default ?update ?help extra =
      let fld = 
        Field.create ?schema ?name ?parse ?print ?default ?update ?help extra
      in
        fun data -> Field.fget data fld
  
  end
end


# 324 "setup.ml"
module OASISTypes = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISTypes.ml"
  
  (** OASIS types and exceptions
     @author Sylvain Le Gall
    *)
  
  
  
  (** Alias type
    *)
  type name         = string 
  type package_name = string 
  type url          = string 
  type dirname      = string 
  type filename     = string 
  type prog         = string 
  
  (* Package name for findlib, doesn't contain '.' *)
  type findlib_name = string 
  (* Package path, made of several findlib name concatenated with '.' *)
  type findlib_path = string 
  
  (** Version 
    *)
  type version =
    | VInt of int * version
    | VNonInt of string * version
    | VEnd
    
  
  (** Version comparator
    *)
  type version_comparator = 
    | VGreater of version
    | VGreaterEqual of version
    | VEqual of version
    | VLesser of version
    | VLesserEqual of version
    | VOr of  version_comparator * version_comparator
    | VAnd of version_comparator * version_comparator
    
  
  (** Valid licenses
    *)
  type license =
    | AllRightsReserved
    | BSD3
    | BSD4
    | GPL
    | LGPL
    | LGPL_link_exn
    | PublicDomain
    | OtherLicense of url
    
  
  (** Compilation type
    *)
  type compiled_object =
    | Byte
    | Native
    | Best
    
  
  (** Package dependency
    *)
  type dependency = 
    | FindlibPackage of findlib_path * version_comparator option
    | InternalLibrary of name
    
  
  (** Tool dependency
    *)
  type tool =
    | ExternalTool of name
    | InternalExecutable of name 
    
  
  (** Possible VCS 
    *)
  type vcs = 
    | Darcs 
    | Git 
    | Svn 
    | Cvs 
    | Hg 
    | Bzr 
    | Arch 
    | Monotone
    | OtherVCS of url
    
  
  (** Available test 
    *)
  type expr_test = 
    | TOs_type
    | TSystem
    | TArchitecture
    | TCcomp_type
    | TOCaml_version
    
  
  (** Boolean expression to express condition on values
    *)
  type expr =
    | EBool of bool
    | ENot of expr
    | EAnd of expr * expr
    | EOr of expr * expr
    | EFlag of string
    | ETest of expr_test * string
    
  
  (** Conditional value
    *)
  type 'a conditional = (expr * 'a) list 
  
  type common_section =
      {
        cs_name: name;
        cs_data: PropList.Data.t;
      }
      
  
  type build_section =
      {
        bs_build:           bool conditional;
        bs_install:         bool conditional;
        bs_path:            dirname;
        bs_compiled_object: compiled_object;
        bs_build_depends:   dependency list;
        bs_build_tools:     tool list;
        bs_c_sources:       filename list;
        bs_data_files:      (filename * filename option) list;
      }
      
  
  (** Library definition 
    *)
  type library = 
      {
        lib_modules:            string list;
        lib_findlib_parent:     findlib_name option;
        lib_findlib_name:       findlib_name option;
        lib_findlib_containers: findlib_name list;
      } 
  
  (** Executable definition 
    *)
  type executable = 
      {
        exec_custom:          bool;
        exec_main_is:         filename;
      } 
  
  (** Command line flag defintion 
    *)
  type flag = 
      {
        flag_description:  string option;
        flag_default:      bool conditional;
      } 
  
  (** Source repository definition
    *)
  type source_repository = 
      {
        src_repo_type:        vcs;
        src_repo_location:    url;
        src_repo_browser:     url option;
        src_repo_module:      string option;
        src_repo_branch:      string option;
        src_repo_tag:         string option;
        src_repo_subdir:      filename option;
      } 
  
  (** Test definition
    *)
  type test = 
      {
        test_type:               string;
        test_command:            string * string list;
        test_working_directory:  filename option;
        test_run:                bool conditional;
        test_build_tools:        tool list;
      } 
  
  type section =
    | Library    of common_section * build_section * library
    | Executable of common_section * build_section * executable
    | Flag       of common_section * flag
    | SrcRepo    of common_section * source_repository
    | Test       of common_section * test
    
  
  (** OASIS file whole content
    *)
  type package = 
      {
        oasis_version:   version;
        ocaml_version:   version_comparator option;
        findlib_version: version_comparator option;
        name:            package_name;
        version:         version;
        license:         license;
        license_file:    filename option;
        copyrights:      string list;
        maintainers:     string list;
        authors:         string list;
        homepage:        url option;
        synopsis:        string;
        description:     string option;
        categories:      url list;
        conf_type:       string;
        build_type:      string;
        install_type:    string;
        files_ab:        filename list;
        sections:        section list;
        plugins:         string list;
        schema_data:     PropList.Data.t;
      } 
  
end

module OASISVersion = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISVersion.ml"
  
  (** Version comparisons
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  open CommonGettext
  
  (** Compare versions
    *)
  let rec version_compare v1 v2 =
    compare v1 v2
  
  (** Convert a string into a version
    *)
  let version_of_string str =
    let is_digit c =
      '0' <= c && c <= '9'
    in
  
    let str_len =
      String.length str
    in
  
    let buff =
      Buffer.create str_len
    in
  
    let rec extract_filter test start = 
      if start < str_len && test str.[start] then
        (
          Buffer.add_char buff str.[start];
          extract_filter test (start + 1)
        )
      else
        (
          let res =
            Buffer.contents buff
          in
            Buffer.clear buff;
            res, start
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
  
    let rec parse_aux pos =
      if pos < str_len then
        begin
          if is_digit str.[pos] then
            begin
              let vl, end_pos =
                extract_int pos
              in
                VInt (vl, parse_aux end_pos)
            end
          else
            begin
              let vl, end_pos =
                extract_non_int pos
              in
                VNonInt (vl, parse_aux end_pos)
            end
        end
      else
        VEnd 
    in
  
    let rec compress =
      function
        | VInt (i, VNonInt(".", (VInt _ as tl))) ->
            VInt (i, compress tl)
        | VInt (i, tl) ->
            VInt (i, compress tl)
        | VNonInt (i, tl) ->
            VNonInt (i, compress tl)
        | VEnd ->
            VEnd
    in
  
      compress (parse_aux 0)
  
  (** Convert a version to a string
    *)
  let rec string_of_version =
    function
      | VInt (i, (VInt _ as tl)) ->
          (string_of_int i)^"."^(string_of_version tl)
      | VInt (i, tl) -> 
          (string_of_int i)^(string_of_version tl)
      | VNonInt (s, tl) -> 
          s^(string_of_version tl)
      | VEnd -> ""
  
  (** Apply version comparator expression
    *)
  let rec comparator_apply v op =
    match op with
      | VGreater cv ->
          (version_compare v cv) > 0
      | VGreaterEqual cv ->
          (version_compare v cv) >= 0
      | VLesser cv ->
          (version_compare v cv) < 0
      | VLesserEqual cv ->
          (version_compare v cv) <= 0
      | VEqual cv ->
          (version_compare v cv) = 0
      | VOr (op1, op2) ->
          (comparator_apply v op1) || (comparator_apply v op2)
      | VAnd (op1, op2) ->
          (comparator_apply v op1) && (comparator_apply v op2)
  
  (** Convert a comparator to string 
    *)
  let rec string_of_comparator =
    function 
      | VGreater v  -> "> "^(string_of_version v)
      | VEqual v    -> "= "^(string_of_version v)
      | VLesser v   -> "< "^(string_of_version v)
      | VGreaterEqual v -> ">= "^(string_of_version v)
      | VLesserEqual v  -> "<= "^(string_of_version v)
      | VOr (c1, c2)  -> 
          (string_of_comparator c1)^" || "^(string_of_comparator c2)
      | VAnd (c1, c2) -> 
          (string_of_comparator c1)^" && "^(string_of_comparator c2)
  
  (** Convert a version to a varname 
    *)
  let varname_of_version v =
    let vstr = 
      string_of_version v
    in
    let buff = 
      Buffer.create (String.length vstr)
    in
      String.iter 
        (fun c ->
           if ('a' <= c && c <= 'z') ||
              ('A' <= c && c <= 'Z') ||
              ('0' <= c && c <= '9') ||
              (c = '_') then
             Buffer.add_char buff c
           else
             Buffer.add_char buff '_')
        vstr;
      Buffer.contents buff 
  
  (** Convert a comparator to a varname 
    *)
  let rec varname_of_comparator =
    function 
      | VGreater v -> "gt_"^(varname_of_version v)
      | VLesser v  -> "lt_"^(varname_of_version v)
      | VEqual v   -> "eq_"^(varname_of_version v)
      | VGreaterEqual v -> "ge_"^(varname_of_version v)
      | VLesserEqual v  -> "le_"^(varname_of_version v)
      | VOr (c1, c2) ->
          (varname_of_comparator c1)^"_or_"^(varname_of_comparator c2)
      | VAnd (c1, c2) ->
          (varname_of_comparator c1)^"_and_"^(varname_of_comparator c2)
  
end

module OASISUtils = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISUtils.ml"
  
  (** Various utilities for OASIS.
    *)
  
  module MapString = Map.Make(String)
  
  (** Build a MapString with an association list 
    *)
  let map_string_of_assoc assoc =
    List.fold_left
      (fun acc (k, v) -> MapString.add k v acc)
      MapString.empty
      assoc
  
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
  
  (** Build a set out of list 
    *)
  let set_string_of_list =
    set_string_add_list
      SetString.empty
  
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
  
end

module OASISExpr = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISExpr.ml"
  
  (** OASIS expression manipulation
    *)
  
  open OASISTypes
  
  (** Evaluate each conditions and choose the right one. *)
  let choose var_get test_get lst =
    let rec eval =
      function
        | EBool b ->
            b
  
        | ENot e -> 
            not (eval e)
  
        | EAnd (e1, e2) ->
            (eval e1) && (eval e2)
  
        | EOr (e1, e2) -> 
            (eval e1) || (eval e2)
  
        | EFlag nm ->
            let v =
              var_get nm
            in
              assert(v = "true" || v = "false");
              (v = "true")
  
        | ETest (nm, vl) ->
            let v =
              test_get nm
            in
              (v = vl)
    in
  
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
  
end

module OASISSection = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISSection.ml"
  
  (** Manipulate section 
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  
end

module OASISBuildSection = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISBuildSection.ml"
  
  (** Build section functions
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  
end

module OASISExecutable = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISExecutable.ml"
  
  (** Executable schema and generator
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  
  (* Return the directory that will contain the executable *)
  let exec_main_path (cs, bs, exec) = 
    let dir = 
      Filename.dirname exec.exec_main_is
    in
      if dir = Filename.current_dir_name then
        bs.bs_path
      else
        Filename.concat bs.bs_path dir
  
  (* Return the name of the real name of executable, with full 
     path
   *)
  let exec_is ((cs, _, _) as exec_data) = 
    Filename.concat (exec_main_path exec_data) cs.cs_name
  
end

module OASISLibrary = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISLibrary.ml"
  
  (** Library schema and generator 
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  open OASISUtils
  open CommonGettext
  
  (** Library group are organized in trees
    *)
  type group_t = 
    | Container of findlib_name * (group_t list)
    | Package of (findlib_name * 
                  common_section *
                  build_section * 
                  library * 
                  (group_t list))
  
  (** Compute groups of libraries, associate root libraries with 
      a tree of its children. A group of libraries is defined by 
      the fact that these libraries has a parental relationship 
      and must be isntalled together, with the same META file.
    *)
  let group_libs pkg =
    (** Associate a name with its children *)
    let children =
      List.fold_left
        (fun mp ->
           function
             | Library (cs, bs, lib) ->
                 begin
                   match lib.lib_findlib_parent with 
                     | Some p_nm ->
                         begin
                           let children =
                             try 
                               MapString.find p_nm mp
                             with Not_found ->
                               []
                           in
                             MapString.add p_nm ((cs, bs, lib) :: children) mp
                         end
                     | None ->
                         mp
                 end
             | _ ->
                 mp)
        MapString.empty
        pkg.sections
    in
  
    (* Compute findlib name of a single node *)
    let findlib_name (cs, _, lib) =
      match lib.lib_findlib_name with 
        | Some nm -> nm
        | None -> cs.cs_name
    in
  
    (** Build a package tree *)
    let rec tree_of_library containers ((cs, bs, lib) as acc) =
      match containers with
        | hd :: tl ->
            Container (hd, [tree_of_library tl acc])
        | [] ->
            (* TODO: allow merging containers with the same 
             * name 
             *)
            Package 
              (findlib_name acc, cs, bs, lib,
               (try 
                  List.rev_map 
                    (fun ((_, _, child_lib) as child_acc) ->
                       tree_of_library 
                         child_lib.lib_findlib_containers
                         child_acc)
                    (MapString.find cs.cs_name children)
                with Not_found ->
                  []))
    in
  
      (* TODO: check that libraries are unique *)
      List.fold_left
        (fun acc ->
           function
             | Library (cs, bs, lib) when lib.lib_findlib_parent = None -> 
                 (tree_of_library lib.lib_findlib_containers (cs, bs, lib)) :: acc
             | _ ->
                 acc)
        []
        pkg.sections
  
  (** Compute internal library findlib names, including subpackage
      and return a map of it.
    *)
  let findlib_name_map pkg = 
  
    (* Compute names in a tree *)
    let rec findlib_names_aux path mp grp =
      let fndlb_nm, children, mp =
        match grp with
          | Container (fndlb_nm, children) ->
              fndlb_nm, children, mp
                                    
          | Package (fndlb_nm, {cs_name = nm}, _, _, children) ->
              fndlb_nm, children, (MapString.add nm (path, fndlb_nm) mp)
      in
      let fndlb_nm_full =
        (match path with
           | Some pth -> pth^"."
           | None -> "")^
        fndlb_nm
      in
        List.fold_left
          (findlib_names_aux (Some fndlb_nm_full))
          mp
          children
    in
  
      List.fold_left
        (findlib_names_aux None)
        MapString.empty
        (group_libs pkg)
  
  
  (** Return the findlib name of the library without parents *)
  let findlib_of_name ?(recurse=false) map nm =
    try 
      let (path, fndlb_nm) = 
        MapString.find nm map
      in
        match path with 
          | Some pth when recurse -> pth^"."^fndlb_nm
          | _ -> fndlb_nm
  
    with Not_found ->
      failwith 
        (Printf.sprintf
           (f_ "Unable to translate internal library '%s' to findlib name")
           nm)
  
  (** Return the findlib root name of a group, it takes into account
      containers. So the return group name is the toplevel name
      for both libraries and theirs containers.
    *)
  let findlib_of_group = 
    function
      | Container (fndlb_nm, _) 
      | Package (fndlb_nm, _, _, _, _) -> fndlb_nm
  
  (** Return the root library, i.e. the first found into the group tree
      that has no parent.
    *)
  let root_of_group grp =
    let rec root_lib_aux =
      function 
        | Container (_, children) ->
            root_lib_lst children        
        | Package (_, cs, bs, lib, children) ->
            if lib.lib_findlib_parent = None then 
              cs, bs, lib
            else
              root_lib_lst children
    and root_lib_lst =
      function
        | [] ->
            raise Not_found
        | hd :: tl ->
            try
              root_lib_aux hd
            with Not_found ->
              root_lib_lst tl
    in
      try
        root_lib_aux grp
      with Not_found ->
        failwith
          (Printf.sprintf 
             "Unable to determine root library of findlib library '%s'"
             (findlib_of_group grp))
  
end

module OASISFlag = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISFlag.ml"
  
  (** Flag schema and generator
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  
end

module OASISPackage = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISPackage.ml"
  
  (** Package schema and generator 
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  
end

module OASISSourceRepository = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISSourceRepository.ml"
  
  (** SourceRepository schema and generator
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  
end

module OASISTest = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISTest.ml"
  
  (** Test schema and generator
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  
end


# 1137 "setup.ml"
module BaseEnvLight = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseEnvLight.ml"
  
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


# 1217 "setup.ml"
module BaseEnv = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseEnv.ml"
  
  (** Read-only environment
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  open PropList
  
  (** Origin of the variable, if a variable has been already set
      with a higher origin, it won't be set again
    *)
  type origin_t = 
    | ODefault     (** Default computed value *)
    | OGetEnv      (** Extracted from environment, using Sys.getenv *)
    | OFileLoad    (** From loading file setup.data *)
    | OCommandLine (** Set on command line *)
  
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
  
  (** Schema for environment 
    *)
  let schema =
    Schema.create "environment"
  
  (** Environment data 
    *)
  let env = 
    Data.create ()
  
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
           with Unknown_field (_, _) ->
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
      Schema.get schema env name
    in
      var_expand vl
  
  (** Choose a value among conditional expression
    *)
  let var_choose lst =
    OASISExpr.choose 
      var_get 
      (function
         | TOs_type       -> var_get "os_type"
         | TSystem        -> var_get "system"
         | TArchitecture  -> var_get "architecture"
         | TCcomp_type    -> var_get "ccomp_type"
         | TOCaml_version -> var_get "ocaml_version")
      lst
  
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
      FieldRO.create
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
    if Schema.mem schema name then
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
  
  (** Well-typed ignore for var_define 
    *)
  let var_ignore (e : unit -> string) = 
    ()
  
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
  
  (** Get all variable
    *)
  let var_all () =
    List.rev
      (Schema.fold
         (fun acc nm def _ -> 
            if not def.hide || bool_of_string (print_hidden ()) then
              nm :: acc
            else
              acc)
         []
         schema)
  
  (** Environment default file 
    *)
  let default_filename =
    BaseEnvLight.default_filename
  
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
                Schema.preset schema env nm ~context:OFileLoad value;
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
  
  (** Uninitialize environment 
    *)
  let unload () = 
    (* TODO *)
    ()
  
  (** Save environment on disk.
    *)
  let dump ?(filename=default_filename) () = 
    let chn =
      open_out_bin filename
    in
      Schema.iter
        (fun nm def _ ->
           if def.dump then
             begin
               try 
                 let value =
                   Schema.get 
                     schema 
                     env 
                     nm
                 in
                   Printf.fprintf chn "%s = %S\n" nm value
               with Not_set _ ->
                 ()
             end)
        schema;
      close_out chn
  
  (** Display environment to user.
    *)
  let print () =
    let printable_vars =
      Schema.fold
        (fun acc nm def short_descr_opt -> 
           if not def.hide || bool_of_string (print_hidden ()) then
             begin
               let value = 
                 Schema.get 
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
    print_newline ()
  
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
                      Schema.set  
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
        (Schema.fold
          (fun acc name def short_descr_opt ->
             let var_set s = 
               Schema.set 
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
               Schema.get
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
end

module BaseMessage = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseMessage.ml"
  
  (** Message to user
      @author Sylvain Le Gall
    *)
  
  let verbose =
    ref true
  
  (** Print a warning message 
    *)
  let warning str =
    if !verbose then
      prerr_endline str
  
  (** Print an error message and exit.
    *)
  let error str =
    if !verbose then 
      prerr_endline str;
    exit 1
  
  (** Print information message.
    *)
  let info str = 
    if !verbose then
      Printf.printf "%s\n%!" str
  
  (** Print begin of line when checking for a feature.
    *)
  let checking str =
    if !verbose then
      Printf.printf "checking for %s... %!" str
  
  (** Print end of line when checking for a feature.
    *)
  let result str =
    if !verbose then
      Printf.printf "%s\n%!" str
  
  (** Print result and return it.
    *)
  let result_wrap str =
    result str;
    str
  
end

module BaseExec = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseExec.ml"
  
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
  
  (** Run a command and returns its output
    *)
  let run_read_output cmd args =
    let fn = 
      Filename.temp_file "oasis-" ".txt"
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
  
end

module BaseFileUtil = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseFileUtil.ml"
  
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
      OASISUtils.split 
        path_sep 
        (Sys.getenv "PATH")
    in
    let exec_ext = 
      match Sys.os_type with 
        | "Win32" ->
            "" 
            :: 
            (OASISUtils.split 
               path_sep 
               (Sys.getenv "PATHEXT"))
        | _ ->
            [""]
    in
      find_file [path_lst; [prg]] exec_ext 
  
  (** Copy a file 
    *)
  let cp src tgt = 
    match Sys.os_type with 
      | "Win32" ->
          BaseExec.run "copy" [src; tgt]
      | _ ->
          BaseExec.run "cp" [src; tgt]
  
  (** Create a directory
    *)
  let mkdir tgt =
    match Sys.os_type with 
      | "Win32" ->
          BaseExec.run "md" [tgt]
      | _ ->
          BaseExec.run "mkdir" [tgt]
  
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
end

module BaseArgExt = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseArgExt.ml"
  
  (** Handle command line argument
      @author Sylvain Le Gall
    *)
  
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
end

module BaseCheck = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseCheck.ml"
  
  (** {1 Checking for particular features} 
    *)
  
  open BaseEnv
  
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
  
  (** Check the presence of a particular program.
    *)
  let prog prg =
    prog_best prg [prg]
  
  (** Check the presence of a program or its native version
    *)
  let prog_opt prg = 
    prog_best prg [prg^".opt"; prg]
  
  let ocamlfind = 
    prog "ocamlfind"
  
  (** Check version, following Sys.ocaml_version convention
    *)
  let version 
        var_prefix 
        cmp
        fversion 
        () = 
    (* Really compare version provided *)
    let var = 
      var_prefix^"_version_"^(OASISVersion.varname_of_comparator cmp)
    in
      var_redefine 
        ~hide:true 
        var
        (lazy
           (let version_str =
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
            let version =
              OASISVersion.version_of_string version_str
            in
              if OASISVersion.comparator_apply version cmp then
                version_str
              else
                failwith 
                  (Printf.sprintf
                     "Cannot satisfy version constraint on %s: %s (version: %s)"
                     var_prefix
                     (OASISVersion.string_of_comparator cmp)
                     version_str)))
        ()
  
  (** Get findlib package version 
    *)
  let package_version pkg =
    BaseExec.run_read_one_line 
      (ocamlfind ())
      ["query"; "-format"; "%v"; pkg]
  
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
    let vl =
      var_redefine
        var
        (lazy (findlib_dir pkg))
        ()
    in
      (
        match version_comparator with 
          | Some ver_cmp ->
              ignore
                (version 
                   var
                   ver_cmp
                   (fun _ -> package_version pkg)
                   ())
          | None -> 
              ()
      );
      vl
end

module BaseOCamlcConfig = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseOCamlcConfig.ml"
  
  (** Read output of command ocamlc -config and transform it
    * into enviornment variable
    *)
  
  open BaseEnv
  
  module SMap = Map.Make(String)
  
  let ocamlc = 
    BaseCheck.prog_opt "ocamlc"
  
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
  
end

module BaseStandardVar = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseStandardVar.ml"
  
  (** Most standard variables for OCaml 
      @author Sylvain Le Gall
    *)
  
  open BaseCheck
  open BaseEnv
  
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
  
  (** Findlib version
    *)
  let findlib_version =
    var_define
      "findlib_version"
      (lazy 
         (BaseCheck.package_version "findlib"))
  
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
      (lazy 
         (OASISVersion.string_of_version 
            (snd (pkg_get ()))))
  
  (** Initialize some variables 
    *)
  let init pkg = 
    rpkg := Some pkg
  
end

module BaseFileAB = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseFileAB.ml"
  
  (** File .ab which content will be replaced by environment variable
     
      This is the same kind of file as .in file for autoconf, except we
      use the variable definition of ${!Buffer.add_substitute}. This is 
      the default file to be generated by configure step (even for 
      autoconf, except that it produce a master file before).
  
      @author Sylvain Le Gall
    *)
  
  open BaseEnv
  
  let to_filename fn =
    if not (Filename.check_suffix fn ".ab") then
      BaseMessage.warning 
        (Printf.sprintf 
           "File '%s' doesn't have '.ab' extension"
           fn);
    Filename.chop_extension fn
  
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
end

module BaseLog = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseLog.ml"
  
  (** Maintain a DB of what has been done
      @author Sylvain Le Gall
    *)
  
  open OASISUtils
  
  let default_filename =
    Filename.concat 
      (Filename.dirname BaseEnv.default_filename)
      "setup.log"
  
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
  
  let register event data =
    let chn_out =
      open_out_gen [Open_append; Open_creat; Open_text] 0o644 default_filename
    in
      Printf.fprintf chn_out "%S %S\n" event data;
      close_out chn_out
  
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
end

module BaseTest = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseTest.ml"
  
  (** Run test
      @author Sylvain Le Gall
    *)
  
  open BaseEnv
  open OASISTypes
  open OASISExpr
  
  let test lst pkg extra_args =
  
    let one_test (test_plugin, cs, test) =
      if var_choose test.test_run then
        begin
          let () = 
            BaseMessage.info 
              (Printf.sprintf "Running test '%s'" cs.cs_name)
          in
          let back_cwd = 
            match test.test_working_directory with 
              | Some dir -> 
                  let cwd = 
                    Sys.getcwd ()
                  in
                  let chdir d =
                    BaseMessage.info 
                      (Printf.sprintf "Changing directory to '%s'" d);
                    Sys.chdir d
                  in
                    chdir dir;
                    fun () -> chdir cwd
  
              | None -> 
                  fun () -> ()
          in
            try 
              let failure_percent =
                test_plugin pkg (cs, test) extra_args 
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
            (Printf.sprintf "Skipping test '%s'" cs.cs_name);
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
  
end

module BaseSetup = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseSetup.ml"
  
  (** Entry points for setup.ml
      @author Sylvain Le Gall
    *)
  
  open BaseEnv
  open OASISTypes
  open OASISSection
  
  type std_args_fun = 
      package -> string array -> unit
  
  type ('a, 'b) section_args_fun = 
      name * (package -> (common_section * 'a) -> string array -> 'b)
  
  type t =
      {
        configure:       std_args_fun;
        build:           std_args_fun;
        doc:             ((unit, unit)  section_args_fun) list;
        test:            ((test, float) section_args_fun) list;
        install:         std_args_fun;
        uninstall:       std_args_fun;
        clean:           std_args_fun list;
        clean_doc:       (unit, unit) section_args_fun list;
        clean_test:      (test, unit) section_args_fun list;
        distclean:       std_args_fun list;
        distclean_doc:   (unit, unit) section_args_fun list;
        distclean_test:  (test, unit) section_args_fun list;
        package:         package;
      } 
  
  (** Configure step *)
  let configure t args = 
    (* Run configure *)
    t.configure t.package args;
  
    (* Reload environment *)
    unload ();
    load ();
  
    (* Replace data in file *)
    BaseFileAB.replace t.package.files_ab
  
  (** Build step *)
  let build t args =
    t.build t.package args
  
  (** Documentation step *)
  let doc t args =
    (* TODO: documentation run *)
    ()
  
  (** Test step *)
  let test t args = 
    BaseTest.test 
      (List.rev
         (List.fold_left 
            (fun acc ->
               function
                 | Test (cs, test) ->
                     (List.assoc cs.cs_name t.test,
                      cs, 
                      test) :: acc
                 | _ ->
                     acc)
            []
            t.package.sections))
      t.package
      args
  
  (** Install step *)
  let install t args =
    t.install t.package args
  
  (** Uninstall step *)
  let uninstall t args =
    t.uninstall t.package args
  
  (** Clean and distclean steps *)
  let clean, distclean = 
    let generic_clean t what mains docs tests args = 
      (* Clean section *)
      List.iter
        (function
           | Test (cs, test) ->
               let f =
                 List.assoc cs.cs_name tests
               in
                 f t.package (cs, test) args
           | Library _ 
           | Executable _
           | Flag _ 
           | SrcRepo _ ->
               ())
        t.package.sections;
      (* Clean whole package *)
      List.iter
        (fun f -> 
           f t.package args)
        mains
    in
  
    let clean t args =
      generic_clean t "cleaning" t.clean t.clean_doc t.clean_test args
    in
  
    let distclean t args =
      (* Call clean *)
      clean t args;
  
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
      
      (* Call distclean code *)
      generic_clean t "distcleaning" t.distclean t.distclean_doc t.distclean_test args
    in
  
      clean, distclean
  
  let setup t = 
    try
      let act_ref =
        ref (fun _ -> 
               failwith
                 (Printf.sprintf
                    "No action defined, run '%s %s -help'"
                    Sys.executable_name
                    Sys.argv.(0)))
  
      in
      let extra_args_ref =
        ref []
      in
      let allow_empty_env_ref = 
        ref false
      in
  
      let arg_handle ?(allow_empty_env=false) act =
        Arg.Tuple
          [
            Arg.Rest (fun str -> extra_args_ref := str :: !extra_args_ref);
  
            Arg.Unit 
              (fun () -> 
                 allow_empty_env_ref := allow_empty_env;
                 act_ref := act);
          ]
      in
  
        Arg.parse 
          [
            "-configure",
            arg_handle ~allow_empty_env:true configure,
            "[options*] Configure build process.";
  
            "-build",
            arg_handle build,
            "[options*] Run build process.";
  
            "-doc",
            arg_handle doc,
            "[options*] Build documentation.";
  
            "-test",
            arg_handle test,
            "[options*] Build and run tests.";
  
            "-install",
            arg_handle install,
            "[options*] Install library, data, executable and documentation.";
  
            "-uninstall",
            arg_handle uninstall,
            "[options*] Uninstall library, data, executable and documentation.";
  
            "-clean",
            arg_handle ~allow_empty_env:true clean,
            "[options*] Clean build environment.";
  
            "-distclean",
            arg_handle ~allow_empty_env:true distclean,
            "[options*] Clean build and configure environment.";
          ]
          (fun str -> failwith ("Don't know what to do with "^str))
          "Setup and run build process current package\n";
  
        (* Build initial environment *)
        load ~allow_empty:!allow_empty_env_ref ();
  
        (** Initialize flags *)
        List.iter
          (function
             | Flag (cs, {flag_description = hlp; 
                          flag_default = choices}) ->
                 begin
                   let apply ?short_desc () = 
                     var_ignore
                       (var_define
                          ~cli:CLIAuto
                          ?short_desc
                          cs.cs_name
                          (lazy (string_of_bool 
                                   (var_choose choices))))
                   in
                     match hlp with 
                       | Some hlp ->
                           apply ~short_desc:hlp ()
                       | None ->
                           apply ()
                 end
             | _ -> 
                 ())
          t.package.sections;
  
        BaseStandardVar.init (t.package.name, t.package.version);
  
        !act_ref t (Array.of_list (List.rev !extra_args_ref))
  
    with e ->
      BaseMessage.error (Printexc.to_string e);
  
end


# 2905 "setup.ml"
module InternalConfigure = struct
# 0 "/home/gildor/programmation/oasis/src/internal/InternalConfigure.ml"
  
  (** Configure using internal scheme
      @author Sylvain Le Gall
    *)
  
  open BaseEnv
  open OASISTypes
  
  (** Configure build using provided series of check to be done
    * and then output corresponding file.
    *)
  let configure pkg argv =
    let var_ignore_eval var = 
      let _s : string =
        var ()
      in 
        ()
    in
  
    let build_checks bs =
      if var_choose bs.bs_build then
        begin
          (* Check tools *)
          List.iter 
            (function
               | ExternalTool tool -> 
                   var_ignore_eval (BaseCheck.prog tool)
               | InternalExecutable nm1 ->
                   (* Check that matching tool is built *)
                   List.iter
                     (function
                        | Executable ({cs_name = nm2}, 
                                      {bs_build = build}, 
                                      _) when nm1 = nm2 ->
                             if not (var_choose build) then
                               failwith 
                                 (Printf.sprintf
                                    "Cannot find buildable internal executable \
                                     '%s' when checking build depends"
                                    nm1)
                             else
                               ()
                        | _ ->
                            ())
                     pkg.sections)
            bs.bs_build_tools;
  
          (* Check depends *)
          List.iter  
            (function
               | FindlibPackage (findlib_pkg, version_comparator) ->
                   var_ignore_eval
                     (BaseCheck.package ?version_comparator findlib_pkg)
               | InternalLibrary nm1 ->
                   (* Check that matching library is built *)
                   List.iter
                     (function
                        | Library ({cs_name = nm2},
                                   {bs_build = build}, 
                                   _) when nm1 = nm2 ->
                             if not (var_choose build) then
                               failwith 
                                 (Printf.sprintf
                                    "Cannot find buildable internal library \
                                     '%s' when checking build depends"
                                    nm1)
                             else
                               ()
                        | _ ->
                            ())
                     pkg.sections)
            bs.bs_build_depends
        end
    in
  
    let ver_opt_check prefix std_var  =
      function
        | Some ver_cmp ->
            var_ignore_eval
              (BaseCheck.version prefix ver_cmp std_var)
        | None ->
            ()
    in
  
  
    (* Parse command line *)
    BaseArgExt.parse argv (args ());
  
    (* OCaml version *)
    ver_opt_check "ocaml" BaseStandardVar.ocaml_version pkg.ocaml_version;
  
    (* Findlib version *)
    ver_opt_check "findlib" BaseStandardVar.findlib_version pkg.findlib_version;
  
    (* Check build depends *)
    List.iter
      (function
         | Executable (_, bs, _)
         | Library (_, bs, _) ->
             build_checks bs
         | _ ->
             ())
      pkg.sections;
  
    (* Save and print environment *)
    dump ();
    print ()
  
end

module InternalInstall = struct
# 0 "/home/gildor/programmation/oasis/src/internal/InternalInstall.ml"
  
  (** Install using internal scheme
      @author Sylvain Le Gall
    *)
  
  open BaseEnv
  open BaseStandardVar
  open OASISTypes
  open OASISLibrary
  
  let srcdir =
    var_define
      "srcdir"
      (lazy ".")
  
  let builddir =
    var_define
      "builddir"
      (lazy (Filename.concat (srcdir ()) "_build"))
  
  let dllfn path name = 
    Filename.concat path ("dll"^name^(ext_dll ()))
  
  let libfn path name =
    Filename.concat path ("lib"^name^(ext_lib ()))
  
  let exec_hook =
    ref (fun (cs, bs, exec) -> cs, bs, exec)
  
  let lib_hook =
    ref (fun (cs, bs, lib) -> cs, bs, lib, [])
  
  let install_file_ev = 
    "install-file"
  
  let install_dir_ev =
    "install-dir"
  
  let install_findlib_ev =
    "install-findlib"
  
  let install pkg argv =
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
  
    (* Install all datas *)
    let install_datas pkg = 
  
      (* Install data for a single section *)
      let install_data bs = 
        List.iter
          (fun (src, tgt_opt) ->
             let real_srcs = 
               let real_src = 
                 Filename.concat bs.bs_path src
               in
               (* Glob the src expression *)
               let filename = 
                 Filename.basename real_src
               in
                 if String.contains filename '*' then
                   (
                     let ext = 
                       match OASISUtils.split '.' filename with 
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
               
          bs.bs_data_files
      in
  
        (* Install datas for libraries and executables *)
        List.iter
          (function
             | Library (_, bs, _)
             | Executable (_, bs, _) ->
                 install_data bs
             | _ ->
                 ())
          pkg.sections
    in
  
    (** Install all libraries *)
    let install_libs pkg =
  
      let find_lib_file (cs, bs, lib) fn =
        find_file
          (fun rootdir -> [rootdir; bs.bs_path; fn])
          rootdirs
      in
  
      let files_of_library acc data_lib = 
        let cs, bs, lib, lib_extra =
          !lib_hook data_lib
        in
        let find_lib_file =
          find_lib_file (cs, bs, lib)
        in
          (if var_choose bs.bs_install then
             [
               find_lib_file (cs.cs_name^".cma");
             ]
             :: 
             (if is_native bs.bs_compiled_object then
                (
                  try 
                    [
                      find_lib_file (cs.cs_name^".cmxa");
                      find_lib_file (cs.cs_name^(ext_lib ()));
                    ]
                  with Failure txt ->
                    BaseMessage.warning 
                      (Printf.sprintf
                         "Cannot install native library %s: %s"
                         cs.cs_name
                         txt);
                    []
                )
              else
                []
             )
             ::
             lib_extra
             ::
             (if bs.bs_c_sources <> [] then
                [
                  find_build_file (libfn bs.bs_path cs.cs_name);
                ]
              else
                [])
             ::
             (* Some architecture doesn't allow shared library (Cygwin, AIX) *)
             (if bs.bs_c_sources <> [] then
                (try 
                  [
                    find_build_file (dllfn bs.bs_path cs.cs_name);
                  ]
                 with Failure txt ->
                   if (os_type ()) <> "Cygwin" then
                     BaseMessage.warning
                       (Printf.sprintf
                          "Cannot install C static library %s: %s"
                          cs.cs_name
                          txt);
                   [])
              else
                [])
             ::
             (
               let module_to_cmi modul =
                 find_file 
                    (fun (rootdir, fn) -> [rootdir; bs.bs_path; (fn^".cmi")])
                    (rootdirs * (make_module modul))
               in
  
               let module_to_header modul =
                 assert(modul <> "");
                 find_file 
                    (fun ((rootdir, fn), ext) -> [rootdir; bs.bs_path; fn^ext])
                    (rootdirs * (make_module modul) * [".mli"; ".ml"])
               in
                 List.fold_left
                   (fun acc modul -> 
                      module_to_cmi modul :: module_to_header modul :: acc)
                   []
                   lib.lib_modules
             )
             ::
             acc
           else
             acc)
      in
  
      (* Install one group of library *)
      let install_group_lib grp = 
        (* Iterate through all group nodes *)
        let rec install_group_lib_aux acc grp =
          let acc, children = 
            match grp with 
              | Container (_, children) ->
                  acc, children
              | Package (_, cs, bs, lib, children) ->
                  files_of_library acc (cs, bs, lib), children
          in
            List.fold_left
              install_group_lib_aux
              acc
              children
        in
  
        (* Findlib name of the root library *)
        let findlib_name =
          findlib_of_group grp
        in
  
        (* Determine root library *)
        let root_lib =
          root_of_group grp
        in
  
        (* All files to install for this library *)
        let files =
          List.flatten (install_group_lib_aux [] grp)
        in
  
          (* Really install, if there is something to install *)
          if files = [] then 
            begin
              BaseMessage.info 
                (Printf.sprintf 
                   "Nothing to install for findlib library '%s'"
                   findlib_name)
            end
          else
            begin
              let meta = 
                find_lib_file root_lib "META"
              in
                BaseMessage.info 
                  (Printf.sprintf
                     "Installing findlib library '%s'"
                     findlib_name);
                BaseExec.run (ocamlfind ()) ("install" :: findlib_name :: meta :: files);
                BaseLog.register install_findlib_ev findlib_name 
            end
      in
  
        (* We install libraries in groups *)
        List.iter 
          install_group_lib
          (group_libs pkg)
    in
  
    let install_execs pkg = 
      let install_exec data_exec =
        let (cs, bs, exec) as data_exec =
          !exec_hook data_exec
        in
          if var_choose bs.bs_install then
            (
                install_file
                  (find_build_file
                     ((OASISExecutable.exec_is data_exec)^(suffix_program ())))
                  bindir;
  
                if bs.bs_c_sources <> [] && 
                   not exec.exec_custom && 
                   (* TODO: move is_native to OASISBuildSection *)
                   not (is_native bs.bs_compiled_object) then
                  (
                    install_file
                      (find_build_file
                         (dllfn (OASISExecutable.exec_main_path data_exec) cs.cs_name))
                      libdir
                  )
                else
                  ()
            )
      in
        List.iter
          (function
             | Executable (cs, bs, exec)->
                 install_exec (cs, bs, exec)
             | _ ->
                 ())
          pkg.sections
    in
    
      install_libs pkg;
      install_execs pkg;
      install_datas pkg
  
  (* Uninstall already installed data *)
  let uninstall _ argv =
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
  
end


# 3486 "setup.ml"
module OCamlbuildBuild = struct
# 0 "/home/gildor/programmation/oasis/src/ocamlbuild/OCamlbuildBuild.ml"
  
  (** Build using ocamlbuild  
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  open BaseEnv
  open BaseStandardVar
  
  type target =
    | Std of string
    | CLibrary  of string * string
    | Rename of string * string
  
  let cond_targets_hook =
    ref (fun lst -> lst)
  
  let build pkg argv =
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
  
    let cond_targets =
      List.flatten 
        [
          List.fold_left
            (fun acc ->
               function 
                 | Library (cs, bs, lib) ->
                     if var_choose bs.bs_build then
                       begin
                         let acc =
                           (* Compute what libraries should be built *)
                           let target ext =
                             Std (Filename.concat bs.bs_path (cs.cs_name^ext))
                           in
                           let byte, native =
                             target ".cma", target ".cmxa"
                           in
                             match bs.bs_compiled_object, ocamlbest () with 
                               | Native, _ 
                               | Best, "native" ->
                                   byte :: native :: acc
                               | Byte, _
                               | Best, "byte" ->
                                   byte :: acc
                               | Best, ocamlbest ->
                                   failwith 
                                     (Printf.sprintf 
                                        "Unknown ocamlbest: '%s'"
                                        ocamlbest)
                         in
  
                         let acc = 
                           (* Add C library to be built *)
                           if bs.bs_c_sources <> [] then
                             CLibrary (bs.bs_path, cs.cs_name) :: acc
                           else
                             acc
                         in
                           acc
                       end
                     else
                       acc
                 | Executable (cs, bs, exec) ->
                     if var_choose bs.bs_build then
                       begin
                         let target ext =
                           let src = 
                             (Filename.concat
                                bs.bs_path
                                (Filename.chop_extension 
                                   exec.exec_main_is))^ext
                           in
                           let exec_is =
                             OASISExecutable.exec_is (cs, bs, exec)
                           in
                             if src = exec_is then
                               Std src
                             else
                               Rename (src, exec_is)
                         in
                         let byte, native = 
                           target ".byte", target ".native" 
                         in
                           match bs.bs_compiled_object, ocamlbest () with
                             | Byte, _
                             | Best, "byte" ->
                                 byte :: acc
                             | Native, _
                             | Best, "native" ->
                                 native :: acc
                             | Best, ocamlbest ->
                                 failwith 
                                   (Printf.sprintf 
                                      "Unknown ocamlbest: '%s'"
                                      ocamlbest)
                       end
                     else
                       acc
                 | Test _ | SrcRepo _ | Flag _ ->
                     acc)
            []
            pkg.sections;
        ]
    in
  
    let last_rtargets =
      List.fold_left
        (fun acc tgt ->
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
                 [])
        []
        (!cond_targets_hook cond_targets)
    in
      if last_rtargets <> [] then
        ocamlbuild_run last_rtargets
  
  let clean pkg extra_args  = 
    (* TODO use ocamlbuild *)
    BaseExec.run "ocamlbuild" ("-clean" :: (Array.to_list extra_args))
  
end


# 3657 "setup.ml"
module CustomPlugin = struct
# 0 "/home/gildor/programmation/oasis/src/custom/CustomPlugin.ml"
  
  (** Generate custom configure/build/doc/test/install system
      @author
    *)
  
  open BaseEnv
  
  
  
  type t =
      {
        cmd_main:     string * (string list);
        cmd_clean:     (string * (string list)) option;
        cmd_distclean: (string * (string list)) option;
      } 
  
  let run cmd args extra_args =
    BaseExec.run 
      (var_expand cmd)
      (List.map 
         var_expand
         (args @ (Array.to_list extra_args)))
  
  let main {cmd_main = (cmd, args)} _ extra_args =
    run cmd args extra_args 
  
  let clean t pkg extra_args =
    match t with
      | {cmd_clean = Some (cmd, args)} ->
          run cmd args extra_args
      | _ ->
          ()
  
  let distclean t pkg extra_args =
    match t with
      | {cmd_distclean = Some (cmd, args)} ->
          run cmd args extra_args
      | _ ->
          ()
  
  module Test =
  struct
    let main t pkg (cs, test) extra_args =
      try
        main t pkg extra_args;
        0.0
      with Failure _ ->
        1.0
  
    let clean t pkg (cs, test) extra_args =
      clean t pkg extra_args
  
    let distclean t pkg (cs, test) extra_args =
      distclean t pkg extra_args 
  end
  
  module Doc =
  struct
    let main t pkg (cs, ()) extra_args =
      main t pkg extra_args
  
    let clean t pkg (cs, ()) extra_args =
      clean t pkg extra_args
  
    let distclean t pkg (cs, ()) extra_args =
      distclean t pkg extra_args
  end
  
end


# 3731 "setup.ml"
open OASISTypes;;
let setup () =
  BaseSetup.setup
    {
       BaseSetup.configure = InternalConfigure.configure;
       build = OCamlbuildBuild.build;
       test =
         [
            ("main",
              CustomPlugin.Test.main
                {
                   CustomPlugin.cmd_main = ("_build/tests/test", []);
                   cmd_clean = None;
                   cmd_distclean = None;
                   })
         ];
       doc = [];
       install = InternalInstall.install;
       uninstall = InternalInstall.uninstall;
       clean = [OCamlbuildBuild.clean];
       clean_test =
         [
            ("main",
              CustomPlugin.Test.clean
                {
                   CustomPlugin.cmd_main = ("_build/tests/test", []);
                   cmd_clean = None;
                   cmd_distclean = None;
                   })
         ];
       clean_doc = [];
       distclean = [];
       distclean_test =
         [
            ("main",
              CustomPlugin.Test.distclean
                {
                   CustomPlugin.cmd_main = ("_build/tests/test", []);
                   cmd_clean = None;
                   cmd_distclean = None;
                   })
         ];
       distclean_doc = [];
       package =
         {
            oasis_version = VInt (1, VInt (0, VEnd));
            ocaml_version =
              Some (VGreaterEqual (VInt (3, VInt (11, VInt (1, VEnd)))));
            findlib_version = None;
            name = "ocaml-data-notation";
            version = VInt (0, VInt (0, VInt (1, VEnd)));
            license = LGPL_link_exn;
            license_file = None;
            copyrights = [];
            maintainers = [];
            authors = ["Sylvain Le Gall"];
            homepage = None;
            synopsis = "Store data using OCaml notation";
            description = None;
            categories = [];
            conf_type = "internal";
            build_type = "ocamlbuild";
            install_type = "internal";
            files_ab = [];
            sections =
              [
                 Test
                   ({cs_name = "main"; cs_data = PropList.Data.create (); },
                     {
                        test_type = "custom";
                        test_command = ("_build/tests/test", []);
                        test_working_directory = None;
                        test_run = [(EBool true, true)];
                        test_build_tools = [];
                        });
                 Executable
                   ({cs_name = "test"; cs_data = PropList.Data.create (); },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, false)];
                        bs_path = "tests";
                        bs_compiled_object = Byte;
                        bs_build_depends =
                          [
                             FindlibPackage ("oUnit", None);
                             FindlibPackage ("fileutils", None);
                             InternalLibrary "odn";
                             FindlibPackage ("str", None)
                          ];
                        bs_build_tools = [ExternalTool "ocamlbuild"];
                        bs_c_sources = [];
                        bs_data_files = [];
                        },
                     {exec_custom = false; exec_main_is = "test.ml"; });
                 Library
                   ({cs_name = "pa_noodn"; cs_data = PropList.Data.create (); 
                    },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, true)];
                        bs_path = "src";
                        bs_compiled_object = Byte;
                        bs_build_depends =
                          [
                             FindlibPackage
                               ("type-conv",
                                 Some
                                   (VGreaterEqual
                                      (VInt (1, VInt (6, VInt (7, VEnd))))));
                             FindlibPackage ("camlp4.lib", None);
                             FindlibPackage ("camlp4.quotations.o", None)
                          ];
                        bs_build_tools = [ExternalTool "ocamlbuild"];
                        bs_c_sources = [];
                        bs_data_files = [];
                        },
                     {
                        lib_modules = ["Pa_noodn"];
                        lib_findlib_parent = Some "odn";
                        lib_findlib_name = Some "syntax";
                        lib_findlib_containers = ["without"];
                        });
                 Library
                   ({cs_name = "pa_odn"; cs_data = PropList.Data.create (); },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, true)];
                        bs_path = "src";
                        bs_compiled_object = Byte;
                        bs_build_depends =
                          [
                             FindlibPackage
                               ("type-conv",
                                 Some
                                   (VGreaterEqual
                                      (VInt (1, VInt (6, VInt (7, VEnd))))));
                             FindlibPackage ("camlp4.lib", None);
                             FindlibPackage ("camlp4.quotations.o", None)
                          ];
                        bs_build_tools = [ExternalTool "ocamlbuild"];
                        bs_c_sources = [];
                        bs_data_files = [];
                        },
                     {
                        lib_modules = ["Pa_odn"];
                        lib_findlib_parent = Some "odn";
                        lib_findlib_name = Some "syntax";
                        lib_findlib_containers = ["with"];
                        });
                 Library
                   ({cs_name = "odn"; cs_data = PropList.Data.create (); },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, true)];
                        bs_path = "src";
                        bs_compiled_object = Best;
                        bs_build_depends = [];
                        bs_build_tools = [ExternalTool "ocamlbuild"];
                        bs_c_sources = [];
                        bs_data_files = [];
                        },
                     {
                        lib_modules = ["ODN"];
                        lib_findlib_parent = None;
                        lib_findlib_name = None;
                        lib_findlib_containers = [];
                        })
              ];
            plugins = ["StdFiles"; "DevFiles"; "META"];
            schema_data = PropList.Data.create ();
            };
       }
  ;;
(* OASIS_STOP *)
setup ();;
