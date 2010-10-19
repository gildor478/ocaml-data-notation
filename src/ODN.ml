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

(** OCaml data notation.
    
    This module helps to translate OCaml data into a string following
    OCaml syntax.
  *)

(** {2 Types}
  *)

type module_name  = string
type field_name   = string
type variant_name = string
type var_name     = string

type t = 
  (** Record *)
  | REC of module_name * (field_name * t) list
  (** List *)
  | LST of t list
  (** String *)
  | STR of string
  (** Variant type constructor *)
  | VRT of variant_name * t list
  (** Boolean *)
  | BOO of bool
  (** Integer *)
  | INT of int
  (** Float *)
  | FLT of float
  (** Tuple *)
  | TPL of t list
  (** Unit () *)
  | UNT
  (** Function application *)
  | APP of var_name * (var_name * t) list * t list 
  (** Variable *)
  | VAR of var_name
  (** Polymorphic variant *)
  | PVR of variant_name

(** {2 Basic conversion} 
  *)

let of_unit () = 
  UNT

let of_bool b =
  BOO b 

let of_string s =
  STR s

let of_int i = 
  INT i

let of_float f =
  FLT f
   
let of_option f =
  function
    | Some v -> VRT("Some", [f v])
    | None   -> VRT("None", [])

let of_list f lst =
  LST (List.map f lst)

let of_tuple2 (f1, f2) (v1, v2) =
  TPL [f1 v1; f2 v2]

let of_tuple3 (f1, f2, f3) (v1, v2, v3) = 
  TPL [f1 v1; f2 v2; f3 v3]

let of_tuple4 (f1, f2, f3, f4) (v1, v2, v3, v4) = 
  TPL [f1 v1; f2 v2; f3 v3; f4 v4]

let of_tuple5 (f1, f2, f3, f4, f5) (v1, v2, v3, v4, v5) = 
  TPL [f1 v1; f2 v2; f3 v3; f4 v4; f5 v5]

(** {2 Formating}
  *)

open Format

let pp_odn ?(opened_modules=[]) fmt t =

  let opened_modules = 
    (* Use opened modules starting with the bigger *)
    List.sort 
      (fun mod1 mod2 -> String.length mod2 - String.length mod1)
      opened_modules
  in

  let pp_list pp_elem lst_sep fmt =
    function
      | [] ->
          ()
      | hd :: tl ->
          pp_elem fmt hd;
          List.iter
            (fun e ->
               fprintf fmt lst_sep;
               pp_elem fmt e)
            tl
  in

  let pp_print_id fmt id =  
    let chop_opened_module str =
      try
        let str_len =
          String.length str
        in

        let matching_opened_mod =
          List.find
            (fun opened_mod ->
               let opened_mod_len =
                 String.length opened_mod
               in
                 if opened_mod_len + 1 <= str_len then
                   (opened_mod = String.sub str 0 opened_mod_len)
                   &&
                   str.[opened_mod_len] = '.'
                 else
                   false)
            opened_modules
        in

        let chop_prefix_len =
          (String.length matching_opened_mod) + 1
        in

          String.sub str chop_prefix_len (str_len - chop_prefix_len)
            
      with Not_found ->
        str
    in
      pp_print_string fmt (chop_opened_module id)
  in

  let rec pp_odn_aux fmt =
    function
      | REC (mod_nm, flds) ->
          begin
            match flds with 
              | (hd_fld, hd_e) :: tl ->
                  (* We get the first field to add
                   * the module name at the beginning
                   *)
                  begin
                    let pp_field fmt (fld, e) =
                      fprintf fmt 
                        "@[<hv 2>%a =@ %a@];@ " 
                        pp_print_id fld 
                        pp_odn_aux e
                    in

                    fprintf fmt "@[{@[<hv 2>@,";
                    pp_field fmt (mod_nm^"."^hd_fld, hd_e);
                    List.iter (pp_field fmt) tl;
                    fprintf fmt "@]@,}@]"
                  end

              | [] ->
                  fprintf fmt "{}"
          end

      | LST lst ->
          fprintf fmt "@[[@[<hv 2>@,%a@]@,]@]"
            (pp_list pp_odn_aux ";@ ") lst
      | STR str ->
          fprintf fmt "%S" str
      | VRT (nm, []) ->
          pp_print_id fmt nm
      | VRT (nm, lst) ->
          fprintf fmt
            "@[<hv 2>%a@ %a@]"
            pp_print_id nm
            pp_odn_aux (TPL lst)
      | BOO b ->
          pp_print_bool fmt b
      | TPL [] ->
          pp_print_string fmt "()"
      | TPL [(FLT _) as v] 
      | TPL [(INT _) as v]
      | TPL [(STR _) as v]
      | TPL [(REC _) as v] 
      | TPL [(LST _) as v] 
      | TPL [(BOO _) as v] 
      | TPL [UNT as v]
      | TPL [(VAR _) as v] ->
          pp_odn_aux fmt v
      | TPL lst ->
          fprintf fmt
            "@[<hv 2>(%a)@]"
            (pp_list pp_odn_aux ",@ ") lst
      | UNT ->
          pp_print_string fmt "()"
      | FLT f ->
          pp_print_float fmt f
      | INT i ->
          pp_print_int fmt i
      | APP (fnm, named_args, args) ->
          fprintf fmt
            "@[<hv 2>%a%a%a@]"
            pp_print_id fnm

            (pp_list 
               (fun fmt (nm, e) -> 
                  fprintf fmt "@ ~%s:%a" nm pp_odn_aux e) "") 
            named_args

            (pp_list 
               (fun fmt e -> 
                  fprintf fmt "@ %a" pp_odn_aux e) "")
            args
      | VAR nm ->
          pp_print_id fmt nm
      | PVR nm ->
          pp_print_id fmt ("`"^nm)
  in

    pp_odn_aux fmt t


let string_of_odn ?opened_modules odn =
  let buff = 
    Buffer.create 13
  in
  let fmt =
    formatter_of_buffer buff
  in
    pp_odn ?opened_modules fmt odn;
    pp_print_flush fmt ();
    Buffer.contents buff


