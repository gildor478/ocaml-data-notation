
(** OCaml data notation.
    
    This module helps to translate OCaml data into a string following
    OCaml syntax.
  *)

(** {2 Types}
  *)

type module_name  = string
type field_name   = string
type variant_name = string

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

let pp_odn fmt t =

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

  let rec pp_odn_aux fmt =
    function
      | REC (mod_nm, lst) ->
          fprintf fmt "@[{@[<hv1>@,";
          List.iter
            (fun (fld, e) ->
               fprintf fmt "@[<hv2>%s.%s =@ %a@];@ " 
                 mod_nm fld pp_odn_aux e)
            lst;
          fprintf fmt "@]@,}@]"
      | LST lst ->
          fprintf fmt "@[[@[<hv1>@,%a@]@,]@]"
            (pp_list pp_odn_aux ";@ ") lst
      | STR str ->
          fprintf fmt "%S" str
      | VRT (nm, []) ->
          pp_print_string fmt nm
      | VRT (nm, lst) ->
          pp_open_hvbox fmt 2;
          pp_print_string fmt nm;
          pp_print_space fmt ();
          pp_odn_aux fmt (TPL lst);
          pp_close_box fmt ()
      | BOO b ->
          pp_print_bool fmt b
      | TPL [] ->
          invalid_arg "BaseGenCode.TPL []"
      | TPL [v] ->
          pp_odn_aux fmt v
      | TPL (hd :: tl) ->
          pp_open_hvbox fmt 2;
          pp_print_char fmt '(';
          pp_odn_aux fmt hd;
          List.iter
            (fun v ->
               pp_print_char fmt ',';
               pp_print_space fmt ();
               pp_odn_aux fmt v)
            tl;
          pp_print_char fmt ')';
          pp_close_box fmt ()
      | UNT ->
          pp_print_string fmt "()"
      | FLT f ->
          pp_print_float fmt f
      | INT i ->
          pp_print_int fmt i
  in

    pp_odn_aux fmt t


let string_of_odn odn =
  let buff = 
    Buffer.create 13
  in
  let fmt =
    formatter_of_buffer buff
  in
    pp_odn fmt odn;
    pp_print_flush fmt ();
    Buffer.contents buff


