(********************************************************************************)
(*  ODN: Dump data using OCaml notation                                         *)
(*                                                                              *)
(*  Copyright (C) 2009-2011, OCamlCore SARL                                     *)
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

(** Syntax extension that adds function converting 
    data into ODN.t using type declaration to guess
    how to do.

  @author Sylvain Le Gall
  *)

open Camlp4;;
open PreCast;;
open Ast;;
open Pa_type_conv;;

let dbug =
  prerr_endline
;;

let odn_fun_name tn =
  "odn_of_"^tn
;;

let odn_id_name _loc tn rev_path =
  Gen.ident_of_rev_path _loc ((odn_fun_name tn) :: rev_path)
;;

let odn_patt_name _loc tn =
  <:patt< $lid:odn_fun_name tn$ >>
;;

let rec odn_of_tuple _loc tps =
    let patts, exprs, _ = 
      List.fold_left 
      (fun (acc_patt, acc_expr, i) tp -> 
         let vnm =
           "v"^(string_of_int i)
         in
           <:patt< $lid:vnm$ >> :: acc_patt,
           <:expr< $odn_of_type _loc tp$ $lid:vnm$ >> :: acc_expr,
           i + 1)
      ([], [], 0)
      (List.rev 
         (list_of_ctyp tps []))
    in
    let patt = 
      match patts with 
        | [patt] -> patt
        | _ -> <:patt<($tup:paCom_of_list patts$)>>
    in
      <:expr<fun $patt$ -> 
        ODN.TPL($Gen.mk_expr_lst _loc exprs$)>>

and odn_of_variants _loc vrts = 
  let lst = 
    list_of_ctyp vrts []
  in

  let lst' = 
    List.map 
      (function
         | <:ctyp<`$cnstr$>> ->
             <:match_case<`$cnstr$ -> ODN.PVR ($str:cnstr$, None) >>
         | <:ctyp<`$cnstr$ of $tps$>> ->
             begin
               let vnm = "tpl" in
               let var_expr = <:expr< $lid:vnm$ >> in
               let var_patt = <:patt< $lid:vnm$ >> in
               let expr = 
                 <:expr<$odn_of_tuple _loc tps$ $var_expr$>>
               in
                 <:match_case<`$uid:cnstr$ $var_patt$ ->
                   ODN.PVR($str:cnstr$, Some $expr$)>>
             end
         | _ ->
             assert false)
      lst
  in
    <:expr<function $mcOr_of_list lst'$>>

and odn_of_type _loc =
  function 
    | <:ctyp<$id:id$>> ->
        begin
          match Gen.get_rev_id_path id [] with 
            | ["unit"] ->   <:expr<ODN.of_unit>>
            | ["bool"] ->   <:expr<ODN.of_bool>>
            | ["string"] -> <:expr<ODN.of_string>>
            | ["int"]    -> <:expr<ODN.of_int>>
            | ["float"]  -> <:expr<ODN.of_float>>
            | tn :: rev_path  -> 
                <:expr<$id:odn_id_name _loc tn rev_path$>>
            | [] ->
                assert false
        end
    | <:ctyp<($tp$ option)>> ->
        <:expr<function x -> ODN.of_option $odn_of_type _loc tp$ x>>
    | <:ctyp<($tp$ list)>> ->
        <:expr<function x -> ODN.of_list $odn_of_type _loc tp$ x>>
    | <:ctyp<($tp1$ $tp2$)>> ->
        <:expr<$odn_of_type _loc tp2$ $odn_of_type _loc tp1$>>
    | <:ctyp<'$parm$>> ->
        <:expr<$id:odn_id_name _loc parm []$>>
    | <:ctyp< ( $tup:tp$ ) >> ->
        odn_of_tuple _loc tp
    | <:ctyp<[$vrts$]>> ->
        odn_of_variants _loc vrts
    | _ ->
        assert false
;;

let odn_of_alias _loc ctp =
  <:expr<$odn_of_type _loc ctp$>>
;;

let odn_of_sum _loc ctp = 
  let sum_def =
    let sum_name nm = 
      get_conv_path ()^"."^nm
    in
    let rec sum_fold _loc =
      function
        | TyOr (_loc, tp1, tp2) ->
            <:match_case<$sum_fold _loc tp1$ | $sum_fold _loc tp2$>>
        | TyId (_loc, IdUid(_, cnstr)) ->
            <:match_case<$uid:cnstr$ -> ODN.VRT($str:sum_name cnstr$,[])>>
        | TyOf (_loc, TyId(_, IdUid(_, cnstr)), tps) ->
            let patts, exprs, _ = 
              List.fold_left 
              (fun (acc_patt, acc_expr, i) tp -> 
                 let vnm =
                   "v"^(string_of_int i)
                 in
                   <:patt< $lid:vnm$ >> :: acc_patt,
                   <:expr< $odn_of_type _loc tp$ $lid:vnm$ >> :: acc_expr,
                   i + 1)
              ([], [], 0)
              (List.rev 
                 (list_of_ctyp tps []))
            in
            let patt = 
              match patts with 
                | [patt] -> patt
                | _ -> <:patt<($tup:paCom_of_list patts$)>>
            in
              <:match_case<$uid:cnstr$ $patt$ -> 
                ODN.VRT($str:sum_name cnstr$, $Gen.mk_expr_lst _loc exprs$)>>
        | ty ->
            assert false
    in
      sum_fold _loc ctp
  in
    <:expr<function $sum_def$>>
;;

let odn_of_record _loc ctp = 
  let rec_def = 
    let rec rec_map = 
      function 
        | TyCol(_loc, TyId(_, IdLid(_, nm)), ctp) ->
            <:expr<$str:nm$, $odn_of_type _loc ctp$ v.$lid:nm$>>
        | ty -> 
            assert false
    in
     List.map rec_map (list_of_ctyp ctp [])
 in
    <:expr<
       fun v -> 
         ODN.REC ($str:get_conv_path ()$, 
              $Gen.mk_expr_lst _loc rec_def$)>>
;;

let odn_of_mani _loc ctp1 ctp2 = 
  dbug "mani";
  assert false
;;

let odn_of_nil _loc =
  dbug "nil";
  assert false
;;

let odn_of tp = 
  let rec odn_aux = 
    function
      | TyDcl (_loc, type_name, tps, rhs, _cl) ->
          let body = 
            Gen.switch_tp_def
              ~alias:odn_of_alias
              ~sum:odn_of_sum
              ~record:odn_of_record
              ~variants:odn_of_variants
              ~mani:odn_of_mani
              ~nil:odn_of_nil
              rhs
          in
          let patts =
            List.map 
              (fun tp -> odn_patt_name _loc (Gen.get_tparam_id tp))
              tps
          in
          let fun_name = 
            odn_id_name _loc type_name []
          in
            <:binding<$id:fun_name$ = $Gen.abstract _loc patts body$>>
      | TyAnd (_loc, tp1, tp2) ->
          <:binding<$odn_aux tp1$ and $odn_aux tp2$>>
      | _ ->
          assert false
  in
  let _loc, recursive = 
    match tp with
      | TyDcl (_loc, type_name, _, rhs, _) -> 
          _loc, Gen.type_is_recursive type_name rhs
      | TyAnd (_loc, _, _) -> 
          _loc, true
      | _ -> assert false
  in
    if recursive then
      <:str_item<let rec $odn_aux tp$>> 
    else
      <:str_item<let $odn_aux tp$>> 
;;

add_generator 
  "odn"
  odn_of
;;



