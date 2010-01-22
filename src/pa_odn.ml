
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
  Gen.idp _loc (odn_fun_name tn)
;;

let rec odn_of_type _loc =
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
    | <:ctyp<$tp$ option>> ->
        <:expr<ODN.of_option $odn_of_type _loc tp$>>
    | <:ctyp<$tp$ list>> ->
        <:expr<ODN.of_list $odn_of_type _loc tp$>> 
    | <:ctyp<$tp1$ $tp2$>> ->
        <:expr<$odn_of_type _loc tp2$ $odn_of_type _loc tp1$>>
    | <:ctyp<'$parm$>> ->
        <:expr<$id:odn_id_name _loc parm []$>>

    (* Tuples *)
    | <:ctyp<$tp1$ * $tp2$ * $tp3$ * $tp4$ * $tp5$>> ->
        <:expr<ODN.of_tuple5 
                  ($odn_of_type _loc tp1$,
                   $odn_of_type _loc tp2$,
                   $odn_of_type _loc tp3$,
                   $odn_of_type _loc tp4$,
                   $odn_of_type _loc tp5$)>>
    | <:ctyp<$tp1$ * $tp2$ * $tp3$ * $tp4$>> ->
        <:expr<ODN.of_tuple4 
                  ($odn_of_type _loc tp1$,
                   $odn_of_type _loc tp2$,
                   $odn_of_type _loc tp3$,
                   $odn_of_type _loc tp4$)>>
    | <:ctyp<$tp1$ * $tp2$ * $tp3$>> ->
        <:expr<ODN.of_tuple3 
                  ($odn_of_type _loc tp1$,
                   $odn_of_type _loc tp2$,
                   $odn_of_type _loc tp3$)>>
    | <:ctyp<$tp1$ * $tp2$>> ->
        <:expr<ODN.of_tuple2 
                  ($odn_of_type _loc tp1$,
                   $odn_of_type _loc tp2$)>>
    | <:ctyp<( $tup:tp$ )>> ->
        failwith "Tuple is too big"

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
        | <:ctyp<$tp1$ | $tp2$>> ->
            <:match_case<$sum_fold _loc tp1$ | $sum_fold _loc tp2$>>
        | <:ctyp<$uid:cnstr$>> ->
            <:match_case<$uid:cnstr$ -> ODN.VRT($str:sum_name cnstr$,[])>>
        | <:ctyp<$uid:cnstr$ of $tps$>> ->
            let patts, exprs, _ = 
              List.fold_left 
              (fun (acc_patt, acc_expr, i) tp -> 
                 let vnm =
                   "v"^(string_of_int i)
                 in
                   (Gen.idp _loc vnm) :: acc_patt, 
                   <:expr<$odn_of_type _loc tp$ $Gen.ide _loc vnm$>> :: acc_expr, 
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
        | _ ->
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
        | <:ctyp<$lid:nm$: $ctp$>> ->
            <:expr<$str:nm$, $odn_of_type _loc ctp$ v.$lid:nm$>>
        | _ -> 
            assert false
    in
     List.map rec_map (list_of_ctyp ctp [])
 in
    <:expr<
       fun v -> 
         ODN.REC ($str:get_conv_path ()$, 
              $Gen.mk_expr_lst _loc rec_def$)>>
;;

let odn_of_variants _loc ctp =
  dbug "variants";
  assert false
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
            Gen.switch_tp_def _loc
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
          _loc, Gen.type_is_recursive _loc type_name rhs
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



