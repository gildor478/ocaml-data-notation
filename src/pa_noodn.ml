(** Syntax extension that removes "with odn" 
    from files.

  @author Sylvain Le Gall
  *)

open Camlp4
open PreCast
open Ast
open Pa_type_conv

add_generator
  "odn"
  (fun _ ->
     let _loc = 
       Loc.ghost
     in
       <:str_item<>>)
