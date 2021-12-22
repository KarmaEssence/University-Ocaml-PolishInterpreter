(*****************************************************************************)

open Types_pf5
open Utility_pf5

(*****************************************************************************)

(***********************************************************************)
(*                             vars_polish                             *)
(***********************************************************************)

let print_element key value =
  print_string (key ^ " ")

let print_map map =
    NameTable.iter print_element map

let rec vars_block list_of_block map = 
  match list_of_block with
  | [] -> map
  | (position, instruction) :: sub_list_of_block ->

    match instruction with
    | Set (name, expr) ->
      let new_map = NameTable.add name name map in
      vars_block sub_list_of_block new_map

    | Read (name) ->
      let new_map = NameTable.add name name map in
      vars_block sub_list_of_block new_map

    | Print (expr) -> 
      vars_block sub_list_of_block map

    | If (cond, block_1, block_2) ->
      let new_map = vars_block block_1 map in
      let new_map2 = vars_block block_2 new_map in
      vars_block sub_list_of_block new_map2
      
    | While (cond, block) ->
      let new_map = vars_block block map in
      vars_block sub_list_of_block new_map
      

let vars_polish (p:program) : unit = 
  let map = NameTable.empty in 
  print_string "\n"; 
  let res = vars_block p map in
  print_map res 
