(*****************************************************************************)

open Types_pf5
open Utility_pf5

(*****************************************************************************)

(***********************************************************************)
(*                             vars_polish                             *)
(***********************************************************************)

let rec vars_expr expr map = 
  match expr with
  | Num (value) -> 
    map
  | Var (name) -> 
    if NameTable.mem name map then
      map
    else
      NameTable.add name (name ^ " ") map  

  | Op (op, expr_1, expr_2) ->
    let new_map = vars_expr expr_1 map in
    vars_expr expr_2 new_map

let print_first_line key value =
  print_string (key ^ " ")

let print_second_line key value =
  print_string value

let find_map map =
    NameTable.iter print_first_line map

let find_map2 map =
  NameTable.iter print_second_line map

let rec vars_block list_of_block map = 
  match list_of_block with
  | [] -> map
  | (position, instruction) :: sub_list_of_block ->

    match instruction with
    | Set (name, expr) ->
      if NameTable.mem name map then
        let new_map = vars_expr expr map in 
        vars_block sub_list_of_block new_map
      else
      let new_map = NameTable.add name "" map in
      let new_map2 = vars_expr expr new_map in
      vars_block sub_list_of_block new_map2

    | Read (name) ->
      let new_map = NameTable.add name "" map in
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
  find_map res;
  print_string "\n"; 
  find_map2 res

