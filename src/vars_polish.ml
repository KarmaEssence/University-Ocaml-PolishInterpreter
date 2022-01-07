(*****************************************************************************)

open Types_pf5
open Utility_pf5

(*****************************************************************************)

(***********************************************************************)
(*                             vars_polish                             *)
(***********************************************************************)

(*Ajoute a la map les variables dans les expressions*)
let rec vars_expr (expr : expr) (map : string NameTable.t) : string NameTable.t = 
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

(*Affiche toute les variables du programmes*)    
let print_first_line (key : string) (value : string) : unit =
  print_string (key ^ " ")

(*Affiche toute les variables non initialisés du programmes*)   
let print_second_line (key : string) (value : string) : unit =
  print_string value

(*Pour afficher toute les variables du programmes*)  
let find_map (map : string NameTable.t) : unit =
    NameTable.iter print_first_line map

(*Pour afficher toute les variables non initialisés du programmes*)     
let find_map2 (map : string NameTable.t) : unit =
  NameTable.iter print_second_line map

(*Rempli la map avec les variables du programme*)  
let rec vars_block (list_of_block : program) (map : string NameTable.t) : string NameTable.t = 
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
      
(*affiche deux lignes, la première affiche toute
les variables et la seconde affiche les variables non
initialisees*)
let vars_polish (p:program) : unit = 
  let map = NameTable.empty in 
  print_string "\n"; 
  let res = vars_block p map in
  find_map res;
  print_string "\n"; 
  find_map2 res

