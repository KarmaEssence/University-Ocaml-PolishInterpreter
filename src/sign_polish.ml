(*****************************************************************************)

open Types_pf5
open Utility_pf5
open Simpl_polish
open Vars_polish
open Eval_polish

(*****************************************************************************)

(***********************************************************************)
(*                             sign_polish                             *)
(***********************************************************************)

let rec make_sign_comparaison list1 list2 comp =
  match list1 with
  |[] -> true
  |x:: sub_list_1 -> 
    match list2 with
    |[] -> true
    |y:: sub_list_2 -> 
      if comp_sign x comp y then
        make_sign_comparaison sub_list_1 sub_list_2 comp
      else 
        false   


let rec make_sign_operation list1 list2 op list_res =  
  match list1 with
  |[] -> list_res
  |x:: sub_list_1 -> 

    let rec make_sub_sign_operation x list2 op list_res = 
      match list2 with
      |[] -> list_res
      |y:: sub_list_2 -> 
        let t = 0 in
        let res = op_sign x op y in
        let list_new_res = avoid_duplicate_sign_type_in_list res list_res in
        make_sub_sign_operation x sub_list_2 op list_new_res

    in 
    let list_new_res = make_sub_sign_operation x list2 op list_res in
    make_sign_operation sub_list_1 list2 op list_new_res  

let rec expr_sign expr map =
  match expr with
  |Num (value) -> 
    if value < 0 then [Neg]  
    else if value > 0 then [Pos]
    else [Zero]
  |Var(name) -> 
    if NameTable.mem name map then
      NameTable.find name map
    else []
  |Op (op, expr_1, expr_2) -> 
    if is_Num expr_1 && is_Num expr_2 then
      obtains_sign_from_number op (get_expr expr_1) (get_expr expr_2)

    else   
      let expr_1_res = expr_sign expr_1 map in
      let expr_2_res = expr_sign expr_2 map in
      let list_res = make_sign_operation expr_1_res expr_2_res op [] in
      quicksort list_res      
    
let apply_condition_sign_type cond map =
  match cond with
  | (expr_1, comp, expr_2) ->
    let expr_1_res = expr_sign expr_1 map in
    let expr_2_res = expr_sign expr_2 map in
    make_sign_comparaison expr_1_res expr_2_res comp

let has_error_condition_sign_type cond map =
  match cond with
  | (expr_1, comp, expr_2) ->
    let expr_1_res = expr_sign expr_1 map in
    let expr_2_res = expr_sign expr_2 map in
    if List.mem Error expr_1_res || List.mem Error expr_2_res then
      true
    else false    

let can_apply_condition_sign_type cond map =
  match cond with
  | (expr_1, comp, expr_2) ->
    let expr_1_res = expr_sign expr_1 map in
    let expr_2_res = expr_sign expr_2 map in
    if (List.length expr_1_res) > 2 || (List.length expr_1_res) > 2 || List.mem Error expr_1_res || List.mem Error expr_2_res then
     false
    else true

let rec print_sign list acc = 
  match list with
  |[] -> acc
  |x::reste_list -> 
    match x with
    | Neg -> print_sign reste_list (acc ^ "-")
    | Zero -> print_sign reste_list (acc ^ "0")
    | Pos -> print_sign reste_list (acc ^ "+")
    | Error -> print_sign reste_list (acc ^ "!")    

let print_line key value =
  if List.length value > 0 then
    let () = print_string (key ^ " " ^ (print_sign value "")) in
    print_string "\n"
  else 
    print_string ""

let print_error key value =
  if List.length value = 0 then
    let () = print_string ("divbyzero " ^ key) in
    print_string "\n";
  else 
    print_string "" 

let element_contains_error key value = List.length value == 0

let find_map map =
  NameTable.iter print_line map;
  print_string "\n"; 
  if NameTable.exists element_contains_error map then
    let () = NameTable.iter print_error map in
    print_string "\n"
  else
    print_string "safe\n" 

let rec sign_block list_of_block map = 
  match list_of_block with
  | [] -> map
  | (position, instruction) :: sub_list_of_block ->
  
    match instruction with
    | Set (name, expr) ->
      let new_map = NameTable.add name (expr_sign expr map) map in
      
      if not (NameTable.exists element_contains_error map) &&
         List.mem Error (NameTable.find name new_map) then

        let new_map_with_error = NameTable.add (string_of_int position) [] new_map in
        sign_block sub_list_of_block new_map_with_error

      else   
        sign_block sub_list_of_block new_map
      
    | Read (name) ->
      let new_map = NameTable.add name [Neg; Zero; Pos] map in
      sign_block sub_list_of_block new_map

    | Print (expr) ->
      sign_block sub_list_of_block map
  
    | If (cond, block_1, block_2) ->
      if can_apply_condition_sign_type cond map then
        if apply_condition_sign_type cond map then
          let new_map = sign_block block_1 map in
          sign_block sub_list_of_block new_map
        else
          let new_map = sign_block block_2 map in
          sign_block sub_list_of_block new_map

          
      else  
        if not (NameTable.exists element_contains_error map) && has_error_condition_sign_type cond map then 
          let map_with_error = NameTable.add (string_of_int position) [] map in
          sign_block sub_list_of_block map_with_error
          
        else
          let new_map = sign_block block_1 map in
          let new_map2 = sign_block block_2 new_map in
          sign_block sub_list_of_block new_map2
  
    | While (cond, block) ->
      if apply_condition_sign_type cond map then

        let new_map = sign_block block map in
        if NameTable.equal compare_list map new_map then
          let () = print_string "random.org\n" in
          
          sign_block sub_list_of_block map
        else 
          let () = print_string "je suis la \n" in
          sign_block list_of_block new_map

      else  
        if not (NameTable.exists element_contains_error map) && has_error_condition_sign_type cond map then 
          let map_with_error = NameTable.add (string_of_int position) [] map in
          sign_block sub_list_of_block map_with_error
          
        else
          sign_block sub_list_of_block map
      
(*Permet d evaluer un code en syntaxe abstraite*)        
let sign_polish (p:program) : unit = 
  let map = NameTable.empty in 
  print_string "\n"; 
  let res = sign_block p map in
  find_map res;
  print_string "\n"