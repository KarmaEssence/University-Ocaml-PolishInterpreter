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

let op_sign_mod expr1 expr2 = 
  match expr1, expr2 with
  | Zero, (Pos|Neg) -> [Zero]
  | Pos, Pos -> [Zero; Pos]
  | Neg, Neg -> [Neg; Zero]
  | Pos, Neg
  | Neg, Pos -> [Neg; Zero; Pos]
  | (Pos|Neg|Zero), Zero
  | Error, _
  | _, Error -> [Error]     

let op_sign_div expr1 expr2 = 
  match expr1, expr2 with
  | Pos, Pos 
  | Neg, Neg -> [Pos]
  | Neg, Pos
  | Pos, Neg -> [Neg]
  | Zero, (Pos|Neg) -> [Zero]
  | _, Zero
  | Error, _
  | _, Error -> [Error]

let op_sign_mul expr1 expr2 = 
  match expr1, expr2 with
  | Pos, Pos
  | Neg, Neg -> [Pos]
  | Pos, Neg
  | Neg, Pos -> [Neg]
  | Zero, (Zero | Pos | Neg) 
  | (Pos | Neg), Zero -> [Zero]
  | Error, _
  | _, Error -> [Error]  
  
let op_sign_sub expr1 expr2 = 
  match expr1, expr2 with
  | Neg, Neg
  | Pos, Pos -> [Neg; Zero; Neg]
  | Neg, Pos
  | Zero, Pos 
  | Neg, Zero -> [Neg]
  | Pos, Neg
  | Zero, Neg
  | Pos, Zero -> [Pos]
  | Zero, Zero -> [Zero]
  | Error, _
  | _, Error -> [Error]  

let op_sign_add expr1 expr2 = 
  match expr1, expr2 with
  | Pos, Pos 
  | Pos, Zero
  | Zero, Pos -> [Pos]
  | Neg, Neg 
  | Zero, Neg
  | Neg, Zero -> [Neg]
  | Zero, Zero -> [Zero]
  | Pos, Neg
  | Neg, Pos -> [Neg; Zero; Pos]
  | Error, _
  | _, Error -> [Error]  

let op_sign expr1 op expr2 =
  match op with
  | Add -> 
    op_sign_add expr1 expr2
  | Sub -> 
    op_sign_sub expr1 expr2
  | Mul -> 
    op_sign_mul expr1 expr2
  | Div -> 
    op_sign_div expr1 expr2
   
  | Mod ->
    op_sign_mod expr1 expr2

let comp_sign_gt expr1 expr2 =
  match expr1, expr2 with
  |Pos, (Neg|Zero)
  |Zero, Neg -> true
  |_, _ -> false  

let comp_sign_lt expr1 expr2 =
  match expr1, expr2 with
  |(Neg|Zero), Pos
  |Neg, Zero -> true
  |_, _ -> false  

let comp_sign_eq expr1 expr2 =
  match expr1, expr2 with
  |Pos, Pos
  |Neg, Neg
  |Zero, Zero -> true
  |_, _ -> false   

let comp_sign expr1 comp expr2 =
  match comp with
  | Eq -> comp_sign_eq expr1 expr2
  | Ne -> not(comp_sign_eq expr1 expr2)
  | Lt -> comp_sign_lt expr1 expr2
  | Le -> (comp_sign_eq expr1 expr2) || (comp_sign_lt expr1 expr2)
  | Gt -> comp_sign_gt expr1 expr2
  | Ge -> (comp_sign_eq expr1 expr2) || (comp_sign_gt expr1 expr2)

let rec avoid_duplicate_sign_type_in_list list list_res =
  match list with 
  | [] -> list_res
  | x :: sub_list ->
    if List.mem x list_res then
        avoid_duplicate_sign_type_in_list sub_list list_res
    else 
        avoid_duplicate_sign_type_in_list sub_list (x::list_res)

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
    let expr_1_res = expr_sign expr_1 map in
    let expr_2_res = expr_sign expr_2 map in
    make_sign_operation expr_1_res expr_2_res op []

(*let rec make_sign_comparaison list1 list2 comp =
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
    else true*)

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
  print_string (key ^ " " ^ (print_sign value ""));
  print_string "\n"

let find_map map =
  NameTable.iter print_line map
  
let rec sign_block list_of_block map = 
  match list_of_block with
  | [] -> map
  | (position, instruction) :: sub_list_of_block ->
  
    match instruction with
    | Set (name, expr) ->
      (*let get_sign_list = expr_sign expr map in*)
      let new_map = NameTable.add name (expr_sign expr map) map in
      sign_block sub_list_of_block new_map
      
    | Read (name) ->
      let new_map = NameTable.add name [Neg; Zero; Pos] map in
      sign_block sub_list_of_block new_map

    | Print (expr) ->
      sign_block sub_list_of_block map
  
    | If (cond, block_1, block_2) ->
      (*if can_apply_condition_sign_type cond map then
        if apply_condition_sign_type cond map then
          let new_map = sign_block block_1 map in
          sign_block sub_list_of_block new_map
        else
          let new_map = sign_block block_2 map in
          sign_block sub_list_of_block new_map

          
      else  
        if has_error_condition_sign_type cond map then 
          sign_block sub_list_of_block map
        else
          let new_map = sign_block block_1 map in
          let new_map2 = sign_block block_2 new_map in
          sign_block sub_list_of_block new_map2*)
    
      sign_block sub_list_of_block map
    | While (cond, block) ->
      sign_block sub_list_of_block map
      

(*Permet d evaluer un code en syntaxe abstraite*)        
let sign_polish (p:program) : unit = 
  let map = NameTable.empty in 
  print_string "\n"; 
  let res = sign_block p map in
  find_map res;
  print_string "\n"