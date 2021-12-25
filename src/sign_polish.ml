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

let rec print_sign list acc = 
  match list with
  |[] -> acc
  |x::reste_list -> 
    match x with
    | Neg -> print_sign reste_list (acc ^ "-")
    | Zero -> print_sign reste_list (acc ^ "0")
    | Pos -> print_sign reste_list (acc ^ "+")
    | Error -> print_sign reste_list (acc ^ "!")

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
  | Add -> op_sign_add expr1 expr2
  | Sub -> op_sign_sub expr1 expr2
  | Mul -> op_sign_mul expr1 expr2
  | Div -> op_sign_div expr1 expr2
  | Mod -> op_sign_mod expr1 expr2

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
    match list2 with
    |[] -> list_res
    |y:: sub_list_2 -> 
      let res = op_sign x op y in
      let new_res = avoid_duplicate_sign_type_in_list res list_res in
      make_sign_operation sub_list_1 sub_list_2 op new_res

let rec expr_sign expr =
  match expr with
  |Num (value) -> 
    if value < 0 then [Neg]  
    else if value > 0 then [Pos]
    else [Zero]
  |Var(name) -> []
  |Op (op, expr_1, expr_2) -> 
    let expr_1_res = expr_sign expr_1 in
    let expr_2_res = expr_sign expr_2 in
    make_sign_operation expr_1_res expr_2_res op []
    
let cond_sign cond map boolean =
  match cond with
  |(expr_1, comp, expr_2) -> 
    if boolean then 
      let new_map = NameTable.add "" (expr_sign expr_1) map in
      new_map
    else
      let new_map = NameTable.add "" (expr_sign expr_2) map in
      new_map    

let print_line key value =
  print_string (key ^ " " ^ (print_sign value "0"))

let find_map map =
  NameTable.iter print_line map
  
let rec sign_block list_of_block map = 
  match list_of_block with
  | [] -> map
  | (position, instruction) :: sub_list_of_block ->
  
    match instruction with
    | Set (name, expr) ->
      let new_map = NameTable.add name (expr_sign expr) map in
      sign_block sub_list_of_block new_map
      
    | Read (name) ->
      sign_block sub_list_of_block map
  
    | Print (expr) ->
      sign_block sub_list_of_block map
  
    | If (cond, block_1, block_2) ->
      let cond_res = eval_condition cond map in
      if choose_simpl_block cond_res then
        let new_map = sign_block block_1 map in
          cond_sign cond map (choose_simpl_block cond_res);
          sign_block sub_list_of_block new_map
        
        else 
          let new_map = sign_block block_2 map in
          cond_sign cond map (choose_simpl_block cond_res);
          sign_block sub_list_of_block new_map
      
    | While (cond, block) ->
      let cond_res = eval_condition cond map in
      if (choose_simpl_block cond_re) = false then
        let new_map = sign_block block_1 map in
        sign_block sub_list_of_block map
        
        else 
          let new_map = sign_block block map in
          cond_sign cond map (choose_simpl_block cond_res);
          sign_block sub_list_of_block new_map
      

(*Permet d evaluer un code en syntaxe abstraite*)        
let sign_polish (p:program) : unit = 
  let map = NameTable.empty in 
  print_string "\n"; 
  let res = sign_block p map in
  print_string find_map res;
  print_string "\n"