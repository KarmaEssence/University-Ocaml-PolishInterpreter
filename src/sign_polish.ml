(*****************************************************************************)

open Types_pf5
open Utility_pf5
open Simpl_polish
open Vars_polish

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

let op_sign expr1 op expr2 =
  match expr1, op, expr2 with
  |Zero, (Add|Sub|Mul), Zero
  |Zero, Mul, (Pos|Neg)
  |(Pos|Neg), Mul, Zero
  |Zero, Div, (Pos|Neg)-> [Zero]
  |Pos, Add, Zero
  |Zero, Add, Pos
  |Pos, Mul, Pos
  |Pos, Div, Pos
  |Neg, Mul, Neg
  |Neg, Div, Neg
  |Zero, Sub, Neg
  |Pos, Sub, (Neg|Zero)
  |Pos, Add, Pos -> [Pos]
  |Zero, Add, Neg
  |Neg, Add, (Zero|Neg)
  |Zero, Sub, Pos
  |Neg, Mul, Pos
  |Pos, Mul, Neg
  |Neg, Div, Pos
  |Pos, Div, Neg
  |Neg, Sub, Pos
  |Neg, Sub, Zero -> [Neg]
  |Pos, Sub, Pos
  |Neg, Add, Pos 
  |Pos, Add, Neg
  |Neg, Sub, Neg
  |(Pos|Neg), Mod, (Pos|Neg)
  |Zero, Mod, (Pos|Neg)-> [Neg; Zero; Pos]
  |_, _, Error
  |Error, _, _
  |_, (Div|Mod), Zero -> [Error]

let rec expr_sign expr list =
  match expr with
  |Num (value) -> if value < 0 then list@[Neg]  
  else if value > 0 then list@[Pos]
  else list@[Zero]
  |Var(name) -> []
  |Op (op, expr_1, expr_2) -> 
    let expr_1_res = expr_sign expr_1 list in
    let expr_2_res = expr_sign expr_2 list in
    op_sign expr_1_res op expr_2_res
    
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
  print_string (key ^ " " ^ (print_sign value 0))

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