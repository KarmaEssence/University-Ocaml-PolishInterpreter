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
    | Neg -> type_sign reste_list (acc ^ "-")
    | Zero -> type_sign reste_list (acc ^ "0")
    | Pos -> type_sign reste_list (acc ^ "+")
    | Error -> type_sign reste_list (acc ^ "!")

let rec op_sign expr1 op expr2 =
  match expr1, op, expr2 with
  |Zero, Add, Zero
  |Zero, Sub, Zero
  |Pos, Mul, Zero
  |Neg, Mul, Zero
  |Zero, Div, Pos
  |Zero, Div, Neg
  |Zero, Mul, Zero -> [Zero]
  |Pos, Add, Zero
  |Zero, Add, Pos
  |Pos, Mul, Pos
  |Pos, Div, Pos
  |Neg, Mul, Neg
  |Neg, Div, Neg
  |Pos, Add, Pos -> [Pos]
  |Neg, Add, Neg
  |Zero, Sub, Neg
  |Neg, Mul, Pos
  |Pos, Mul, Neg
  |Neg, Div, Pos
  |Pos, Div, Neg
  |Neg, Sub, Zero -> [Neg]
  |Pos, Add, Neg
  |Pos, Mod, Pos
  |Pos, Mod, Neg
  |Neg, Mod, Pos
  |Neg, Mod, Neg
  |Zero, Mod, Pos
  |Zero, Mod, Neg
  |Pos, Mod, Zero
  |Neg, Mod, Zero
  |Zero, Mod, Zero
  |Neg, Add, Pos -> [Neg, Zero, Pos]
  |Zero, Div, Zero -> [Error]

let rec expr_sign expr =
  match expr with
  |Num (value) -> if value < 0 then [Neg]  
  else if value > 0 then [Pos]
  else [Zero]
  |Var(name) -> []
  |Op (op, expr_1, expr_2) -> 
    let expr_1_res = expr_sign expr_1 in
    let expr_2_res = expr_sign expr_2 in
    op_sign expr_1_res op expr_2_res

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
      
        
  
    | While (cond, block) ->
  
      

(*Permet d evaluer un code en syntaxe abstraite*)        
let sign_polish (p:program) : unit = 
  let map = NameTable.empty in 
  print_string "\n"; 
  let res = sign_block p map in
  print_string find_map res;
  print_string "\n"