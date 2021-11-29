(*****************************************************************************)

open Types_pf5
open Utility_pf5

(*****************************************************************************)

(***********************************************************************)
(*                             simpl_polish                            *)
(***********************************************************************)

 

let make_simpl_operation op expr_1_res expr_2_res = 
  (*print_string "je suis ici-1";*)
  match op with
  | Add -> Num (get_expr expr_1_res  +  get_expr expr_2_res) 
  | Sub -> Num (get_expr expr_1_res  -  get_expr expr_2_res) 
  | Mul -> Num (get_expr expr_1_res  *  get_expr expr_2_res)  
  | Div -> Num (get_expr expr_1_res  /  get_expr expr_2_res)
  | Mod -> Num (get_expr expr_1_res  mod  get_expr expr_2_res)
  
let is_constant_case op expr_1 expr_2 = 
  match op with
  | Add -> 
    if (is_Var expr_1 && is_Num expr_2 && (get_expr expr_2) = 0) 
      || (is_Var expr_2 && is_Num expr_1 && (get_expr expr_1) = 0) then true

    else false

  | Sub ->
    if (is_Var expr_1 && is_Num expr_2 && (get_expr expr_2) = 0) then true

    else false
    
  | Mul -> 
    if   (is_Var expr_1 && is_Num expr_2 && (get_expr expr_2) = 0) 
      || (is_Var expr_2 && is_Num expr_1 && (get_expr expr_1) = 0)
      || (is_Var expr_1 && is_Num expr_2 && (get_expr expr_2) = 1) 
      || (is_Var expr_2 && is_Num expr_1 && (get_expr expr_1) = 1) then true

    else false

  | Div ->
    if   (is_Var expr_1 && is_Num expr_2 && (get_expr expr_2) = 0) 
      || (is_Var expr_2 && is_Num expr_1 && (get_expr expr_1) = 0)
      || (is_Var expr_1 && is_Num expr_2 && (get_expr expr_2) = 1)
      || (is_Num expr_1 && is_Num expr_2 && (get_expr expr_2) = 0) then true

    else false

  | Mod ->
    if   (is_Var expr_1 && is_Num expr_2 && (get_expr expr_2) = 0) 
      || (is_Var expr_2 && is_Num expr_1 && (get_expr expr_1) = 0)
      || (is_Var expr_1 && is_Num expr_2 && (get_expr expr_2) = 1)
      || (is_Num expr_1 && is_Num expr_2 && (get_expr expr_2) = 0) then true

    else false 

let make_contant_case op expr_1 expr_2 = 
  (*print_string "je suis ici-1";*)   
  match op with
  | Add -> 
    if is_Var expr_1 && is_Num expr_2 then expr_1
    else expr_2

  | Sub ->
    if is_Var expr_1 && is_Num expr_2 then expr_1
    else expr_2
    
  | Mul -> 
    if is_Var expr_1 && is_Num expr_2 then
      
      if get_expr expr_2 = 0 then expr_2
      else expr_1

    else 

      if get_expr expr_1 = 0 then expr_1
      else expr_2

  | Div ->
    if is_Var expr_1 && is_Num expr_2 then 
      if get_expr expr_2 = 0 then Op (op, expr_1, expr_2)
      else expr_1
  
    else 
      if get_expr expr_1 = 0 then expr_1
      else Op (op, expr_1, expr_2)

  | Mod ->
    
    if is_Var expr_1 && is_Num expr_2 then 
      if get_expr expr_2 = 0 then Op (op, expr_1, expr_2)
      else Num (0)
  
    else 
      if get_expr expr_1 = 0 then expr_1
      else Op (op, expr_1, expr_2)

    
let make_simpl_expr op expr_1 expr_2 = 
  if is_constant_case op expr_1 expr_2 then
    make_contant_case op expr_1 expr_2
  
  else 
    if is_Num expr_1 && is_Num expr_2 then
      make_simpl_operation op expr_1 expr_2

    else  
      Op(op, expr_1, expr_2)


let rec simpl_expr expr = 
  match expr with
  | Num (value) -> Num (value)
  | Var (name) -> Var (name)
  | Op (op, expr_1, expr_2) ->
    let expr_1_res = simpl_expr expr_1 in
    let expr_2_res = simpl_expr expr_2 in
    make_simpl_expr op expr_1_res expr_2_res

let simpl_cond cond = 
  match cond with
  | (expr_1, comp, expr_2) ->
    let expr_1_res = simpl_expr expr_1 in
    let expr_2_res = simpl_expr expr_2 in
    (expr_1_res, comp, expr_2_res)

let can_simpl_block cond = 
  match cond with
  | (expr_1, comp, expr_2) ->
    
    if is_Num expr_1 && is_Num expr_2 then true
    else false

let choose_simpl_block cond = 
  match cond with
  | (expr_1, comp, expr_2) ->
    
    match comp with
    | Eq -> expr_1 = expr_2
    | Ne -> expr_1 <> expr_2
    | Lt -> expr_1 < expr_2
    | Le -> expr_1 <= expr_2
    | Gt -> expr_1 > expr_2
    | Ge -> expr_1 >= expr_2 


let rec convert_block_to_simpl_block block simpl_block = 
  match block with
  | [] -> simpl_block
  | (position, instruction)::sub_list_of_block ->

    match instruction with 
    | Set (name, expr) ->
      
      let expr_res = simpl_expr expr in
      let block_res = (position, Set(name, expr_res)) :: simpl_block in
      convert_block_to_simpl_block sub_list_of_block block_res 

    | Read (name) ->

      let block_res = (position, Read (name)) :: simpl_block  in
      convert_block_to_simpl_block sub_list_of_block block_res

    | Print (expr) ->

      
      (*test_expr expr;*)
      let expr_res = simpl_expr expr in
      (*print_string "After\n";
      test_expr expr_res;*)
      let block_res = (position, Print(expr_res)) :: simpl_block in
      convert_block_to_simpl_block sub_list_of_block block_res 

    | If (cond, block_1, block_2) ->

      let cond_res = simpl_cond cond in

      if can_simpl_block cond_res then

        if choose_simpl_block cond_res then
          let convert_sub_block_to_block = convert_block_to_simpl_block block_1 simpl_block in 
          convert_block_to_simpl_block sub_list_of_block convert_sub_block_to_block
        
        else 
          let convert_sub_block_to_block = convert_block_to_simpl_block block_2 simpl_block in
          convert_block_to_simpl_block sub_list_of_block convert_sub_block_to_block

      else

        let convert_sub_if_block_to_if_block = List.rev (convert_block_to_simpl_block block_1 []) in   
        let convert_sub_else_block_to_else_block = List.rev (convert_block_to_simpl_block block_2 []) in
        let block_res = (position, 
        If (cond_res, convert_sub_if_block_to_if_block, convert_sub_else_block_to_else_block)) :: simpl_block in
        convert_block_to_simpl_block sub_list_of_block block_res

    | While (cond, block) ->

      let cond_res = simpl_cond cond in
      if can_simpl_block cond_res then
        
        if (choose_simpl_block cond_res) = false then
          let convert_sub_block_to_block = convert_block_to_simpl_block block simpl_block in 
          convert_block_to_simpl_block sub_list_of_block convert_sub_block_to_block

        else
          let block_res = (position, While (cond_res, block)) :: simpl_block in
          convert_block_to_simpl_block sub_list_of_block block_res   

      else  
        let convert_sub_block_to_block = List.rev (convert_block_to_simpl_block block []) in 
        let block_res = (position, While (cond_res, convert_sub_block_to_block)) :: simpl_block in
        convert_block_to_simpl_block sub_list_of_block block_res

let convert_block_to_simpl_block_clean block simpl_block = 
  let list = convert_block_to_simpl_block block simpl_block in
  List.rev list

let simpl_polish (p:program) : program = 
  convert_block_to_simpl_block_clean p []
