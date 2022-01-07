(*****************************************************************************)

open Types_pf5
open Utility_pf5
open Simpl_polish

(*****************************************************************************)

(***********************************************************************)
(*                             eval_polish                             *)
(***********************************************************************)


(*Evalue le code en syntaxe en abstraite en fonction
des valeurs contenu dans les variables*)    
let rec eval_expr (expr : expr) (map : int NameTable.t) : expr = 
  match expr with
  | Num (value) -> 
    Num (value)
  | Var (name) -> 
    
    if NameTable.mem name map then
      Num (NameTable.find name map)
    else
      Var (name)   

  | Op (op, expr_1, expr_2) ->
    let expr_1_res = eval_expr expr_1 map in
    let expr_2_res = eval_expr expr_2 map in
    make_simpl_expr op expr_1_res expr_2_res

(*Evalue une condition*)    
let eval_condition (cond : cond) (map : int NameTable.t) : cond = 
  match cond with
  | (expr_1, comp, expr_2) ->
    let expr_1_res = eval_expr expr_1 map in
    let expr_2_res = eval_expr expr_2 map in
    (expr_1_res, comp, expr_2_res)   

(*Evalue un programme*)    
let rec eval_block (list_of_block : program) (map : int NameTable.t) : int NameTable.t = 
  match list_of_block with
  | [] -> map
  | (position, instruction) :: sub_list_of_block ->

    match instruction with
    | Set (name, expr) ->
      
      let expr_res = eval_expr expr map in
      if is_Num expr_res then 
        let new_map = NameTable.add name (get_expr expr_res) map in
        eval_block sub_list_of_block new_map

      else
        let error_message = "L'expression n'est pas calculé, une variable doit avoir une valuation" in
        print_string (string_of_int position ^ ". " ^ name ^" : " ^ error_message ^ "\n");  
        exit 1

    | Read (name) ->

      print_string (string_of_int position ^ ". " ^ "Read : Nom de la variable : " ^ name ^ " ?\n");
      let value = read_int() in
      print_string (string_of_int position ^ ". " ^ "Read : Vous avez choisi : " ^ name ^ " := " ^ 
      string_of_int value ^ "\n\n");
      let new_map = NameTable.add name value map in
      eval_block sub_list_of_block new_map

    | Print (expr) ->

      let expr_res = eval_expr expr map in
      if is_Num expr_res then
        let value_message = string_of_int position ^ ". " ^ "Print : " ^ string_of_int (get_expr expr_res)  ^ "\n" in
        print_string value_message;
        eval_block sub_list_of_block map
      
      else
       
        let error_message = "L'expression n'est pas calculé, une variable doit avoir une valuation" in
        print_string (string_of_int position ^ ". " ^ "Print : " ^ error_message ^ "\n");  
        exit 1

    | If (cond, block_1, block_2) ->

      let cond_res = eval_condition cond map in
      if can_simpl_block cond_res then

        if choose_simpl_block cond_res then

          let new_map = eval_block block_1 map in
          eval_block sub_list_of_block new_map
        
        else 
          let new_map = eval_block block_2 map in
          eval_block sub_list_of_block new_map

      else
        let error_message = "L'expression n'est pas calculé, une variable doit avoir une valuation" in
        print_string (string_of_int position ^ ". " ^ "If : " ^ error_message ^ "\n");  
        exit 1     

    | While (cond, block) ->

      let cond_res = eval_condition cond map in
      if can_simpl_block cond_res then
        
        if (choose_simpl_block cond_res) = false then
          eval_block sub_list_of_block map

        else
          let new_map = eval_block block map in
          eval_block list_of_block new_map
          

      else  
        let error_message = "L'expression n'est pas calculé, une variable doit avoir une valuation" in
        print_string (string_of_int position ^ ". " ^ "While : " ^ error_message ^ "\n");  
        exit 1

(*Permet d evaluer un code en syntaxe abstraite*)        
let eval_polish (p:program) : unit = 
  let map = NameTable.empty in 
  print_string "\n"; 
  print_string "Début de l'évalutation\n";
  let res = eval_block p map in
  print_string "\n"; 
  print_string "Fin de l'évalutation\n"