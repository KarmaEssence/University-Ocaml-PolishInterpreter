(*****************************************************************************)

open Types_pf5
open Utility_pf5

(*****************************************************************************)

(***********************************************************************)
(*                             print_polish                            *)
(***********************************************************************)

(*Transforme une expression en syntaxe abstraite en syntaxe concrete*)  
let rec make_string_expression (expr : expr) : string = 
  match expr with
    | Num (value) -> string_of_int value
    | Var (name) -> name
    | Op (op, expr1, expr2) ->
      let string_1 = make_string_expression expr1 in
      let string_2 = make_string_expression expr2 in
      let string_op = make_string_operator op in
      string_op ^ " " ^ string_1 ^ " " ^ string_2

(*Transforme une condition en syntaxe abstraite en syntaxe concrete*)      
let make_string_condition (cond : cond) : string = 
  match cond with
  | (expr1, comp, expr2) -> 
     let string_expr1 = make_string_expression expr1 in
     let string_expr2 = make_string_expression expr2 in
     let string_comp = make_string_comparation comp in 
     string_expr1 ^ string_comp ^ string_expr2

(*Transforme du code en syntaxe abstraite en syntaxe concrete*)     
let rec convert_block_to_string (list_of_block : program) (indentation : int) : file_line list =
  match list_of_block with
    | [] -> []
    | (position, instruction)::sub_list_of_block ->
   
       let make_string_instruction position instruction list_file_lines indentation = 
         match instruction with
           | Set (name, expr) ->
             let contents = name ^ " := " ^ make_string_expression expr in
             let line = { position = position; indentation = indentation; content = contents} in
             [line] @ list_file_lines
           | Read (name) ->
             let contents = "READ " ^ name in
             let line = { position = position; indentation = indentation; content = contents} in
             [line] @ list_file_lines
           | Print (expr) -> 
             let contents = "PRINT " ^ make_string_expression expr in
             let line = { position = position; indentation = indentation; content = contents} in
             [line] @ list_file_lines
           | If (cond, block_if, block_else) ->
             let contents = "IF " ^ make_string_condition cond in
             let line = { position = position; indentation = indentation; content = contents} in
             let block_lines_if = convert_block_to_string block_if (indentation + 2) in
             
             if List.length block_else > 0 then
               
               let block_lines_else = convert_block_to_string block_else (indentation + 2) in
               let line_with_if_block = [line] @ block_lines_if  in
               let else_pos = (List.length block_lines_if) + position + 1 in 
               let line_else = { position = else_pos; indentation = indentation; content = "ELSE"} in
               let lines_with_else =  line_with_if_block @ [line_else] in
               let line_else_content = lines_with_else @ block_lines_else  in
               list_file_lines @ line_else_content
               
             else
               [line] @ block_lines_if
               
           | While (cond, block) ->
             let contents = "WHILE " ^ make_string_condition cond in
             let line = { position = position; indentation = indentation; content = contents} in
             let lines = [line] @ list_file_lines in 
             let block_lines = convert_block_to_string block (indentation + 2) in
             lines @ block_lines

       in 
       let list_file_lines_all = convert_block_to_string sub_list_of_block indentation in
       let list_file_lines = make_string_instruction position instruction [] indentation in
       
       list_file_lines @ list_file_lines_all
       
     ;;

(*Fais des espaces*)
let rec make_space (indentation : int) (result : string) : string = 
  if indentation <= 0 then
    result
  else
    make_space (indentation - 1) (" " ^ result);;

(*Affiche le code ligne par ligne*)    
let rec print_lines (file_lines : file_line list) : unit =
  match file_lines with 
    | [] -> print_string ""
    | element::sub_file_lines ->
      let pos = string_of_int element.position in
      let indentation =  make_space element.indentation "" in
      print_string (pos ^ ". " ^ indentation ^ element.content ^ "\n");
      print_lines sub_file_lines;;

(*Pour afficher le code polish*)      
let print_polish (p:program) : unit = 
  let file_lines = convert_block_to_string p 0 in
  print_string "\n";
  print_lines file_lines