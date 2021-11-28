
(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block
type file_line = { position : position ; indentation : int; content : string}
module NameTable = Map.Make(String);;

(***********************************************************************)
(*                      A mettre dans utility-PF5.ml                   *)
(***********************************************************************)

let make_list_string_list_without_space string = 
  String.split_on_char ' ' string

let rec list_without_first_word list_of_word iteration res_list = 
  match list_of_word with
  | [] -> res_list
  | word::sub_list_of_word ->
    if iteration = 0 then
      list_without_first_word sub_list_of_word (iteration + 1) res_list
    else 
      list_without_first_word sub_list_of_word (iteration + 1) (word :: res_list)

let list_without_first_word_clean list_of_word iteration res_list = 
  let list = list_without_first_word list_of_word iteration res_list in
  List.rev list 

let make_list_string_list_without_space_and_first_word string = 
  let list_of_word = make_list_string_list_without_space string in 
  list_without_first_word_clean list_of_word 0 [] 
    
let first_word_of_file_line string = 
  List.hd (make_list_string_list_without_space string)

let construct_file_line position indentation content =  
  {position = position; indentation = indentation; content = content}

let add_file_line_to_list position indentation content list =
  let file_line = construct_file_line position indentation content in
  file_line::list

let rec get_indentation_from_line line count =
  let line_size = String.length line in
    if line_size = 0 then
      count
    else
      if String.get line 0 = ' ' then
        get_indentation_from_line (String.sub line 1 (line_size-1)) (count + 1)
      else
        count 

let rec skip_element list_of_word number =
  if number <= 0 then
    list_of_word
  else
    let list = list_without_first_word_clean list_of_word 0 [] in
    skip_element list (number-1)
            
let rec obtain_sub_block list_of_file_line indentation list_result = 
  match list_of_file_line with 
  | [] -> list_result
  | file_line :: sub_list_of_file_line ->
    if file_line.indentation < indentation then
      list_result
    else
      obtain_sub_block sub_list_of_file_line indentation (file_line :: list_result)
              
let obtain_sub_block_clean list_of_file_line indentation list_result = 
  let list = obtain_sub_block list_of_file_line indentation list_result in
  List.rev list
  
let rec obtain_else_sub_block list_of_file_line indentation list_result else_was_readed = 
  match list_of_file_line with 
  | [] -> list_result
  | file_line :: sub_list_of_file_line ->
    if else_was_readed then
      obtain_sub_block_clean sub_list_of_file_line indentation list_result
      
    else 
      let first_word = first_word_of_file_line (file_line.content) in
        
      if file_line.indentation < indentation && first_word = "ELSE" then
        obtain_else_sub_block list_of_file_line indentation list_result true
        
      else
        obtain_else_sub_block sub_list_of_file_line indentation list_result else_was_readed                     

(***********************************************************************)
(*                             print_polish                            *)
(***********************************************************************)

let make_string_operator op = 
  match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
   
    
let make_string_comparation comp = 
  match comp with
    | Eq -> " = "
    | Ne -> " <> "
    | Lt -> " < "
    | Le -> " <= "
    | Gt -> " > "
    | Ge -> " >= "



let rec make_string_expression expr = 
  match expr with
    | Num (value) -> string_of_int value
    | Var (name) -> name
    | Op (op, expr1, expr2) ->
      let string_1 = make_string_expression expr1 in
      let string_2 = make_string_expression expr2 in
      let string_op = make_string_operator op in
      string_op ^ " " ^ string_1 ^ " " ^ string_2

let make_string_condition cond = 
  match cond with
  | (expr1, comp, expr2) -> 
     let string_expr1 = make_string_expression expr1 in
     let string_expr2 = make_string_expression expr2 in
     let string_comp = make_string_comparation comp in 
     string_expr1 ^ string_comp ^ string_expr2

let rec convert_block_to_string list_of_block indentation =
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
             (*let lines = [line] @ list_file_lines in *)
             let block_lines_if = convert_block_to_string block_if (indentation + 2) in
             
             if List.length block_else > 0 then
               
               let block_lines_else = convert_block_to_string block_else (indentation + 2) in
               let line_with_if_block = [line] @ block_lines_if  in
               let else_pos = (List.length block_lines_if) + position + 1 in 
               let line_else = { position = else_pos; indentation = indentation; content = "ELSE"} in
               let lines_with_else =  line_with_if_block @ [line_else] in
               let line_else_content = lines_with_else @ block_lines_else  in
               (*block_lines_else @ lines_with_else*)
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

let rec make_space indentation result = 
  if indentation <= 0 then
    result
  else
    make_space (indentation - 1) (" " ^ result);;

let rec print_lines file_lines =
  match file_lines with 
    | [] -> print_string ""
    | element::sub_file_lines ->
      let pos = string_of_int element.position in
      let indentation =  make_space element.indentation "" in
      print_string (pos ^ ". " ^ indentation ^ element.content ^ "\n");
      print_lines sub_file_lines;;

let print_polish (p:program) : unit = 
  let file_lines = convert_block_to_string p 0 in
  print_string "\n";
  print_lines file_lines      

(***********************************************************************)
(*                             read_polish                             *)
(***********************************************************************)

let is_comp word = 
  match word with
  | "<=" -> true
  | "<"  -> true
  | ">=" -> true
  | ">"  -> true
  | "="  -> true
  | "<>" -> true
  | _  -> false
          
let is_operator word = 
  match word with
  | "+" -> true
  | "-"  -> true
  | "*" -> true
  | "/" -> true
  | "%" -> true
  | _ -> false  
                      
let is_number word =  
  match word with
  | "0" -> true
  | "1"  -> true
  | "2" -> true
  | "3" -> true
  | "4" -> true
  | "5" -> true
  | "6" -> true
  | "7" -> true
  | "8" -> true
  | "9" -> true
  | _ -> false  
             
           
let get_operator word = 
  match word with
  | "+" -> Add
  | "-"  -> Sub
  | "*" -> Mul
  | "/" -> Div
  | _ -> Mod   
             
let get_condition word =
  match word with
  | "<=" -> Le
  | "<"  -> Lt
  | ">=" -> Ge
  | ">"  -> Gt
  | "="  -> Eq
  | _  -> Ne   


let rec make_list_of_word_before_operator list_of_word list_result = 
  match list_of_word with
  | [] -> list_result
  | word :: sub_list_of_word ->
    if is_comp word then
      list_result
    else
      make_list_of_word_before_operator sub_list_of_word (word :: list_result) 

let make_list_of_word_before_operator_clean list_of_word list_result = 
  let list = make_list_of_word_before_operator list_of_word list_result in 
  List.rev list

let rec make_list_of_word_after_operator list_of_word =  
  match list_of_word with
  | [] -> []
  | word :: sub_list_of_word -> 
    if is_comp word then
      sub_list_of_word
    else
      make_list_of_word_after_operator sub_list_of_word

let rec search_string_operator list_of_word =  
  match list_of_word with
  | [] -> ""
  | word :: sub_list_of_word ->
    if is_comp word then
      word
    else
      search_string_operator sub_list_of_word

let rec construct_expression list_of_word = 
  let first_word = List.hd list_of_word in

  if is_operator first_word then

    let sub_string_1 = skip_element list_of_word 1 in
    let sub_string_2 = skip_element list_of_word 2 in
    let exp_1 = construct_expression sub_string_1 in
    let exp_2 = construct_expression sub_string_2 in
    Op (get_operator first_word, exp_1, exp_2)

  else if is_number first_word then
    Num (int_of_string first_word)

  else  
    Var (first_word)
  
let make_condition list_of_word =
  let list_1 = make_list_of_word_before_operator_clean list_of_word [] in
  let list_2 = make_list_of_word_after_operator list_of_word in
  let exp_1 = construct_expression list_1 in
  let exp_2 = construct_expression list_2 in
  (exp_1, get_condition (search_string_operator list_of_word), exp_2);;    

let rec convert_file_line_list_to_block list_of_file_line block indentation = 
  match list_of_file_line with
  | [] -> block
  | file_line::sub_list_of_file_line ->

    if file_line.indentation <> indentation then
      convert_file_line_list_to_block sub_list_of_file_line block indentation

    else
      
      let first_word = first_word_of_file_line (file_line.content) in
      let list_of_word = make_list_string_list_without_space_and_first_word (file_line.content) in

      match first_word with
      
      | "READ" -> 

        let res = (file_line.position, Read (List.hd list_of_word)) :: block in
        convert_file_line_list_to_block sub_list_of_file_line res indentation

      | "PRINT" -> 

        let res = (file_line.position, Print (construct_expression list_of_word)) :: block in
        convert_file_line_list_to_block sub_list_of_file_line res indentation

      | "IF" -> 

        let condition = make_condition list_of_word in
        let if_sub_list_of_file_line = obtain_sub_block_clean sub_list_of_file_line (file_line.indentation + 2) [] in
        let else_sub_list_of_file_line = obtain_else_sub_block sub_list_of_file_line (file_line.indentation + 2) [] false in
        
        let if_sub_block = convert_file_line_list_to_block if_sub_list_of_file_line [] (indentation + 2) in
        let else_sub_block = convert_file_line_list_to_block else_sub_list_of_file_line [] (indentation + 2) in
        let res = (file_line.position, If (condition, List.rev if_sub_block, List.rev else_sub_block)) :: block in
        convert_file_line_list_to_block sub_list_of_file_line res indentation

      | "WHILE" -> 
        
        let condition = make_condition list_of_word in
        let while_sub_list_of_file_line = obtain_sub_block_clean sub_list_of_file_line (file_line.indentation + 2) [] in

        let while_sub_block = convert_file_line_list_to_block while_sub_list_of_file_line [] (indentation + 2) in
        let res = (file_line.position, While (condition, List.rev while_sub_block)) :: block in
        convert_file_line_list_to_block sub_list_of_file_line res indentation
      
        | _ -> 

        if first_word <> "ELSE" then

          let sub_list_of_word = list_without_first_word_clean list_of_word 0 [] in
          let res = (file_line.position, Set (first_word, construct_expression sub_list_of_word)) :: block in
          convert_file_line_list_to_block sub_list_of_file_line res indentation

        else
          convert_file_line_list_to_block sub_list_of_file_line block indentation

let clean_convert_file_line_list_to_block list_of_file_line block indentation = 
  let list =  convert_file_line_list_to_block list_of_file_line block indentation in
  List.rev list   

(***********************************************************************)
(*                             simpl_polish                            *)
(***********************************************************************)

let is_Num expr = 
  match expr with
  | Num (_) -> true
  | Var (_) -> false
  | Op (_) -> false

let is_Var expr = 
  match expr with
  | Num (_) -> false
  | Var (_) -> true
  | Op (_) -> false  

let get_expr expr = 
  match expr with
  | Num (value) -> value
  | _ -> 0  

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

      let expr_res = simpl_expr expr in
      let block_res = (position, Print(expr_res)) :: simpl_block in
      convert_block_to_simpl_block sub_list_of_block block_res 

    | If (cond, block_1, block_2) ->

      let cond_res = simpl_cond cond in

      if can_simpl_block cond_res then

        if choose_simpl_block cond then
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
        
        if (choose_simpl_block cond) = false then
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

(***********************************************************************)
(*                             eval_polish                             *)
(***********************************************************************)

(*let test_expr expr = 
  match expr with
  | Num (value) -> 
    print_string "num";
  | Var (name) ->   
    print_string "var";
  | Op (op, expr_1, expr_2) ->
    print_string "op"*)
    
let rec eval_expr expr map = 
  match expr with
  | Num (value) -> 
    (*let error = "je suis ici-5\n" in
    print_string error;*)
    Num (value)
  | Var (name) -> 
    
    (*print_string name;*)
    if NameTable.mem name map then
      (*let error = "je suis ici-3\n" in
      print_string error;*)
      Num (NameTable.find name map)
    else
      (*let error = "je suis ici-4\n" in
      print_string error;*)
      Var (name)   

  | Op (op, expr_1, expr_2) ->
    (*let error = "je suis ici-6\n" in
    print_string error;*)
    let expr_1_res = eval_expr expr_1 map in
    let expr_2_res = eval_expr expr_2 map in
    make_simpl_expr op expr_1_res expr_2_res

let rec eval_block list_of_block map = 
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
        eval_block sub_list_of_block map

    | Read (name) ->

      print_string (string_of_int position ^ ". " ^ "Read : Nom de la variable : " ^ name ^ " ?\n");
      let value = read_int() in
      print_string (string_of_int position ^ ". " ^ "Read : Vous avez choisi : " ^ name ^ " := " ^ 
      string_of_int value ^ "\n\n");
      let new_map = NameTable.add name value map in
      eval_block sub_list_of_block new_map

    | Print (expr) ->

      (*test_expr expr;*)
      let expr_res = eval_expr expr map in
      if is_Num expr_res then
        let value_message = string_of_int position ^ ". " ^ "Print : " ^ string_of_int (get_expr expr_res)  ^ "\n" in
        print_string value_message;
        eval_block sub_list_of_block map
      
      else
        let error_message = "L'expression n'est pas calculé, une variable doit avoir une valuation" in
        print_string (string_of_int position ^ ". " ^ "Print : " ^ error_message ^ "\n");  
        eval_block sub_list_of_block map

    | If (cond, block_1, block_2) ->
      
      let new_map = map in
      eval_block sub_list_of_block new_map

    | While (cond, block) ->

      let new_map = map in
      eval_block sub_list_of_block new_map

let eval_polish (p:program) : unit = 
  let map = NameTable.empty in 
  print_string "\n"; 
  print_string "Début de l'évalutation\n";
  let res = eval_block p map in
  print_string "\n"; 
  print_string "Fin de l'évalutation\n" 

(***********************************************************************)
(*                Récupération des lignes dans le fichier              *)
(***********************************************************************)

(*Lis le fichier et récupère toute les lignes renvoie un type file_line*)
let rec get_file_lines_from_files file position list_of_file_lines =
  try 

    let line = input_line file in 
    let clean_line = String.trim (line) in
    let first_word = first_word_of_file_line clean_line in 

    if first_word <> "COMMENT"  then

      let indentation = get_indentation_from_line line 0 in
      let list = add_file_line_to_list position indentation clean_line list_of_file_lines in
      get_file_lines_from_files file (position + 1) (list)

    else
      get_file_lines_from_files file (position + 1) list_of_file_lines 

  with End_of_file -> list_of_file_lines 

let read_polish (filename:string) : program = 
  try

    let file = open_in filename in
    let list_of_file_lines = List.rev (get_file_lines_from_files file 1 []) in
    clean_convert_file_line_list_to_block list_of_file_lines [] 0

  with Sys_error _ -> 
    
    let () = print_endline ("Cannot read filename : " ^ filename) in
    [] ;;   

(***********************************************************************)
(*                              Launcher                               *)
(***********************************************************************)

let usage () =
  print_string "\n";
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "-reprint : affichage du code polish dans le terminal\n";
  print_string "-simpl : simplifie le code polish\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish (String.trim file))
  | [|_;"-eval";file|] -> eval_polish (simpl_polish (read_polish (String.trim file)))
  | [|_;"-simpl";file|] -> print_polish (simpl_polish (read_polish (String.trim file)))
  | [|_;"-readint";file|] -> print_string (string_of_int (read_int()) ^ "\n");
                             print_polish (read_polish (String.trim file))
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
