(*****************************************************************************)

open Types_pf5

(*****************************************************************************)

(***********************************************************************)
(*                      A mettre dans utility-PF5.ml                   *)
(***********************************************************************)

(*Renvoie une liste de mot sans les espaces*)
let make_list_string_list_without_space string = 
  String.split_on_char ' ' string

(*Renvoie une liste de mot sans le premiers mot*)  
let rec list_without_first_word list_of_word iteration res_list = 
  match list_of_word with
  | [] -> res_list
  | word::sub_list_of_word ->
    if iteration = 0 then
      list_without_first_word sub_list_of_word (iteration + 1) res_list
    else 
      list_without_first_word sub_list_of_word (iteration + 1) (word :: res_list)

(*Renvoie une liste de mot sans le premiers mot et dans l'ordre*)       
let list_without_first_word_clean list_of_word iteration res_list = 
  let list = list_without_first_word list_of_word iteration res_list in
  List.rev list 

(*Renvoie une liste de mot sans les espaces et sans le premiers mot*)  
let make_list_string_list_without_space_and_first_word string = 
  let list_of_word = make_list_string_list_without_space string in 
  list_without_first_word_clean list_of_word 0 [] 

(*Renvoie le premier mot de la liste de ligne*)  
let first_word_of_file_line string = 
  List.hd (make_list_string_list_without_space string)

(*construit une liste de ligne*)  
let construct_file_line position indentation content =  
  {position = position; indentation = indentation; content = content}

(*Ajoute une ligne a la liste de ligne*)
let add_file_line_to_list position indentation content list =
  let file_line = construct_file_line position indentation content in
  file_line::list

(*Renvoie l indentation de la ligne*)  
let rec get_indentation_from_line line count =
  let line_size = String.length line in
    if line_size = 0 then
      count
    else
      if String.get line 0 = ' ' then
        get_indentation_from_line (String.sub line 1 (line_size-1)) (count + 1)
      else
        count 

(*renvoie la liste de mot sans "number"
premiers mots*)        
let rec skip_element list_of_word number =
  if number <= 0 then
    list_of_word
  else
    let list = list_without_first_word_clean list_of_word 0 [] in
    skip_element list (number-1)

(*Pour obtenir la liste de ligne d un "while" ou d un "if"*)     
let rec obtain_sub_block list_of_file_line indentation list_result = 
  match list_of_file_line with 
  | [] -> list_result
  | file_line :: sub_list_of_file_line ->
    if file_line.indentation < indentation then
      list_result
    else
      obtain_sub_block sub_list_of_file_line indentation (file_line :: list_result)

(*Pour obtenir la liste de ligne d un "while" ou d un "if" dans le bon ordre*)       
let obtain_sub_block_clean list_of_file_line indentation list_result = 
  let list = obtain_sub_block list_of_file_line indentation list_result in
  List.rev list
  
(*Pour obtenir la liste de ligne du "else"*)  
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

(*Permet de tester une expression dans 
cas d'un problème*)        
let rec test_expr expr = 
  match expr with
  | Num (value) -> 
    print_string "num\n";
  | Var (name) ->   
    print_string "var\n";
  | Op (op, expr_1, expr_2) ->
    print_string "op\n";
    test_expr expr_1;
    test_expr expr_2

(*Regarde l expression est
un entier*)     
let is_Num expr = 
  match expr with
  | Num (_) -> true
  | Var (_) -> false
  | Op (_) -> false

(*Regarde l expression est
une variable*)  
let is_Var expr = 
  match expr with
  | Num (_) -> false
  | Var (_) -> true
  | Op (_) -> false  

(*Recupere la valeur d une expression*)  
let get_expr expr = 
  match expr with
  | Num (value) -> value
  | _ -> 0         

(*Transforme un operateur en string*)
let make_string_operator op = 
  match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
   
(*Transforme un operateur de comparaison 
en string*)    
let make_string_comparation comp = 
  match comp with
    | Eq -> " = "
    | Ne -> " <> "
    | Lt -> " < "
    | Le -> " <= "
    | Gt -> " > "
    | Ge -> " >= "

(*regarde si le string est un 
operateur de comparaison*)   
let is_comp word = 
  match word with
  | "<=" -> true
  | "<"  -> true
  | ">=" -> true
  | ">"  -> true
  | "="  -> true
  | "<>" -> true
  | _  -> false

(*regarde si le string est un 
operateur*)  
let is_operator word = 
  match word with
  | "+" -> true
  | "-"  -> true
  | "*" -> true
  | "/" -> true
  | "%" -> true
  | _ -> false
 
(*Regarde si la liste de string est 
la representation d'un entier*)  
let rec is_number list_of_char =  
  match list_of_char with
  | [] -> 
    true
  | character :: sub_list_of_char ->
 
  (*Transforme un string de 0 a 9
  en un entier*)    
  let characters = String.make 1 character in
    match characters with
    | "0" -> is_number sub_list_of_char
    | "1"  -> is_number sub_list_of_char
    | "2" -> is_number sub_list_of_char
    | "3" -> is_number sub_list_of_char
    | "4" -> is_number sub_list_of_char
    | "5" -> is_number sub_list_of_char
    | "6" -> is_number sub_list_of_char
    | "7" -> is_number sub_list_of_char
    | "8" -> is_number sub_list_of_char
    | "9" -> is_number sub_list_of_char
    | _ -> false  
             
(*Renvoie l'operateur (en syntaxe abstraite)*)             
let get_operator word = 
  match word with
  | "+" -> Add
  | "-"  -> Sub
  | "*" -> Mul
  | "/" -> Div
  | _ -> Mod   
             
(*Renvoie l'operateur de comparaison (en syntaxe abstraite)*)  
let get_condition word =
  match word with
  | "<=" -> Le
  | "<"  -> Lt
  | ">=" -> Ge
  | ">"  -> Gt
  | "="  -> Eq
  | _  -> Ne   