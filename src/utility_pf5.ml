(*****************************************************************************)

open Types_pf5

(*****************************************************************************)

(***********************************************************************)
(*                      A mettre dans utility-PF5.ml                   *)
(***********************************************************************)

(*Permet de tester une expression dans 
cas d'un problème*)        
let rec test_expr (expr : expr) : unit = 
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
let is_Num (expr : expr) : bool = 
  match expr with
  | Num (_) -> true
  | Var (_) -> false
  | Op (_) -> false

(*Regarde l expression est
une variable*)  
let is_Var (expr : expr) : bool = 
  match expr with
  | Num (_) -> false
  | Var (_) -> true
  | Op (_) -> false  

(*Recupere la valeur d une expression*)  
let get_expr (expr : expr) : int = 
  match expr with
  | Num (value) -> value
  | _ -> 0         

(*Transforme un operateur en string*)
let make_string_operator (op : op) : string = 
  match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
   
(*Transforme un operateur de comparaison 
en string*)    
let make_string_comparation (comp : comp) : string = 
  match comp with
    | Eq -> " = "
    | Ne -> " <> "
    | Lt -> " < "
    | Le -> " <= "
    | Gt -> " > "
    | Ge -> " >= "

(*regarde si le string est un 
operateur de comparaison*)   
let is_comp (word : string) : bool = 
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
let is_operator (word : string) : bool = 
  match word with
  | "+" -> true
  | "-"  -> true
  | "*" -> true
  | "/" -> true
  | "%" -> true
  | _ -> false
 
(*Regarde si la liste de string est 
la representation d'un entier*)  
let rec is_number (list_of_char : char list) : bool =  
  match list_of_char with
  | [] -> true
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
let get_operator (word : string) : op = 
  match word with
  | "+" -> Add
  | "-"  -> Sub
  | "*" -> Mul
  | "/" -> Div
  | _ -> Mod   
             
(*Renvoie l'operateur de comparaison (en syntaxe abstraite)*)  
let get_condition (word : string) : comp =
  match word with
  | "<=" -> Le
  | "<"  -> Lt
  | ">=" -> Ge
  | ">"  -> Gt
  | "="  -> Eq
  | _  -> Ne   

(*Renvoie la liste liste de signe en fonction de l operation
modulo*)
let op_sign_mod (expr1 : sign) (expr2 : sign) : sign list = 
  match expr1, expr2 with
  | Zero, (Pos|Neg) -> [Zero]
  | Pos, Pos -> [Zero; Pos]
  | Neg, Neg -> [Neg; Zero]
  | Pos, Neg
  | Neg, Pos -> [Neg; Zero; Pos]
  | (Pos|Neg|Zero), Zero
  | Error, _
  | _, Error -> [Error]   

(*Renvoie la liste liste de signe en fonction de l operation
division*)  
let op_sign_div (expr1 : sign) (expr2 : sign) : sign list = 
  match expr1, expr2 with
  | Pos, Pos 
  | Neg, Neg -> [Pos]
  | Neg, Pos
  | Pos, Neg -> [Neg]
  | Zero, (Pos|Neg) -> [Zero]
  | _, Zero
  | Error, _
  | _, Error -> [Error]

(*Renvoie la liste liste de signe en fonction de l operation
multiplication*)  
let op_sign_mul (expr1 : sign) (expr2 : sign) : sign list = 
  match expr1, expr2 with
  | Pos, Pos
  | Neg, Neg -> [Pos]
  | Pos, Neg
  | Neg, Pos -> [Neg]
  | Zero, (Zero | Pos | Neg) 
  | (Pos | Neg), Zero -> [Zero]
  | Error, _
  | _, Error -> [Error]  

(*Renvoie la liste liste de signe en fonction de l operation
soustraction*)  
let op_sign_sub (expr1 : sign) (expr2 : sign) : sign list = 
  match expr1, expr2 with
  | Neg, Neg
  | Pos, Pos -> [Neg; Zero; Pos]
  | Neg, Pos
  | Zero, Pos 
  | Neg, Zero -> [Neg]
  | Pos, Neg
  | Zero, Neg
  | Pos, Zero -> [Pos]
  | Zero, Zero -> [Zero]
  | Error, _
  | _, Error -> [Error]  
 
(*Renvoie la liste liste de signe en fonction de l operation
addition*)  
let op_sign_add (expr1 : sign) (expr2 : sign) : sign list = 
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

(*Renvoie la liste liste de signe en fonction de l operateur*)  
let op_sign (expr1 : sign) (op : op) (expr2 : sign) : sign list =
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

(*Renvoie true si expr1 est superieur a expr2
false sinon*)    
let comp_sign_gt (expr1 : sign) (expr2 : sign) : bool =
  match expr1, expr2 with
  |Pos, (Neg|Zero)
  |Zero, Neg -> true
  |_, _ -> false  

(*Renvoie true si expr1 est inferieur a expr2
false sinon*)    
let comp_sign_lt (expr1 : sign) (expr2 : sign) : bool =
  match expr1, expr2 with
  |(Neg|Zero), Pos
  |Neg, Zero -> true
  |_, _ -> false  
  
(*Renvoie true si expr1 est egal a expr2
false sinon*)    
let comp_sign_eq (expr1 : sign) (expr2 : sign) : bool =
  match expr1, expr2 with
  |Pos, Pos
  |Neg, Neg
  |Zero, Zero -> true
  |_, _ -> false   

(*Renvoie true si la condition est vrai pour expr1 et expr2
false sinon*)    
let comp_sign (expr1 : sign) (comp : comp) (expr2 : sign) : bool =
  match comp with
  | (Eq | Ne) -> comp_sign_eq expr1 expr2
  | Lt -> comp_sign_lt expr1 expr2
  | Le -> (comp_sign_eq expr1 expr2) || (comp_sign_lt expr1 expr2)
  | Gt -> comp_sign_gt expr1 expr2
  | Ge -> (comp_sign_eq expr1 expr2) || (comp_sign_gt expr1 expr2)

(*Renvoie un tableau de sign en fonction de l operation entre deux entiers*)  
let obtains_sign_from_number (op : op) (num_1 : int) (num_2 : int) : sign list = 
  match op with 
  | Add -> [Pos]
  | Sub -> 
    if num_1 - num_2 = 0 then
      [Zero]
    else if num_1 - num_2 < 0 then
      [Neg]
    else [Pos]

  | Mul -> 
    if num_1 = 0 || num_2 = 0 then
      [Zero]
    else if num_1 * num_2 < 0 then
      [Neg]
    else [Pos]

  | Div -> 
    if num_2 = 0 then
      [Error]
    else if num_1 * num_2 < 0 then
      [Neg]
    else [Pos]
   
  | Mod ->
    if num_2 = 0 then
      [Error]
    else if num_1 * num_2 < 0 then
      [Neg]
    else [Pos]

(*Verifie que x est inferieur a y*)    
let is_sign_inferior_of (x : sign) (y : sign) : bool = 
  match x, y with
  | Neg, (Zero | Pos | Error) 
  | Zero, (Pos | Error) 
  | Pos, (Error)
  | Neg, Neg 
  | Zero, Zero 
  | Pos, Pos  
  | Error, Error -> true
  | _, _ -> false     

(*Renvoie une liste de mot sans les espaces*)
let make_list_string_list_without_space (string : string) : string list = 
  String.split_on_char ' ' string

(*Renvoie une liste de mot sans le premiers mot*)  
let rec list_without_first_word (list_of_word : string list) (iteration : int) (res_list : string list) : string list = 
  match list_of_word with
  | [] -> res_list
  | word::sub_list_of_word ->
    if iteration = 0 then
      list_without_first_word sub_list_of_word (iteration + 1) res_list
    else 
      list_without_first_word sub_list_of_word (iteration + 1) (word :: res_list)

(*Renvoie une liste de mot sans le premiers mot et dans l'ordre*)       
let list_without_first_word_clean (list_of_word : string list) (iteration : int) (res_list : string list) : string list = 
  let list = list_without_first_word list_of_word iteration res_list in
  List.rev list 

(*Renvoie une liste de mot sans les espaces et sans le premiers mot*)  
let make_list_string_list_without_space_and_first_word (string : string) : string list = 
  let list_of_word = make_list_string_list_without_space string in 
  list_without_first_word_clean list_of_word 0 [] 

(*Renvoie le premier mot de la liste de ligne*)  
let first_word_of_file_line (string : string) : string = 
  List.hd (make_list_string_list_without_space string)

(*construit une liste de ligne*)  
let construct_file_line (position : int) (indentation : int) (content : string) : file_line =  
  {position = position; indentation = indentation; content = content}

(*Ajoute une ligne a la liste de ligne*)
let add_file_line_to_list (position : int) (indentation : int) (content : string) (list : file_line list) : file_line list =
  let file_line = construct_file_line position indentation content in
  file_line::list

(*Renvoie l indentation de la ligne*)  
let rec get_indentation_from_line (line : string) (count : int) : int =
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
let rec skip_element (list_of_word : string list) (number : int) : string list =
  if number <= 0 then
    list_of_word
  else
    let list = list_without_first_word_clean list_of_word 0 [] in
    skip_element list (number-1)

(*Pour obtenir la liste de ligne d un "while" ou d un "if"*)     
let rec obtain_sub_block (list_of_file_line : file_line list) (indentation : int) (list_result : file_line list) : file_line list = 
  match list_of_file_line with 
  | [] -> list_result
  | file_line :: sub_list_of_file_line ->
    if file_line.indentation < indentation then
      list_result
    else
      obtain_sub_block sub_list_of_file_line indentation (file_line :: list_result)

(*Pour obtenir la liste de ligne d un "while" ou d un "if" dans le bon ordre*)       
let obtain_sub_block_clean (list_of_file_line : file_line list) (indentation : int) (list_result : file_line list) : file_line list = 
  let list = obtain_sub_block list_of_file_line indentation list_result in
  List.rev list
  
(*Pour obtenir la liste de ligne du "else"*)  
let rec obtain_else_sub_block (list_of_file_line : file_line list) (indentation : int) (list_result : file_line list) (else_was_readed : bool) : file_line list = 
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

(*Renvoie true si deux liste sont identique, false sinon*)        
let rec compare_list (list_1 : 'a list) (list_2 : 'a list) : bool = 
  match list_1, list_2 with
  | [], [] -> true
  | list, [] 
  | [], list -> false  
  | x:: sub_list_1, y:: sub_list_2 -> 
    if x = y then compare_list sub_list_1 sub_list_2
    else false 

(*Renvoie une sign list sans doublons*)      
let rec avoid_duplicate_sign_type_in_list (list : sign list) (list_res : sign list) : sign list =
  match list with 
  | [] -> list_res
  | x :: sub_list ->
    if List.mem x list_res then
        avoid_duplicate_sign_type_in_list sub_list list_res
    else 
        avoid_duplicate_sign_type_in_list sub_list (x::list_res)          
      

(*Trie une liste rapidement*)        
let rec quicksort (list : 'a list) : 'a list = 
  match list with
  | [] -> []
  | [element] -> [element]
  | _ ->
    let list_temp = List.tl list in
    let first_element = List.nth list 0 in 
    let rec copy word list first_element = 
    if word = "inf" then 
      match list with
      | [] -> [] 
      | element :: sub_list -> 
        if is_sign_inferior_of element first_element then 
          [element] @ copy word sub_list first_element 

        else copy word sub_list first_element
      
    else 
      match list with
      | [] -> [] 
      | element :: sub_list -> 
        if not (is_sign_inferior_of element first_element) then 
          [element] @ copy word sub_list first_element
                    
        else copy word sub_list first_element

    in 
    let left_sort = quicksort (copy "inf" list_temp first_element) in
    let right_sort = quicksort (copy "sup" list_temp first_element)  in
    left_sort @ [first_element] @ right_sort        

