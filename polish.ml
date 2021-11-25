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

type file_line = { number : position ; indentation : int; content : string}
 
 let is_comp word = 
 match word with
   | "<=" -> true
   | "<"  -> true
   | ">=" -> true
   | ">"  -> true
   | "="  -> true
   | _  -> true
 
 let is_operator word = 
  match word with
    | "+" -> true
    | "-"  -> true
    | "*" -> true
    | "/" -> true
    | "%" -> true
    | _ -> false  
  ;;
  
  (*Permet de savoir le si le mot est un nombre*)
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
    ;;
  
  let get_operator word = 
  match word with
    | "+" -> Add
    | "-"  -> Sub
    | "*" -> Mul
    | "/" -> Div
    | _ -> Mod   
    ;;
    
    let get_condition word =
      match word with
       | "<=" -> Le
       | "<"  -> Lt
       | ">=" -> Ge
       | ">"  -> Gt
       | "="  -> Eq
       | _  -> Ne
    ;;
    
    let rec get_indentation_from_line line count =
      let line_size = String.length line in
      if line_size = 0 then
          count
      else
          if String.get line 0 = ' ' then
            get_indentation_from_line (String.sub line 1 (line_size-1)) (count + 1)
          else
            count
;;     

let indentation_from_line = get_indentation_from_line "  Leo" 0;;
    
    
    (*Construit une instruction avec une liste de mot sans le mot clé*)
let rec construct_expression list_of_words = 
  let first_word = List.nth list_of_words 0 in
  if is_operator first_word then
    let num_1 = List.nth list_of_words 1 in
    let num_2 = List.nth list_of_words 2 in
    let exp_1 = construct_expression [num_1] in
    let exp_2 = construct_expression [num_2] in
    Op (get_operator first_word, exp_1, exp_2)

  else if is_number first_word then
    Num (int_of_string first_word)

  else  
    Var (first_word)
    ;;
    
     let rec list_without_first_elements list current_iteration min_iteration  = 
  match list with
  | [] -> []
  | element::sublist ->
    if current_iteration < min_iteration then
      list_without_first_elements sublist (current_iteration+1) min_iteration
    else
      let resultList = list_without_first_elements sublist (current_iteration+1) min_iteration in
      element :: resultList
      ;;
      

let rec word_to_char_list word index list =
  if String.length word = List.length list then
    list
  else
    let make_string_from_char = String.make 1 (String.get word index) in
    word_to_char_list word (index + 1) 
    ((make_string_from_char)::list) ;;

word_to_char_list "1+1" 0 [];;
String.make 1 'c';;
String.split_on_char ' ' "1+1";; 
      
let make_condition list_of_words =
    let word_to_list_1 = word_to_char_list (List.nth list_of_words 0) 0 [] in
    let word_to_list_2 = word_to_char_list (List.nth list_of_words 2) 0 [] in
    let exp_1 = construct_expression word_to_list_1 in 
    let exp_2 = construct_expression word_to_list_2 in 
    (exp_1, get_condition (List.nth list_of_words 1), exp_2);;
make_condition  ["1+1"; "<="; "2"]    

let rec list_without_first list iteration = 
  match list with
  | [] -> []
  | element::sublist ->
    if iteration = 0 then
      list_without_first sublist 1
    else
      let resultList = list_without_first sublist (iteration+1) in
      element :: resultList
      ;;
      
list_without_first_elements [1;2;3] 0 1;;


      
      
(*Explore la liste des lignes et renvoie une liste de block*)
let rec convert_string_to_block list_of_lines indentation = 
  match list_of_lines with 
  | [] -> []
  | element::sub_list_of_lines -> 
      if element.indentation <> indentation then
        convert_string_to_block sub_list_of_lines indentation
      else  
        let list_result = convert_string_to_block sub_list_of_lines indentation in
        let list_of_words = String.split_on_char ' ' element.content in
        let first_word = List.nth list_of_words 0 in

        if first_word = "ELSE" || first_word = "COMMENT" then
          list_result

        else
        
          let rec obtain_if_block list_of_lines list_result = 
           match list_of_lines with 
            | [] -> []
            | element::sub_list_of_lines ->
              let list_of_words = String.split_on_char ' ' element.content in
              let first_word = List.nth list_of_words 0 in
              if first_word = "ELSE" then
                list_result
              else
                obtain_if_block sub_list_of_lines ([element] @ list_result)
          in
        
          let rec obtain_else_block list_of_lines = 
           match list_of_lines with 
            | [] -> []
            | element::sub_list_of_lines ->
              let list_of_words = String.split_on_char ' ' element.content in
              let first_word = List.nth list_of_words 0 in
              if first_word = "ELSE" then
                sub_list_of_lines
              else
                obtain_else_block sub_list_of_lines
          in
                    (*Prend une liste de mot et renvoie une instruction*)
          let make_instruction list_of_lines indentation list_of_words first_word = 
            match first_word with 
            | "READ" -> Read (List.nth list_of_words 0)
            | "PRINT" -> Print (construct_expression list_of_words)
            | "IF" -> 
              let condition = make_condition list_of_words in
              let if_lines = obtain_if_block list_of_lines [] in
              let if_block = convert_string_to_block if_lines indentation in
              let else_lines = obtain_else_block list_of_lines in
              let else_block = convert_string_to_block else_lines indentation in 
              If (condition, if_block, else_block)
            | "WHILE" -> 
              let condition = make_condition list_of_words in
              let sub_block = convert_string_to_block list_of_lines indentation in
              While (condition, sub_block)
            | _ -> 
              let sub_list_of_words = list_without_first_elements list_of_words 0 1 in
              Set (first_word, (construct_expression sub_list_of_words)) in
        
          let sub_list_of_words = list_without_first_elements list_of_words 0 1 in
          (element.number, make_instruction sub_list_of_lines (indentation + 2) 
          sub_list_of_words first_word)::list_result
        ;;

(***********************************************************************)

   let make_string_operator op = 
     match op with
       | Add -> "+"
       | Sub -> "-"
       | Mul -> "*"
       | Div -> "/"
       | Mod -> "%"
       ;;
       
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
       ;;
      
make_string_expression (Op (Add, Num 1, Num 2));;

  let make_string_condition cond = 
     match cond with
     | (expr1, comp, expr2) -> 
        let string_expr1 = make_string_expression expr1 in
        let string_expr2 = make_string_expression expr2 in
        let string_comp = make_string_comparation comp in 
        string_expr1 ^ string_comp ^ string_expr2
  ;;
        
   let rec convert_block_to_string list_of_block indentation =
     match list_of_block with
       | [] -> []
       | (position, instruction)::sub_list_of_block ->
      
          let make_string_instruction position instruction list_file_lines indentation = 
            match instruction with
              | Set (name, expr) ->
                let contents = name ^ " := " ^ make_string_expression expr in
                let line = { number = position; indentation = indentation; content = contents} in
                [line] @ list_file_lines
              | Read (name) ->
                let contents = "READ " ^ name in
                let line = { number = position; indentation = indentation; content = contents} in
                [line] @ list_file_lines
              | Print (expr) -> 
                let contents = "PRINT " ^ make_string_expression expr in
                let line = { number = position; indentation = indentation; content = contents} in
                [line] @ list_file_lines
              | If (cond, block_if, block_else) ->
                let contents = "IF " ^ make_string_condition cond in
                let line = { number = position; indentation = indentation; content = contents} in
                (*let lines = [line] @ list_file_lines in *)
                let block_lines_if = convert_block_to_string block_if (indentation + 2) in
                
                if List.length block_else > 0 then
                  
                  let block_lines_else = convert_block_to_string block_else (indentation + 2) in
                  let line_with_if_block = [line] @ block_lines_if  in
                  let else_pos = (List.length block_lines_if) + position + 1 in 
                  let line_else = { number = else_pos; indentation = indentation; content = "ELSE"} in
                  let lines_with_else =  line_with_if_block @ [line_else] in
                  let line_else_content = lines_with_else @ block_lines_else  in
                  (*block_lines_else @ lines_with_else*)
                   list_file_lines @ line_else_content
                  
                else
                  [line] @ block_lines_if
                  
              | While (cond, block) ->
                let contents = "WHILE " ^ make_string_condition cond in
                let line = { number = position; indentation = indentation; content = contents} in
                let lines = [line] @ list_file_lines in 
                let block_lines = convert_block_to_string block (indentation + 2) in
                lines @ block_lines

          in 
          let list_file_lines_all = convert_block_to_string sub_list_of_block indentation in
          let list_file_lines = make_string_instruction position instruction [] indentation in
          
          list_file_lines @ list_file_lines_all
          
        ;;


(***********************************************************************)   

let rec make_space indentation result = 
  if indentation <= 0 then
    result
  else
    make_space (indentation - 1) (" " ^ result);;

let rec print_lines file_lines =
  match file_lines with 
    | [] -> print_string ""
    | element::sub_file_lines ->
      let pos = string_of_int element.number in
      let indentation =  make_space element.indentation "" in
      print_string (pos ^ ". " ^ indentation ^ element.content ^ "\n");
      print_lines sub_file_lines;;

let print_polish (p:program) : unit = 
  let file_lines = convert_block_to_string p 0 in
  print_lines file_lines;;


(***********************************************************************)  

(*Lis le fichier et récupère toute les lignes renvoie un type file_line*)
let rec get_lines_from_files file iteration get_lines =
  try 
    let current_line = input_line file in
    let current_indentation = get_indentation_from_line current_line 0 in
    (*print_endline current_line;*) (*vérifons qu'il existe des espaces*)
    (*Format.printf "%d\n" current_indentation;*) (*comptons le nombre d'espaces*)
    get_lines_from_files file (iteration + 1) ({number = iteration; indentation = current_indentation; content = current_line}::get_lines)
  with End_of_file -> get_lines ;; 

let read_polish (filename:string) : program = 
  try
    let file = open_in filename in
    let list_of_lines = List.rev(get_lines_from_files file 0 []) in (*renvoie ce qu'il faut*)
    print_lines list_of_lines;
    convert_string_to_block list_of_lines 0
  with Sys_error _ -> 
    let () = print_endline ("Cannot read filename : " ^ filename) in
    [] ;;



let eval_polish (p:program) : unit = failwith "TODO";;

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n";;

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish (read_polish (String.trim file))
  | [|_;"--eval";file|] -> eval_polish (read_polish (String.trim file))
  | [|_;"--test";file|] -> print_polish ([(1, Set ("res", Op (Sub, Num 0, Var "n"))); (2, While ((Var "n", Lt, Num 2), [(3, Print (Var "n"))])); (4, Read "n")])
  | _ -> usage ();;

(* lancement de ce main *)
let () = main ();;
