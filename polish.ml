

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
(***********************************************************************)
(*A mettre dans utils.ml*)

(*Permet de savoir le si le mot est un opérateur*)
let is_operator word = 
  match word with
    | "+" -> true
    | "-"  -> true
    | "*" -> true
    | "/" -> true
    | "%" -> true
    | _ -> false  

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

(*Permet de savoir le si le mot est un operateur de comparaison*)   
let is_comp word = 
  match word with
    | "<=" -> true
    | "<"  -> true
    | ">=" -> true
    | ">"  -> true
    | "="  -> true
    | "<>"  -> true  
    | _ -> false  

(*Transforme le mot en operateur*)
let get_operator word = 
  match word with
    | "+" -> Add
    | "-"  -> Sub
    | "*" -> Mul
    | "/" -> Div
    | _ -> Mod  

(*Transforme le mot en operateur de comparaison*)
let get_condition word =
  match word with
    | "<=" -> Le
    | "<"  -> Lt
    | ">=" -> Ge
    | ">"  -> Gt
    | "="  -> Eq
    | _  -> Ne
   

(*Calcul le nombre d'espace avant le début de la ligne*)    
let rec get_space_num line iteration number_of_space =
  if iteration < String.length line then
     if String.get line iteration = ' ' then
       get_space_num line (iteration + 1) (number_of_space + 1)
     else
       get_space_num line (iteration + 1) number_of_space
  else
     number_of_space    

(*Retourne une liste sans les premiers elements (de i à j non compris)*)
let rec list_without_first_elements list current_iteration min_iteration  = 
match list with
| [] -> []
| element::sublist ->
  if current_iteration < min_iteration then
    list_without_first_elements sublist (current_iteration+1) min_iteration
  else
    let resultList = list_without_first_elements sublist (current_iteration+1) min_iteration in
    element :: resultList


(*Retourne l'indentation de la ligne*)
let rec get_indentation_from_line line count =
  let line_size = String.length line in
  if line_size = 0 then
    count
    else
      if String.get line 0 = ' ' then
        get_indentation_from_line (String.sub line 1 (line_size-1)) (count + 1)
      else
        count  

(*transforme le string en tableau de string1*)
let rec word_to_char_list word index list =
  if String.length word = List.length list then
    list
  else
    let make_string_from_char = String.make 1 (String.get word index) in
    word_to_char_list word (index + 1) 
    ((make_string_from_char)::list) ;;     
    

(***********************************************************************)    

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

let make_condition list_of_words =
  let word_to_list_1 = word_to_char_list (List.nth list_of_words 0) 0 [] in
  let word_to_list_2 = word_to_char_list (List.nth list_of_words 2) 0 [] in
  let exp_1 = construct_expression word_to_list_1 in 
  let exp_2 = construct_expression word_to_list_2 in 
  (exp_1, get_condition (List.nth list_of_words 1), exp_2)    
       
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

(*Lis le fichier et récupère toute les lignes renvoie un type file_line*)
let rec get_lines_from_files file iteration get_lines =
  try 
    let current_line = input_line file in
    let current_indentation = get_indentation_from_line current_line 0 in
    (*print_endline current_line;*) (*vérifons qu'il existe des espaces*)
    (*Format.printf "%d\n" current_indentation;*) (*comptons le nombre d'espaces*)
    get_lines_from_files file (iteration + 1) ({number = iteration; indentation = current_indentation; content = current_line}::get_lines)
  with End_of_file -> get_lines   

(**)
let read_polish (filename:string) : program = 
  try
    let file = open_in filename in
    let list_of_lines = List.rev(get_lines_from_files file 0 []) in (*renvoie ce qu'il faut*)
    convert_string_to_block list_of_lines 0
  with Sys_error _ -> 
    let () = print_endline ("Cannot read filename : " ^ filename) in
    [] 



let eval_polish (p:program) : unit = failwith "TODO"

let makeListOfLine (filename:string) : unit =
  try
    let file = open_in filename in 
    let list_of_content_in_file = List.rev(get_lines_from_files file 0 [])
    (*in print_string (List.nth list_of_content_in_file 0)*)
    in print_string (List.nth list_of_content_in_file 0).content
  with Sys_error _ -> print_endline ("Cannot read filename : " ^ filename)

(***********************************************************************)   

let print_polish (p:program) : unit = failwith "TODO"


(***********************************************************************)  

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish (read_polish (String.trim file))
  | [|_;"--eval";file|] -> eval_polish (read_polish (String.trim file))
  | [|_;"--test";file|] -> makeListOfLine (String.trim file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
