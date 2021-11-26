
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

(***********************************************************************)
(*                      A mettre dans utility-PF5.ml                   *)
(***********************************************************************)

let make_list_string_list_without_space string = 
  let list_of_words = String.split_on_char ' ' string in
  print_string ("Premier mot de la ligne : " ^ List.hd list_of_words ^ "\n");
  List.hd list_of_words

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

(***********************************************************************)
(*                             print_polish                            *)
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
      let pos = string_of_int element.position in
      let indentation =  make_space element.indentation "" in
      print_string (pos ^ ". " ^ indentation ^ element.content ^ "\n");
      print_lines sub_file_lines;;

(***********************************************************************)
(*                             read_polish                             *)
(***********************************************************************)      

(***********************************************************************)
(*                Récupération des lignes dans le fichier              *)
(***********************************************************************)

(*Lis le fichier et récupère toute les lignes renvoie un type file_line*)
let rec get_file_lines_from_files file position list_of_file_lines =
  print_string "\n";
  try 

    let line = input_line file in 
    let clean_line = String.trim (line) in

    
    

    let first_word = make_list_string_list_without_space clean_line in 
    if first_word <> "COMMENT"  then

      let indentation = get_indentation_from_line line 0 in

      print_string ("Position : " ^ (string_of_int position) ^ "\n") ;
      print_string ("Indentation : " ^ (string_of_int indentation) ^ "\n") ;
      print_string ("Content : " ^ clean_line ^ "\n");

      let list = add_file_line_to_list position indentation clean_line list_of_file_lines in
      get_file_lines_from_files file (position + 1) (list)

    else
      
      get_file_lines_from_files file (position + 1) list_of_file_lines 

  with End_of_file -> list_of_file_lines 

let read_polish (filename:string) : program = 
  try
    let file = open_in filename in
    let list_of_file_lines = List.rev (get_file_lines_from_files file 0 []) in
    print_lines list_of_file_lines;
    []
  with Sys_error _ -> 
    let () = print_endline ("Cannot read filename : " ^ filename) in
    [] ;;

(***********************************************************************)

let print_polish (p:program) : unit = failwith "TODO"

let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish (read_polish (String.trim file))
  | [|_;"--eval";file|] -> eval_polish (read_polish (String.trim file))
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
