(*****************************************************************************)

open Types_pf5
open Utility_pf5

(*****************************************************************************)

(***********************************************************************)
(*                             read_polish                             *)
(***********************************************************************)

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


let rec construct_list_of_char string list_of_char = 
  if String.length string <> List.length list_of_char then
    let list_of_char_update = String.get string (List.length list_of_char) :: list_of_char in 
    construct_list_of_char string list_of_char_update
  else
    list_of_char
    
let construct_list_of_char_clean string list_of_char = 
  let list = construct_list_of_char string list_of_char in
  List.rev list

let rec construct_expression list_of_word = 
  let first_word = List.hd list_of_word in
  (*print_string "testttt-3\n";*)
  let first_word_list_of_char = construct_list_of_char first_word [] in
  (*print_string "testttt-4\n";*)

  if is_operator first_word then

    let sub_string_1 = skip_element list_of_word 1 in
    let sub_string_2 = skip_element list_of_word 2 in
    let exp_1 = construct_expression sub_string_1 in
    let exp_2 = construct_expression sub_string_2 in
    Op (get_operator first_word, exp_1, exp_2)

  else if is_number first_word_list_of_char then
    
    (*let test = ("Mon premier mot est : " ^ first_word) in 
    print_string "\n";
    print_string test;
    print_string "\n";*)
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