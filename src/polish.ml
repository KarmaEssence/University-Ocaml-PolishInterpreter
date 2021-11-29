(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)

open Types_pf5
open Utility_pf5
open Print_polish
open Read_polish
open Simpl_polish
open Eval_polish

(*****************************************************************************)     

(***********************************************************************)
(*                              Launcher                               *)
(***********************************************************************)

let usage () =
  print_string "\n";
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "-reprint : affichage du code polish dans le terminal\n";
  print_string "-simpl : évalue le code polish\n";
  print_string "-simpl : simplifie le code polish\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish (String.trim file))
  | [|_;"-eval";file|] -> eval_polish (simpl_polish (read_polish (String.trim file)))
  | [|_;"-simpl";file|] -> print_polish (simpl_polish (read_polish (String.trim file)))
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
