(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(*****************************************************************************)

open Types_pf5
open Utility_pf5
open Print_polish
open Read_polish
open Simpl_polish
open Eval_polish
open Vars_polish
open Sign_polish

(*****************************************************************************)     

(***********************************************************************)
(*                              Launcher                               *)
(***********************************************************************)

(*Affiche les differentes options*)
let usage () =
  print_string "\n";
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "-reprint : affichage du code polish dans le terminal\n";
  print_string "-simpl : évalue le code polish\n";
  print_string "-simpl : simplifie le code polish\n";
  print_string "-vars : calcul statique des variables avant leurs écritures\n";
  print_string "-sign : analyse statique du signe possible des variables\n"

(*Execute une option en fonction de la ligne de commande*)
let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish (String.trim file))
  | [|_;"-eval";file|] -> eval_polish (simpl_polish (read_polish (String.trim file)))
  | [|_;"-simpl";file|] -> print_polish (simpl_polish (read_polish (String.trim file)))
  | [|_;"-vars";file|] -> vars_polish (read_polish (String.trim file))
  | [|_;"-sign";file|] -> sign_polish (read_polish (String.trim file))
  | _ -> usage ()

(* Lancement de ce main *)
let () = main ()
