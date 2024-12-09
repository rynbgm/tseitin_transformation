(** Le module Formule contient les types et définitions de base
    permettant la manipulation des formules de la logique propositionnelle. *)

(* Compléter ce fichier avec les éléments du fichier Formule.ml du TP 1 nécessaires pour le projet,
   en plus des définitions ci-dessous. *)

(** Type des formules de la logique propositionnelle, avec des string comme atomes. *)
type formule =
  | Bot
  | Top
  | Atome of string
  | Imp of (formule * formule)
  | Ou of (formule * formule)
  | Et of (formule * formule)
  | Non of formule
  | Equiv of (formule * formule)
  (* ya ca equivalence en plus donc faut tt regler selon ca  *)

(** Conversion d'une formule en chaîne de caractères. *)
let rec string_of_formule : formule -> string = function
  | Bot -> " ⊥ "
  | Top -> " ⊤ "
  | Atome s -> s
  | Non s -> String.concat "" [ "~" ; string_of_formule s ]
  | Et (f, g) ->
      String.concat ""
        [ "("; string_of_formule f; " * "; string_of_formule g; ")" ]
  | Ou (f, g) ->
      String.concat ""
        [ "("; string_of_formule f; " + "; string_of_formule g; ")" ]
  | Imp (f, g) ->
      String.concat ""
        [ "("; string_of_formule f; " -> "; string_of_formule g; ")" ]
  | Equiv (f, g) ->
      String.concat ""
        [ "("; string_of_formule f; " <-> "; string_of_formule g; ")"]

type interpretation = string -> bool
(** Type des interprétations. *)

(** Évalue une formule en fonction d'une interprétation. *)
let rec eval (i : interpretation) (f : formule) : bool =
match f with
| Bot -> false
| Top -> true
| Atome s -> i s
| Et (f1, f2) -> eval i f1 && eval i f2
| Ou (f1, f2) -> eval i f1 || eval i f2
| Non f1 -> not (eval i f1)
| Imp (f1, f2) -> eval i f1 <= eval i f2
| Equiv (f1, f2) -> eval i f1 = eval i f2

(** Transforme une liste de couples string en une interprétation. *)
let interpretation_of_list (ls : string list) : interpretation = 
  fun atome -> List.mem atome ls


  (** Calcule la liste (triée et sans doublon) des atomes d'une formule.*)
let rec atomes (f : formule) : string list =
  match f with
  | Bot | Top -> []
  | Atome s -> [s]
  | Non f1 -> atomes f1
  | Imp (f1, f2) | Ou (f1, f2) | Et (f1, f2) |Equiv(f1,f2) ->
    List.sort_uniq compare (List.concat [atomes f1; atomes f2])


(** Calcule la liste de toutes les sous-listes d'une liste donnée. *)
let rec all_sublists l = match l with
    [] -> [[]]
  |x::t -> let ls = all_sublists t in
                  List.map(fun l-> x::l) ls @ ls

(** Calcule toutes les interprétations pour une liste d'atomes donnée. *)
(*on doit faire celle en haut en premier*)
let all_interpretations (atomes : string list) : interpretation list =
  let sublists = all_sublists atomes in
  List.map interpretation_of_list sublists