open Formule

(* Compléter ce fichier avec les éléments du fichier FCC.ml du TP 3 nécessaires pour le projet,
   en plus des définitions ci-dessous. *)

(** Signe d'un littéral. *)
type signe = Plus | Moins

(** Inversion d'un signe.*)
let neg_sign (s : signe) : signe = match s with Plus -> Moins | Moins -> Plus

type litteral = signe * string
(** Type d'un littéral : produit d'un signe et d'un atome (string). *)

(** Inversion du signe d'un littéral.*)
let neg_lit ((sign, atome) : litteral) : litteral = (neg_sign sign, atome)

(** Le module Clause permet de manipuler les ensembles
    de littéraux. Il est généré via le foncteur Set.Make. *)
module Clause = Set.Make (struct
  type t = litteral

  let compare = Stdlib.compare
end)

type clause = Clause.t
(** Type synonyme : une clause est un ensemble de littéraux. *)

(** Le module FormeClausale permet de manipuler les ensembles
      de clauses. Il est généré via le foncteur Set.Make. *)
module FormeClausale = Set.Make (struct
  type t = clause
  let compare x y =
    let c = Stdlib.compare (Clause.cardinal x) (Clause.cardinal y) in
    if c <> 0 then c else Clause.compare x y
end)

type forme_clausale = FormeClausale.t
(** Type synonyme : une forme clausale est un ensemble de clauses. *)


(*au final fc ensemble de clauses qui elles sont un ensemble de litteraux*)

(** Renvoie la liste des littéraux d'une clause. *)
let clause_to_list (c : clause) : litteral list = Clause.elements c

(** Renvoie la liste des listes de littéraux des clauses d'une forme clausale. *)
let fcc_to_list (fcc : forme_clausale) : litteral list list =
  List.map Clause.elements (FormeClausale.elements fcc)
(*ou clause_yo_list car c'est la meme thina juste en haut*)
(* c'est un liste de clause donc on fait dab la liste des clause avec le element a gauche
   puis des que on a une listed des clause on parcourir la liste ordinaire avec List.map
   pour faire une liste de litterals avec Clause.elements du moment qu'on fait ca sur 
   un liste de clause donc chaqe liste devient une liste de listes *)

(** Transforme un littéral en string. *)
(* let string_of_lit ( l: litteral) : string =  *)
let string_of_lit (( signe , atome ) : litteral) : string = match signe with
  Plus -> " " ^ atome
  |Moins -> " ¬" ^ atome
  

(** Transforme une clause en string. *)
let string_of_clause (c: clause) : string ="{" ^ List.fold_left (fun acc l ->acc^ (string_of_lit l) ) "" (clause_to_list c) ^ "}"
                                             

(** Transforme une forme clausale en string. *)
let string_of_fcc ( f : forme_clausale) : string = "{" ^ List.fold_left (fun acc l -> acc^(string_of_clause l)) "" ((FormeClausale.elements f)) ^ "}"


(** Mise en FCC, étape 1 : Transforme une formule en une formule équivalente avec des opérateurs 
    de conjonction, de disjonction, de négation, Bot et Top uniquement. *)
let rec retrait_operateurs : formule -> formule = function
  (Bot | Top | Atome _) as f -> f
  | Non f1 -> Non (retrait_operateurs f1)
  | Et (f1, f2) -> Et (retrait_operateurs f1, retrait_operateurs f2)
  | Imp (f1, f2) -> Ou (Non (retrait_operateurs f1), retrait_operateurs f2)
  | Ou (f1, f2) -> Ou (retrait_operateurs f1, retrait_operateurs f2)
  (* | Equiv (_,_) -> failwith "jsp encore" *)
  | Equiv (f1,f2) -> let f1' = Imp(f1,f2) in  
                      let f2' = Imp(f2,f1) in 
                      retrait_operateurs (Et(f1',f2'))

(** Mise en FCC, étape 2 : Descend les négations dans une formule au plus profond de l'arbre syntaxique,
    en préservant les évaluations ne doit pas gerer des impliquations et equivalances. *)
let rec descente_non : formule -> formule = function
  | Non Bot -> Top
  | Non Top -> Bot
  | Non (Atome s) -> Non (Atome s)
  | Non (Non f) -> descente_non f
  | Non (Ou (f1, f2)) -> descente_non (Et (descente_non (Non f1), descente_non (Non f2)))
  | Non (Et (f1, f2)) -> descente_non (Ou (descente_non (Non f1), descente_non (Non f2)))
  | (Bot | Top | Atome _) as f -> f
  | Et (f1, f2) -> Et (descente_non f1, descente_non f2)
  | Ou (f1, f2) -> Ou (descente_non f1, descente_non f2)
  | Non (Imp (_, _)) | Imp (_, _) | Equiv(_,_) | Non(Equiv(_,_)) ->
      failwith "ne doit pas avoir Imp pour FCC.ml/decente_non "


(** Calcule la conjonction de deux formes clausales. *)
let fcc_conj (f1: forme_clausale) (f2: forme_clausale) : forme_clausale =
   FormeClausale.union f1 f2

(** Calcule la disjonction de deux formes clausales. *)
(* donc on doit faire le produit cart *)
let fcc_disj (f1: forme_clausale) (f2: forme_clausale) : forme_clausale =
  FormeClausale.fold (fun clause1 acc ->
    FormeClausale.fold (fun clause2 acc_inner ->
      let new_clause = Clause.union clause1 clause2 in
      FormeClausale.add new_clause acc_inner
    ) f2 acc
  ) f1 FormeClausale.empty


(** Mise en FCC, étape 3 : calcule la forme clausale associée à une formule. *)
let rec forme_ensembliste ( f: formule) : forme_clausale = match f with
      Top -> FormeClausale.empty
    | Bot -> FormeClausale.add Clause.empty  FormeClausale.empty
    | Atome s ->FormeClausale.add (Clause.of_list [Plus,s] )  FormeClausale.empty 
    | Non (Atome s) -> FormeClausale.add (Clause.of_list [Moins,s] )  FormeClausale.empty
    | Et(f1,f2) -> fcc_conj (forme_ensembliste f1) (forme_ensembliste f2)
    | Ou(f1,f2) -> fcc_disj (forme_ensembliste f1) (forme_ensembliste f2)
    | _ -> failwith " erreur fcc.ml forme_ensembliste cas imprevu"

(** Convertit une formule en une forme clausale conjonctive équivalente.*)
let formule_to_fcc (f : formule) : forme_clausale =
  forme_ensembliste (descente_non (retrait_operateurs f))

  