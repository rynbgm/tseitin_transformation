open FCC


(** Simplifie la forme clausale fcc en considérant que le littéral lit est vrai *)
let simplif_fcc (fcc : forme_clausale) (l : litteral) : forme_clausale =
  FormeClausale.map
    (Clause.remove (neg_lit l))
    (FormeClausale.filter (function c -> not (Clause.mem l c)) fcc)


(** Applique l'algorithme DPLL pour déterminer si une fcc est satisfaisable. *)
let rec dpll_sat (fcc : forme_clausale) : bool =
  match FormeClausale.min_elt_opt fcc with
  | None -> true
  | Some c -> (
      match Clause.min_elt_opt c with
      | None -> false
      | Some l ->
          dpll_sat (simplif_fcc fcc l) || dpll_sat (simplif_fcc fcc (neg_lit l))
      )

(* truc en plus pour convertir *)
let lit_to_couple = function Plus, s -> (s, true) | Moins, s -> (s, false)

(** Applique l'algorithme DPLL pour déterminer si une fcc est satisfaisable, renvoyant None si ce n'est pas le cas
      et Some res sinon, où res est une liste de couples (atome, Booléen)
      suffisants pour que la formule soit vraie. *)
let rec dpll_ex_sat (fcc : forme_clausale) : (string * bool) list option =
  match FormeClausale.min_elt_opt fcc with
  | None -> Some []
  | Some c -> (
      match Clause.min_elt_opt c with
      | None -> None
      | Some lit -> (
          match dpll_ex_sat (simplif_fcc fcc lit) with
          | Some l -> Some (lit_to_couple lit :: l)
          | None -> (
              match dpll_ex_sat (simplif_fcc fcc (neg_lit lit)) with
              | None -> None
              | Some l -> Some (lit_to_couple (neg_lit lit) :: l))))


let clause_unitaire_fcc (_:forme_clausale)  = failwith "a faire"

(** Applique l'algorithme DPLL pour déterminer si une fcc est satisfaisable. 
    Utilise la propagation unitaire. *)

let dpll_sat_unit_prop (_: forme_clausale) : bool = failwith "a faire"




(** Simplifie une fcc en considérant que tous les littéraux de la liste lits sont vrais *)
let simplif_fcc_lits (_ : forme_clausale) (_ : litteral list) : forme_clausale =
  failwith "à faire"

(* Calcule les littéraux purs d'une fcc *)
(* let find_pure_lits (_ : forme_clausale) : litteral list = failwith "à faire" *)


(** Renvoie la liste des listes de couples (atome, Booléen) suffisants pour que la formule soit vraie,
    selon l'algorithme DPLL. *)

let rec dpll_all_sat (fcc : forme_clausale) : (string * bool) list list =
  match FormeClausale.min_elt_opt fcc with
  | None -> [[]] 
  | Some c ->
    match Clause.min_elt_opt c with
    | None -> [] 
    | Some lit ->
      let vrai = simplif_fcc fcc lit in
      let faux = simplif_fcc fcc (neg_lit lit) in
      List.concat [
        List.map (fun l -> (lit_to_couple lit) :: l) (dpll_all_sat vrai);
        List.map (fun l -> (lit_to_couple (neg_lit lit)) :: l) (dpll_all_sat faux)
      ]
