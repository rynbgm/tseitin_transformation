open Formule

(** Mesure du temps d'exécution d'une formule fonc sur un paramètre arg
    renvoyant le couple (résultat, temps de calcul). *)
let mesure fonc arg =
  let debut = Sys.time () in
  let res = fonc arg in
  let fin = Sys.time () in
  (res, fin -. debut)

let bench_dpll (f : formule) : unit = 
  print_string("F = " ^ string_of_formule f ^ "\n");
  let (f',time) = mesure Tseitin.tseitin f in 
  print_string("T(F)'= " ^ string_of_formule f' ^ "\n");
  Printf.printf "Transformation : %.6fs\n" time ;
  let (fcc_f,time) = mesure FCC.formule_to_fcc f in
  let nb_clauses = List.length(FCC.fcc_to_list fcc_f) in
  let nb_litteaux = 
    List.fold_left (fun acc c -> acc + (FCC.Clause.cardinal c)) 0 ((FCC.FormeClausale.elements fcc_f))in
  Printf.printf "fcc(F) : %.6fs (%d clauses, %d litteraux)\n" time  nb_clauses nb_litteaux;
  let (fcc_f',time) = mesure FCC.formule_to_fcc f' in
  let nb_clauses = List.length(FCC.fcc_to_list fcc_f') in
  let nb_litteaux = 
    List.fold_left (fun acc c -> acc + (FCC.Clause.cardinal c)) 0 ((FCC.FormeClausale.elements fcc_f'))in
  Printf.printf "fcc(T(F)) : %.6fs (%d clauses, %d litteraux)\n" time  nb_clauses nb_litteaux


let bench_dpll' (sl : string list) (k : int) : unit =
  let f = RandomFormule.random_form sl k in
  bench_dpll f

