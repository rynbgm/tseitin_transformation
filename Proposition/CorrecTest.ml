open Formule

let test_equisat_quine ( f : formule) : unit =
  print_string("F = " ^ string_of_formule f ^ "\n");
  let f' = Tseitin.tseitin f in 
  print_string("T(F)'= " ^ string_of_formule f' ^ "\n");
  let b = Quine.quine_sat f in 
  print_string("quine_sat (F) : " ^ string_of_bool b ^ "\n");
  let b2 = Quine.quine_sat f' in 
  print_string("quine_sat (T(F)) : " ^ string_of_bool b2 ^ "\n");
  print_string("Equisat ? " ^ (string_of_bool (b2==b)) ^ "\n")
   
  let test_equisat_quine' (l : string list) ( k: int) : unit =
  let f = RandomFormule.random_form l k in
  test_equisat_quine f
   
  let test_equisat_dpll (f : formule) : unit =
  print_string("F = " ^ string_of_formule f ^ "\n");
  let f' = Tseitin.tseitin f in 
  print_string("T(F)'= " ^ string_of_formule f' ^ "\n")
   
  let test_equisat_dpll' (l : string list) (k : int) : unit =
  let f = RandomFormule.random_form l k in
  test_equisat_dpll f
