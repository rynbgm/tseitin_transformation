(** Module de Test :
    usage :
      - lancer dans le terminal la commande dune utop
      - ouvrir la bibliothèque : open Proposition ;;
      - ensuite, soit :
        - ouvrir le module : open Test;;
          les définitions deviennent accessibles : f;;
        - soit sélectionner du code et l'envoyer dans le terminal,
          sans oublier d'ajouter ";;"
    NB : pour envoyer du code dans un terminal ouvert, il faut ajouter
      le raccourci :
      - Ctrl+k Ctrl +s : ouverture des raccourcis clavier
      - chercher : terminal.runSelectedText
      - entrer un raccourci (par exemple Ctrl + F6)
    *)

open Formule
open FromString
open FCC

let f : formule option = form_from_string "x + y -> z"
let f' = Imp (Ou (Atome "x", Atome "y"), Atome "z")
let f2 : formule option = form_from_string "x -> y -> z"
let f3 : formule option = form_from_string "x * y * z"
let f3' = Et (Et (Atome "x", Atome "y"), Atome "z")

let f4 : formule option = form_from_string "x + y * z"
let f4' = Ou (Atome "x", Et (Atome "y", Atome "z"))
let f5 : formule = Option.get (form_from_string "a * b + c -> ~ d")
let f6 = Imp (Bot, Top)
let f7 = Et (f6, f5)

let f8 = Et(Non(Et(Atome "a",Atome "b")),Et(Atome "a",Atome "c"))
let f9 = Option.get(form_from_string "a -> b ")
let f10 = Option.get (form_from_string "b -> a ")
let f11 =  (Et(f9,f10))

(*des interprétation*)
(*let i s = s = "a" || s = "c"*)
(*let i2 s = List.mem s [ "a"; "c" ]*)

(* {a,¬b,¬c}*)
(* a tester sur le terminal apres dune build puis dune utop *)
(*Clause agi c'est comme le .h en c segmi quand on fait ca ya abstract *)
(* mais ya Clause.elements qui aide a afficher les elements*)

(*Clause.elements c1;;
  - : litteral list =
  [(Proposition.FCC.Plus, "a"); (Proposition.FCC.Moins, "b");
   (Proposition.FCC.Moins, "c")]
*)
let c1 = Clause.of_list [ (Plus, "a"); (Moins, "b"); (Moins, "c") ]
(* let c2 = Clause.map neg_lit c1 *)
(* Clause.elements c3;;
   - : litteral list =
   [(Proposition.FCC.Plus, "c"); (Proposition.FCC.Plus, "d");
    (Proposition.FCC.Moins, "a")]
*)
let c3 = Clause.add (Plus, "d") (Clause.remove (Plus, "b") c1)

(*List.map Clause.elements (FormeClausale.elements fcc1);;
  - : litteral list list =
  [[(Proposition.FCC.Plus, "a"); (Proposition.FCC.Moins, "b");
    (Proposition.FCC.Moins, "c")];
   [(Proposition.FCC.Plus, "c"); (Proposition.FCC.Plus, "d");
    (Proposition.FCC.Moins, "a")]]*)
let fcc1 = FormeClausale.of_list [ c1; c3 ]
let c4 = FormeClausale.of_list 
  [Clause.of_list [(Plus,"a");(Plus,"b")] ;Clause.of_list [(Plus,"c")]]

let a = Clause.of_list[(Plus,"a")]
let non_a = Clause.of_list[(Moins,"a")]

let b = Clause.of_list[(Plus,"b")]
let non_b = Clause.of_list[(Moins,"b")]

let c = Clause.of_list[(Plus,"c")]
let non_c = Clause.of_list[(Moins,"c")]
let d = Clause.of_list[(Plus,"d")]
let non_d = Clause.of_list[(Moins,"d")]
let e = Clause.of_list[(Plus,"e")]
let non_e = Clause.of_list[(Moins,"e")]


let fcc_a = FormeClausale.of_list [a] ;;
let fcc_b = FormeClausale.of_list [b] ;;
let fcc_c = FormeClausale.of_list [c] ;;

let fcc_non_a = FormeClausale.of_list [non_a] ;;
let fcc_non_b = FormeClausale.of_list [non_b] ;;
let fcc_non_c = FormeClausale.of_list [non_c] ;;

let fcc_cours = FormeClausale.of_list 
  [Clause.of_list [(Plus,"a");(Plus,"b");(Plus,"c")] 
  ;Clause.of_list [(Plus,"a");(Moins,"c")]
  ;Clause.of_list [(Moins,"a");(Moins,"c")]
  ;Clause.of_list [(Plus,"b")]
  ;Clause.of_list [(Moins,"b");(Plus,"c")]
  ;Clause.of_list [(Plus,"d");(Plus,"e")]]


let fcc_cours2 = FormeClausale.of_list
  [
    Clause.of_list [(Plus,"a");(Plus,"b");(Plus,"c")] ;
  Clause.of_list [(Plus,"a");(Moins,"b")];
  Clause.of_list [(Plus,"b");(Moins,"c");(Moins,"d")];
  Clause.of_list [(Plus,"b");(Plus,"c");(Plus,"d")];
  Clause.of_list [(Moins,"b");(Plus,"c");(Moins,"d")];
  Clause.of_list [(Moins,"b");(Plus,"c");(Plus,"d")];
  Clause.of_list [(Moins,"c")];
  ]


let formule_cours  =Option.get (form_from_string "( a + b ) * c * ( ~ c + ~ a * ~ b)")

let tsietin2 = Option.get(form_from_string " a * b * c + ( a -> b -> c)");;