open Formule

(** random_form atoms k renvoie une formule pseudo-aléatoire
   avec k opérateurs et des atomes de la liste atoms, liste
   supposée non vide. Les opérateurs sont Top, Bot, Non, Et
   Ou et Imp. La formule générée ne doit pas contenir
   d'opérateurs d'équivalence. *)
   let random_element lst =
    let len = List.length lst in
    List.nth lst (Random.int len)
     
    let rec random_form (atoms : string list) (k : int) : formule =
    match k with
    |0 -> Atome (random_element atoms) 
    |1 -> let reponses = [Bot ; Top ; 
    Non (Atome (random_element atoms)) ; 
    Et(Atome (random_element atoms), Atome (random_element atoms));
    Ou(Atome (random_element atoms), Atome (random_element atoms));
    Imp(Atome (random_element atoms), Atome (random_element atoms))
    ] in
    random_element reponses
    |_ -> let rand_int = Random.int k in
    let reponses = [
    Non (random_form atoms (k-1));
    Et(random_form atoms rand_int, random_form atoms (k - rand_int - 1));
    Ou(random_form atoms rand_int, random_form atoms (k - rand_int - 1));
    Imp(random_form atoms rand_int, random_form atoms (k - rand_int - 1))
    ] in
    random_element reponses