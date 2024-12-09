open Formule

(* Compléter ce fichier avec les éléments du fichier Quine.ml du TP 2 nécessaires pour le projet,
   en plus des définitions ci-dessous. *)


(** subst g s f : substitue une formule g à un atome s dans une formule f. *)
(* remplace avec g les s dans f c'est facile comme ca *)
let rec subst : formule -> string -> formule -> formule =
  fun g s f ->
   match f with
   | Atome a when a = s -> g
   | Bot | Top -> f
   | Atome a -> Atome a
   | Non f -> Non (subst g s f)
   | Et (f1, f2) -> Et (subst g s f1, subst g s f2)
   | Ou (f1, f2) -> Ou (subst g s f1, subst g s f2)
   | Imp (f1, f2) -> Imp (subst g s f1, subst g s f2)
   | Equiv (f1, f2) -> Equiv ( subst g s f1, subst g s f2)
 
 (** Choisit un atome d'une formule, renvoyant None si aucun n'est présent.*)
 let rec choix_atome : formule -> string option =
  fun f ->
   match f with
   | Bot | Top -> None
   | Atome a -> Some a
   | Non f1 -> choix_atome f1
   | Et (f1, f2) | Ou (f1, f2) | Imp (f1, f2) | Equiv(f1,f2)-> (
       match choix_atome f1 with
          None -> choix_atome f2 
          | Some x -> Some x)
 
 (** Simplifie une formule d'une manière paresseuse. *)
 let rec simplif_quine : formule -> formule =  function
   | (Bot | Top | Atome _) as f ->  f
   | Ou (f, g) -> (
       match simplif_quine f with
       | Bot -> simplif_quine g
       | Top -> Top
       | f' -> (
                 match simplif_quine g with 
                 Bot -> f' 
                 | Top -> Top 
                 | g' -> Ou (f', g')
              )
       )
     | Et (f, g) -> (
         match simplif_quine f with
         | Bot -> Bot
         | Top -> simplif_quine g
         | f' -> (
             match simplif_quine g with Bot -> Bot | Top -> f' | g' -> Et (f', g'))
         )
   | Imp (f, g) -> (
       match simplif_quine f with
       | Bot -> Top
       | Top -> simplif_quine g
       | f' -> (
           match simplif_quine g with
           | Bot -> Non f
           | Top -> Top
           | g' -> Imp (f', g')))
   | Equiv (f, g) -> (
       match simplif_quine f , simplif_quine g  with
       |(Bot,Bot) | (Top,Top) -> Top
       | (Bot,Top) | (Top,Bot) -> Bot
       | (f' , g') -> if f' = g' then Top else Equiv (f', g')
       )
   | Non f -> ( match f with Bot -> Top | Top -> Bot | f' -> Non f')
 

(** Teste si une formule est satisfaisable, selon l'algorithme de Quine. *)
let rec quine_sat : formule -> bool = fun f ->
  match simplif_quine f with
  | Top ->  true
  | Bot ->false
  | Atome _ -> true  
  | f' -> (match choix_atome f' with 
            | None -> false
            | Some choix ->
                quine_sat (subst Top choix f') || quine_sat (subst Bot choix f')
          )


(** Teste si une formule est satisfaisable, renvoyant None si ce n'est pas le cas
      et Some res sinon, où res est une liste de couples (atome, Booléen)
      suffisants pour que la formule soit vraie. *)
let  rec quine_ex_sat : formule -> (string * bool) list option = fun f -> 
match simplif_quine f with
  |Bot -> None
  |Top ->Some[] 
  |Atome a -> Some[a,true]
  | f' -> (match choix_atome f' with
          |None -> None 
          |Some choix ->let res_true = quine_ex_sat(subst Top choix f') in  
                        let res_false = quine_ex_sat(subst Bot choix f') in
                        (match res_true,res_false with  
                          Some res_true,_ -> Some((choix,true)::res_true)
                          |_,Some res_false -> Some((choix,false)::res_false)
                          |None,None -> None ))   
      
