open Formule

(** Calcule la transformée de Tseitin d'une formule donnée,
    en supposant que la formule ne contienne pas d'opérateur d'équivalence. *)

let name_atome (f:formule) (i:int)  = match f with
  | (Bot | Top) as f -> f
  | Atome s -> Atome ("old_" ^ s)
  | _ -> Atome ("new_" ^ (string_of_int i))


      (** Calcule la transformée de Tseitin d'une formule donnée,
      en supposant que la formule ne contienne pas d'opérateur d'équivalence. *)
let tseitin (form : formule) : formule =
  let rec aux (f, acc, i) =
    match f with
    | Bot | Top | Atome _ -> f,acc,i
    | Non f' -> let nom_general = name_atome (Non f') i  in (*nom general*)
                let _,f1,i' = aux (f',acc,(i+1)) in (*app rec sur la sous f*)
                let nom_sous_forme = name_atome f' i'  in (*nom general sous f*)
                let eq = Equiv(nom_general,Non nom_sous_forme) in (*equiv entre genrela et sous f*)
                f,Et(f1,eq),i' (*f general et son equiv *)
    |Ou(f',g') -> let nom_general = name_atome (Ou(f',g')) i in  (*nom f general*)
                  let nom_sous_forme_gauche = name_atome f' (i+1) in (*nom sous f gauche*)
                  let f,f1,i1' = aux (f',acc,(i+1)) in  (*app rec sur sous form gauche*)
                  let nom_sous_forme_droite = name_atome g' i1' in (*nom sous f droite*)
                  let eq = Equiv(nom_general,Ou(nom_sous_forme_droite,nom_sous_forme_gauche)) in (*equiv f general et *)
                  let _,f2,_ = aux (g',eq,i1') in (*app rec sur sous f droite*)
                  f,(Et(f1,f2)),i1'
    |Et(f',g') -> let nom_general = name_atome (Et(f',g')) i in
                  let nom_sous_forme_gauche = name_atome f' (i+1) in
                  let f,f1,i1' = aux (f',acc,(i+1)) in
                  let nom_sous_forme_droite = name_atome g' i1' in
                  let eq = Equiv(nom_general,(Et(nom_sous_forme_gauche,nom_sous_forme_droite))) in
                  let _,f2,_ = aux (g',eq,i1') in
                  f,(Et(f1,f2)),i1'
    |Imp(f',g') -> let nom_general = name_atome (Imp(f',g')) i in
                    let nom_sous_forme_gauche = name_atome  f' (i+1) in
                    let f,f1,i1' = aux (f',acc,(i+1)) in
                    let nom_sous_forme_droite = name_atome g' i1' in
                    let eq = Equiv(nom_general,Imp(nom_sous_forme_gauche,nom_sous_forme_droite)) in
                    let _,f2,_ = aux (g',eq,i1') in
                    f,(Et(f1,f2)),i1'
    | _ -> failwith "cas innatendu tseition.ml/tseintin..."
  in
  let result = aux (form, (name_atome form 0), 0) in
  let (_, r, _) = result in
  r

(** Transforme une interprétation évaluant une transformée de Tseitin T(F) comme vraie
    en une interprétation évaluant la formule de départ F comme vraie. *)
(* let restrict_inter (i' : interpretation) : interpretation = match i' with *)

let restrict_inter (_ : interpretation) (_: formule) : interpretation =
  failwith"jsp"

(** Transforme une interprétation évaluant une formule F comme vraie
    en une interprétation évaluant sa transformée de Tseitin T(F) comme vraie. *)
    let extension_inter (_: interpretation) (_ : formule) : interpretation =
   failwith "extension_inter : à faire"



