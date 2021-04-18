module type GENOME = 
sig
  type individu
  type parametre
  val creer: parametre -> individu
  val muter: individu -> individu
  val croiser: individu -> individu -> individu
end
;;(** Un module qui implante le type module GENOME*)
module  MotGenetique = 
struct
  type individu = string
  type parametre = int
  (** Cette fonction permet de créer un individu en fonction d'un parametre *)
  let rec creer : parametre -> individu = 
    fun param ->
      if param > 0 then
        let i = (Random.int 26) + 97 in
        Char.escaped ( Char.chr i ) ^ creer (param-1)
      else
        ""
  (** Cette fonction prend un individu et tente de faire une mutation *)
  let rec muter : individu -> individu = 
    fun mot ->
      if String.length mot > 0 then
        (let fst_carac = String.sub mot 0 1 in
         let i = Random.int 101 in
         if i < 5 then
           Char.escaped (Char.chr ((Random.int 26)+97) )
         else
           fst_carac
        )
        ^ muter (String.sub mot 1 ((String.length mot)-1) )
      else
        ""
  (** Cette fonction prend deux individus et nous fait un croisement entre eux pour en déduire un autre *)      
  let croiser : individu -> individu -> individu = 
    fun mot1 mot2 ->
      if String.length mot1 = String.length mot2 then
        let i = Random.int ((String.length mot1)+1) in
        ( String.sub mot1 0 i ) ^ ( String.sub mot2 i ( (String.length mot1) - i))
      else
        ""
end
;;
(** type condition prend deux constructeurs*)
type condition = 
    Score_parfait of int 
  |Max_iter of int 
;;
(** module Evolution est un foncteur qui prend en parametre le module MotGenetique *) 
module Evolution(MotGenetique : GENOME)   =
struct
  (** le type population est sous forme d'une liste d'individu *)
  type population = MotGenetique.individu list
  (** Le type evaluateur prend un individu et nous rend un entier *) 
  type evaluateur = MotGenetique.individu -> int 
(** Cette fonction prend en parametre une population et un entier n  pour ensuite faire des croisement entre les population n fois et ensuite les rajouter à la population  *)
  let rec reproduction : population -> int -> population =
    fun l n ->
      if n = 0 then
        l
      else
        let taille = List.length l in
        let i = Random.int taille in
        let j = Random.int taille in
        let mot_croise = MotGenetique.croiser ( List.nth l i ) ( List.nth l j ) in
        mot_croise :: reproduction l (n-1)
        (** Cette fonction applique la mutation sur une population*)
  let mutation : population -> population = 
    fun l ->
      List.map MotGenetique.muter l
      (** Cette fonction selection renvoie une population qui découle la selectionne  des meilleurs individus d'une population initial *)
  let selection : evaluateur -> population -> int -> population = 
    fun eval l n ->
      let list_eval = List.map eval l in
      let liste_combine = List.combine list_eval l in
      let liste_trie = List.rev ( List.sort compare liste_combine ) in
      let rec aux =
        fun eval liste_trie n ->
          match liste_trie with
          | [] -> []
          | h :: t -> if n > 0 then
                snd h :: aux eval t (n-1)
              else
                []
      in
      aux eval liste_trie n
      (** Cette fonction fait la selection sur une population ensuite croise les meilleures individus pour donner une population qu'il va muter *)
  let generation : evaluateur->int->int->population-> population = 
    fun eval n m l ->
      let solution = selection eval l m   in
      let repro = reproduction solution (n-m) in
      mutation repro
     (** Cette fonction crée une population initial et là faire évoluer jusqu'a satisfaire la condition de départ*)   
  let evolution : evaluateur -> MotGenetique.parametre -> int -> int ->condition-> population = 
    fun eval parametre nb_individus nb_meilleurs condition  ->
      let rec initialisation = fun nb_individus parametre   ->
        if nb_individus = 0 then []
        else MotGenetique.creer parametre :: initialisation (nb_individus-1) parametre in
      let pop_initiale = initialisation nb_individus parametre in 
      match condition with 
      | Score_parfait  score ->
          let rec aux =  fun eval nb_individus nb_mseilleurs condition liste -> 
            let nouvelle_pop = generation eval nb_individus nb_meilleurs liste in
            if eval( List.hd ( selection eval nouvelle_pop 1 )) = score  then nouvelle_pop
            else aux eval nb_individus nb_meilleurs score nouvelle_pop
          in aux eval nb_individus nb_meilleurs score pop_initiale 
      | Max_iter max ->
          let rec aux =  fun eval nb_individus nb_meilleurs  max  liste ->
            let nouvelle_pop = generation eval nb_individus nb_meilleurs liste in
            if max = 0 then nouvelle_pop
            else aux eval nb_individus nb_meilleurs (max-1) nouvelle_pop
          in aux eval nb_individus nb_meilleurs max pop_initiale
            
end
;;(** Ce module est l'application du foncteur Evolution *)
module Mystere = Evolution (MotGenetique);; (** Cette fonction compare deux chaine de caractére et ensuite renvoie le nombre de caractére identique à la même position *)
let comparer : string -> string -> int =
  fun mot1 mot2 ->
    let rec aux  = fun mot1 mot2 nb_carac_identique ->
      let taille = String.length mot1 in
      if taille = 0 then nb_carac_identique
      else
      if (String.sub mot1 0 1  ) = (String.sub mot2 0 1  ) then
        aux (String.sub mot1 1 (taille-1)) (String.sub mot2 1 (taille-1)) nb_carac_identique+1
      else
        aux (String.sub mot1 1 (taille-1)) (String.sub mot2 1 (taille-1)) nb_carac_identique
    in aux mot1 mot2 0
;;
(** Cette fonction devine l'individu via le score donner par l'utilisateur *)
let rec deviner_score_parfait : string -> string = 
  fun mot_mystere -> 
    let evaluateur = comparer mot_mystere in
    let t = String.length mot_mystere in
    let () = print_string "Veuillez entrer un score entre 1 et"in
    Printf.printf " %d(le score parfait) : " t;
    let score =  read_int () in
    if score >= 1 && score <= t then 
      let score_parfait = Score_parfait score in
      let a = Mystere.evolution evaluateur t t (t/2) score_parfait  in
      (List.hd (Mystere.selection evaluateur a 1))
    else deviner_score_parfait mot_mystere
;;
(** Cette fonction devine le meilleur individu au bout de Max-itération *)
let deviner_max_iteration : string -> string = 
  fun mot_mystere ->
    let evaluateur = comparer mot_mystere in
    let t = String.length mot_mystere in
    let max_iteration = Max_iter 100 in
    let a = Mystere.evolution evaluateur t t (t/2) max_iteration in
    (List.hd (Mystere.selection evaluateur a 1))
;;
(** Cette fonction détecte s'il y a des caractéres autre que des lettres miniscules *)
let rec alpha_num = 
  fun mot ->
    let taille = String.length mot in
    if taille = 0 then true
    else
      let fst_carac =  (String.get mot 0 ) in
      let equi = Char.code(fst_carac) in
      if (equi < 123) && (equi > 96) then 
        alpha_num (String.sub mot 1 (taille-1))
      else
        false
;;
(** Cette fonction intéragit avec l'utilateur *)
let rec interaction () =
  let () = print_string "Veuillez entrer un mot : " in
  let mot_mystere =  read_line () in
  if (alpha_num mot_mystere) = true then 
    let () = print_string "************Mode de transformation************ \n A quel niveau voulez-vous vous arrêter ? \n 1. Au Bout de max_iter interation  \n 2. Arrivé à un score n ( n compris entre 1 et le score parfait) \n" in
    let reponse =  read_int () in
    if reponse = 1 then 
      begin
        print_string (deviner_max_iteration mot_mystere);
        print_string ("\n");
      end
    else
    if reponse = 2 then 
      begin
        print_string (deviner_score_parfait mot_mystere);
        print_string ("\n");
      end
    else
      begin
        print_string "Vous ne pouvez choisir qu'entre 1 et 2  \n ";
        interaction ();
      end
  else
    begin
      print_string "Veuillez ne pas saisir des caracteres autres que des lettres  \n ";
      interaction ();
    end
;; 

interaction ();;
