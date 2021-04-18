module type GENOME =
sig
  type individu
  type parametre
  val creer: parametre -> individu
  val muter: individu -> individu
  val croiser: individu -> individu -> individu
end
;;
module  MotGenetique =
struct
  type individu = string
  type parametre = int
  let rec creer : parametre -> individu =
    fun param ->
      if param > 0 then
        let i = (Random.int 26) + 97 in
        Char.escaped ( Char.chr i ) ^ creer (param-1)
      else
        ""
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
  let croiser : individu -> individu -> individu =
    fun mot1 mot2 ->
      if String.length mot1 = String.length mot2 then
        let i = Random.int ((String.length mot1)+1) in
        ( String.sub mot1 0 i ) ^ ( String.sub mot2 i ( (String.length mot1) - i))
      else
        ""
end
;;
type condition = 
    Score_parfait of int 
  |Max_iter of int 
;;
module Evolution(MotGenetique : GENOME)   =
struct
  type population = MotGenetique.individu list
  type evaluateur = MotGenetique.individu -> int 

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
  let mutation : population -> population =
    fun l ->
      List.map MotGenetique.muter l
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
  let generation : evaluateur->int->int->population-> population =
    fun eval n m l ->
      let solution = selection eval l m   in
      let repro = reproduction solution (n-m) in
      mutation repro
        
  let evolution : evaluateur -> MotGenetique.parametre -> int -> int ->condition-> population =
    fun eval parametre nb_individus nb_meilleurs condition  ->
      let rec initialisation = fun nb_individus parametre   ->
        if nb_individus = 0 then []
        else MotGenetique.creer parametre :: initialisation (nb_individus-1) parametre in
      let pop_initiale = initialisation nb_individus parametre in 
      match condition with 
      | Score_parfait  score ->
          let rec aux =  fun eval nb_individus nb_meilleurs condition liste -> 
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
;;
module Mystere = Evolution (MotGenetique) 
;;
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
let deviner_score_parfait : string -> string =
  fun mot_mystere -> 
    let evaluateur = comparer mot_mystere in
    let t = String.length mot_mystere in
    let score_parfait = Score_parfait t in
    let a = Mystere.evolution evaluateur t t (t/2) score_parfait  in
    (List.hd (Mystere.selection evaluateur a 1))
;;
let deviner_max_iteration : string -> string =
  fun mot_mystere ->
    let evaluateur = comparer mot_mystere in
    let t = String.length mot_mystere in
    let max_iteration = Max_iter 100 in
    let a = Mystere.evolution evaluateur t t (t/2) max_iteration in
    (List.hd (Mystere.selection evaluateur a 1))
;;
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
  let rec interaction () =
    let () = print_string "Veuillez entrer un mot : " in
    let mot_mystere =  read_line () in
      if (alpha_num mot_mystere) = true then 
 
        let () = print_string "A quel niveau voulez vous arrêter ? \n 1. Au Bout de Max iteration \n 2. Arrivé au score parfait \n" in
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
                 
              end
      else
        begin
          print_string "Veuillez ne pas saisir des caracteres autres que des lettres  \n ";
          interaction ();
        end
  ;; 

interaction ();;
