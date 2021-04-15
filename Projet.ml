module type GENOME =
sig
  type individu
  type parametre
  val creer: parametre -> individu
  val muter: individu -> individu
  val croiser: individu -> individu -> individu
end
;;
module  MotGenetique : GENOME =
 
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
module Evolution  (MotGenetique:GENOME)   =
struct 
  open MotGenetique 
  type population = individu list
  type evaluateur = individu -> int 
 
  let rec reproduction : population -> int -> population =
    fun l n ->
      if n = 0 then
        l
      else
        let taille = List.length l in
        let i = Random.int taille in
        let j = Random.int taille in 
        
        let mot_croise = croiser ( List.nth l i ) ( List.nth l j ) in 
        mot_croise :: reproduction l (n-1)
          
 
  let mutation : population -> population =
    fun l ->
      List.map muter l
        
  
 
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
        
        
  let evolution : evaluateur -> parametre -> int -> int -> int -> population =
    fun eval parametre nb_individus nb_meilleurs max_iter -> 
      let rec initialisation = fun nb_individus parametre   ->
        if nb_individus = 0 then []
        else creer parametre :: initialisation (nb_individus-1) parametre in
      let pop_initiale = initialisation nb_individus parametre in
      let rec aux =  fun eval nb_individus nb_meilleurs  max_iter  liste ->
        let nouvelle_pop = generation eval nb_individus nb_meilleurs liste in
        if max_iter = 0 then nouvelle_pop 
        else aux eval nb_individus nb_meilleurs (max_iter-1) nouvelle_pop
      in aux eval nb_individus nb_meilleurs max_iter pop_initiale
        
end 
;;

                    