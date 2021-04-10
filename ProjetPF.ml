module type GENOME = 
sig
  type individu
  type parametre 
  val creer: parametre -> individu
  val muter: individu -> individu
end;;

module MotGenetique  = 
  struct
    type parametre = int 
    type individu = string
    let rec creer : parametre -> individu = 
      fun parametre ->  
      if parametre > 0 then   
      let i = (Random.int 26) + 97 in
      Char.escaped (Char.chr i) ^ creer (parametre-1)
      else ""
      let rec muter : individu -> individu =
        fun mot ->
          if String.length mot > 0 then
            (let fst_carac = String.sub mot 0 1 in
             let i = Random.int 101 in
             if i < 5 then
               Char.escaped (Char.chr ((Random.int 26)+97) )
             else
               fst_carac )
               ^ muter (String.sub mot 1 ((String.length mot)-1) )
          else
            ""
       
      
end ;;
      
