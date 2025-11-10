  $ cat << EOF | ./run.exe
  > type 'a list =
  >   | Cons of 'a * 'a list
  >   | Nil
  > 
  > let rec map f ls =
  >   match ls with
  >   | Nil -> Nil
  >   | Cons (hd, tl) -> Cons (f hd, map f tl)
  > 
  result:
  type ([ 0; ]) list =
  | Cons of '_0 * ('_0) list
  | Nil
  
  let rec map: ('_3 -> '_4) -> ('_3) list -> ('_4) list =
    fun f ls -> match ls with
                  | Nil -> Nil
                  
                  | Cons (hd, tl) -> Cons ((f hd), ((map f) tl))
                  
  
  

  $ cat << EOF | ./run.exe
  > type 'a list =
  >   | Cons of 'a * 'a list
  >   | Nil
  > 
  > let rec helper ls acc =
  >   match ls with
  >   | Nil -> acc
  >   | Cons (hd, tl) -> helper tl (Cons (hd, acc))
  > 
  > let rev ls = helper ls Nil
  result:
  type ([ 0; ]) list =
  | Cons of '_0 * ('_0) list
  | Nil
  
  let rec helper: ('_3) list -> ('_3) list -> ('_3) list =
    fun ls acc -> match ls with
                    | Nil -> acc
                    
                    | Cons (hd, tl) -> (helper tl) Cons (hd, acc)
                    
  
  let rev: ('_3) list -> ('_3) list =
    fun ls -> (helper ls) Nil
  
  
  $ cat << EOF | ./run.exe
  > type 'a list =
  >   | Cons of 'a * 'a list
  >   | Nil
  > 
  > let is_empty ls =
  >   match ls with
  >   | Cons (_, _) -> false
  >   | Nil -> true
  > 
  > let rec inc_list ls =
  >   match ls with
  >   | Cons (hd, tl) -> Cons (hd + 1, inc_list tl)
  >   | Nil -> Nil
  > 
  > let rec is_false_list ls =
  >   match ls with
  >   | Nil -> true
  >   | Cons (hd, tl) -> if hd then false else is_false_list tl
  result:
  type ([ 0; ]) list =
  | Cons of '_0 * ('_0) list
  | Nil
  
  let is_empty: ('_3) list -> bool =
    fun ls -> match ls with
                | Cons (_, _) -> false
                
                | Nil -> true
                
  
  let rec inc_list: (int) list -> (int) list =
    fun ls -> match ls with
                | Cons (hd, tl) -> Cons ((hd + 1), (inc_list tl))
                
                | Nil -> Nil
                
  
  let rec is_false_list: (bool) list -> bool =
    fun ls -> match ls with
                | Nil -> true
                
                | Cons (hd, tl) -> (if hd then false else is_false_list tl)
                
  
  
