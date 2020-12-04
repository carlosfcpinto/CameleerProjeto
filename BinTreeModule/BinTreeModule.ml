module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module type S =
  sig
    type elt
    type t
    val find: t -> elt -> bool
    val insert: t -> elt -> t
    val delete: t -> elt -> t
  end

module BinTree(TreeNode : Comparable) = 
  struct
    type elt = TreeNode.t
    type t =  |Node of t * elt * t 
              |Empty

    let rec find tree b = 
    match tree with
      |Node (l,a,r)-> if (a=b) then true else
                      if (a < b) then find r b else find r b
      |Empty -> false
    
    let rec insert tree x =
      match tree with
        |Empty -> Node (Empty,x,Empty)
        |Node (l,y,r) -> if y > x then Node ((insert l x),y,r) else
                      if y < x then Node (l, y, (insert r x))
                      else Node (l,y,r)

    let rec find_min tree =
      match tree with
        | Empty -> assert false
        | Node (Empty, x, r) -> (x, r)
        | Node (l,a,b) -> let x,y = find_min l in (x, (Node (y,a, b)))
    
    let rec delete tree x = 
      match tree with
        |Empty -> Empty
        |Node (l,y,r) ->  if x < y then Node ((delete l x),y,r) else
                if x > y then Node (l ,y ,(delete r x)) else 
                  match l, r with
                  |Empty, _ -> r
                  |_, Empty -> l
                  |l', r' -> let (min, x') = find_min r' in Node (l',min,(x')) 
  
end
