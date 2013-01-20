module type TREE = sig
  type key = string
  type t

  val empty : t
  val insert : key -> t -> t
  val member : key -> t -> bool
end


module Tree : TREE = struct
  type key = string

  type t = Leaf
         | Tree of t * key * t


  let empty = Leaf

  let rec insert key = function
    | Leaf -> Tree (Leaf, key, Leaf)
    | Tree (l, k, r) ->
      if key < k then
        Tree (insert key l, k, r)
      else if key > k then
        Tree (l, k, insert key r)
      else
        Tree (l, key, r)

  let rec member key = function
    | Leaf -> false
    | Tree (l, k, r) when key = k -> true
    | Tree (l, k, r) -> (member key l) || (member key r)
end
