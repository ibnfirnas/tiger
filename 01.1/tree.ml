module type TREE = sig
  type key = string
  type 'a t

  val empty : 'a t
  val insert : 'a t -> key * 'a -> 'a t
  val member : 'a t -> key -> bool
  val lookup : 'a t -> key -> 'a option
end


module Tree : TREE = struct
  type key = string

  type 'a t = Tree of 'a t * key * 'a * 'a t
            | Leaf


  let empty = Leaf

  let rec insert t (key, value) =
    match t with
    | Leaf -> Tree (Leaf, key, value, Leaf)
    | Tree (l, k, v, r) ->
      if key < k then
        Tree (insert l (key, v), k, v, r)
      else if key > k then
        Tree (l, k, v, insert r (key, value))
      else
        Tree (l, key, value, r)

  let rec member t key =
    match t with
    | Leaf -> false
    | Tree (l, k, _, r) when key = k -> true
    | Tree (l, k, _, r) ->
      if key < k then
        member l key
      else
        member r key

  let rec lookup t key =
    match t with
    | Leaf -> None
    | Tree (l, k, v, r) when key = k -> Some v
    | Tree (l, k, _, r) ->
      if key < k then
        lookup l key
      else
        lookup r key
end
