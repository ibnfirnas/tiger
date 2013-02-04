open Printf


module type TREE = sig
  type key = string
  type 'a t

  val empty  : 'a t
  val insert : 'a t -> key * 'a -> 'a t
  val member : 'a t -> key -> bool
  val lookup : 'a t -> key -> 'a option
  val print  : 'a t -> string -> unit
end


module Tree : TREE = struct
  type key = string

  type 'a t = Tree of 'a t * key * 'a * 'a t
            | Leaf


  let empty = Leaf

  let rec insert t (key, value) =
    match t with
    | Leaf                           -> Tree (Leaf, key, value, Leaf)
    | Tree (l, k, v, r) when key < k -> Tree (insert l (key, v), k, v, r)
    | Tree (l, k, v, r) when key > k -> Tree (l, k, v, insert r (key, value))
    | Tree (l, _, _, r)              -> Tree (l, key, value, r)

  let rec member t key =
    match t with
    | Leaf                           -> false
    | Tree (_, k, _, _) when key = k -> true
    | Tree (l, k, _, _) when key < k -> member l key
    | Tree (_, _, _, r)              -> member r key

  let rec lookup t key =
    match t with
    | Leaf                           -> None
    | Tree (_, k, v, _) when key = k -> Some v
    | Tree (l, k, _, _) when key < k -> lookup l key
    | Tree (_, _, _, r)              -> lookup r key

  let edges t =
    let rec edges x = function
      | Leaf              -> []
      | Tree (l, y, _, r) ->
        (x, `Node y) :: ((edges (`Node y) l) @ (edges (`Node y) r))
    in
    edges `Root t

  let to_dot t name =
    let str_of_node = function
      | `Root -> "ROOT"
      | `Node x -> x
    in
    let rec str_of_edges = function
      | [] -> ""
      | (a, b) :: es ->
        sprintf
        "  \"%s\" -> \"%s\";\n%s"
        (str_of_node   a)
        (str_of_node   b)
        (str_of_edges es)
    in
    sprintf "digraph %s {\n%s};\n" name (str_of_edges (edges t))

  let print t name =
    print_endline (to_dot t name)
end


let main () =
  let xs = ["t"; "s"; "p"; "i"; "p"; "f"; "b"; "s"; "t"] in
  let ys = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"] in

  let xt = List.fold_left (fun t x -> Tree.insert t (x, x)) Tree.empty xs in
  let yt = List.fold_left (fun t y -> Tree.insert t (y, y)) Tree.empty ys in

  Tree.print xt "X";
  Tree.print yt "Y"


let () = main ()
