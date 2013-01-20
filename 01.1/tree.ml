open Printf


module type TREE = sig
  type key = string
  type 'a t

  val empty  : 'a t
  val insert : 'a t -> key * 'a -> 'a t
  val member : 'a t -> key -> bool
  val lookup : 'a t -> key -> 'a option
  val print  : 'a t -> unit
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

  let to_string t =
    let margin = "   " in
    let rec to_string indent = function
      | Leaf              -> ""
      | Tree (l, k, _, r) ->
        let left = to_string (margin ^ indent)  l in
        let right =  to_string (margin ^ indent) r in
        sprintf "%s.%s%s\n%s" right indent k left
    in
    to_string margin t

  let print t =
    print_endline (to_string t)
end


let main () =
  let bar = String.make 80 '-' in

  let xs = ["t"; "s"; "p"; "i"; "p"; "f"; "b"; "s"; "t"] in
  let ys = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"] in

  let xt = List.fold_left (fun t x -> Tree.insert t (x, x)) Tree.empty xs in
  let yt = List.fold_left (fun t y -> Tree.insert t (y, y)) Tree.empty ys in

  List.iter print_string xs;
  print_newline ();
  print_endline bar;
  Tree.print xt;

  print_newline ();

  List.iter print_string ys;
  print_newline ();
  print_endline bar;
  Tree.print yt


let () = main ()
