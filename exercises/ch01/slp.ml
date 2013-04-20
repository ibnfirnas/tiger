open Printf


module type TABLE = sig
  type t

  val empty  : t
  val lookup : t    -> string       -> int option
  val update : t    -> string * int -> t
  val print  : t    -> unit
end

module Table : TABLE = struct
  type t = (string * int) list

  let empty = []

  let rec lookup t k =
    match t with
    | [] -> None
    | (k', v) :: t when k = k' -> Some v
    | _ :: t -> lookup t k

  let update t (k, v) =
    (k, v) :: t

  let print t =
    List.iter (fun (k, v) -> printf "K: %s, V: %d\n" k v) t
end


type id = string

type binop = Plus | Minus | Times | Div

type stm = CompoundStm of stm * stm
         | AssignStm of id * exp
         | PrintStm of exp list

 and exp = IdExp of id
         | NumExp of int
         | OpExp of exp * binop * exp
         | EseqExp of stm * exp


let bar = String.make 80 '-'


let fun_of_op = function
  | Plus  -> (+)
  | Minus -> (-)
  | Times -> ( * )
  | Div   -> (/)


let maxargs (stm : stm) : int =
  let rec eval_stm = function
    | CompoundStm (a, b) -> (eval_stm a) @ (eval_stm b)
    | AssignStm (id, exp) -> eval_exp exp
    | PrintStm exps ->
      let n = List.length exps in
      n :: (List.fold_left (fun acc e -> (eval_exp e) @ acc) [] exps)
  and eval_exp = function
    | IdExp _ -> []
    | NumExp _ -> []
    | EseqExp (stm, exp) -> (eval_stm stm) @ (eval_exp exp)
    | OpExp (a, _, b) -> (eval_exp a) @ (eval_exp b)
  in
  List.fold_left max 0 (eval_stm stm)


let interp (stm : stm) : unit =
  let rec interp_stm (s : stm) (t : Table.t) : Table.t =
    begin match s with
    | CompoundStm (a, b) -> interp_stm b (interp_stm a t)
    | AssignStm (id, e) ->
      let v, t = interp_exp e t in
      Table.update t (id, v)

    | PrintStm exps ->
      let t =
        List.fold_left
        ( fun t e ->
            let v, t = interp_exp e t in
            printf "%d " v;
            t
        )
        t
        exps
      in
      print_newline ();
      t
    end

  and interp_exp (e : exp) (t : Table.t) : (int * Table.t) =
    match e with
    | IdExp id ->
      begin match Table.lookup t id with
      | None -> failwith (sprintf "ERROR: No value assigned to \"%s\"" id)
      | Some v -> v, t
      end
    | NumExp v -> v, t
    | EseqExp (s, e) -> interp_exp e (interp_stm s t)
    | OpExp (a, op, b) ->
      let a, t = interp_exp a t in
      let b, t = interp_exp b t in
      let v = (fun_of_op op) a b in
      v, t
  in
  let t = interp_stm stm Table.empty in
  print_endline bar;
  Table.print t


let main () =
  let prog : stm =
    CompoundStm
    ( AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3))
    , CompoundStm
      ( AssignStm
        ( "b"
        , EseqExp
          ( PrintStm [IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1)]
          , OpExp (NumExp 10, Times, IdExp "a")
          )
        )
      , PrintStm [IdExp "b"]
      )
    )
  in

  interp prog;
  print_endline bar;
  printf "Max args to PrintStm: %d\n" (maxargs prog)


let () = main ()
