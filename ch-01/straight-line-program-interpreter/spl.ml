open Printf


type id = string

type binop = Plus | Minus | Times | Div

type stm = CompoundStm of stm * stm
         | AssignStm of id * exp
         | PrintStm of exp list

 and exp = IdExp of id
         | NumExp of int
         | OpExp of exp * binop * exp
         | EseqExp of stm * exp


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


let maxargs (stm : stm) : int =
  let rec eval_stm = function
    | CompoundStm (a, b) -> (eval_stm a) @ (eval_stm b)
    | AssignStm (id, exp) -> eval_exp exp
    | PrintStm exps ->
      let args = List.length exps in
      args :: (List.fold_left (fun acc e -> (eval_exp e) @ acc) [] exps)
  and eval_exp = function
    | IdExp _ -> []
    | NumExp _ -> []
    | EseqExp (stm, exp) -> (eval_stm stm) @ (eval_exp exp)
    | OpExp (a, _, b) -> (eval_exp a) @ (eval_exp b)
  in
  List.fold_left max 0 (eval_stm stm)


let main () =
  printf "%d\n" (maxargs prog)


let () = main ()
