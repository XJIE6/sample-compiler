module Expr =
  struct

    open Language.Expr

    let eval' left right op =
      let int   f = fun x y -> if f x y then 1 else 0 in
      let bool  f =
        let bool' x = if x == 0 then true else false in
        fun x y -> f (bool' x) (bool' y)
      in
       (match op with
       |"+" -> (+)
       |"-" -> (-)
       |"*" -> ( * )
       |"/" -> (/)
       |"%" -> (mod)
       |">" -> int (>)
       |"<" -> int (<)
       |">=" -> int (>=)
       |"<=" -> int (<=)
       |"==" -> int (==)
       |"!=" -> int (!=)
       |"&&" -> int @@ bool (&&)
       |"!!" -> int @@ bool (||)
       ) left right
                                                         
    let rec eval ((state, def, stmt_eval) as c) = function
      | Const n -> n
      | Var   x -> List.assoc x state
      | Binop (op, l, r) -> eval' (eval c l)  (eval c r) op
      | Call (n, p) -> let (names, body) = List.assoc n def in
                       let args = List.map (fun x -> eval c x) p in
                       let fun_state  = List.map2 (fun x y -> (x, y)) names args in
                       let (fun_state', ret) = stmt_eval fun_state body in
                       match ret with
                       | `Return e -> eval (fun_state, def, stmt_eval) e
                       | _ -> failwith "no return" 
  end
  
module Stmt =
  struct

    open Language.Stmt

    let eval read write def stmt =
      let rec eval' state stmt =
        let expr_eval = Expr.eval (state, def, eval') in
	match stmt with
	| Skip          -> (state, `Continue)
	| Seq    (l, r) -> let (state', ret) as c = eval' state l in
                           (match ret with
                           | `Continue -> eval' state' r
                           | _ -> c)
	| Assign (x, e) -> ((x, expr_eval e) :: state, `Contunue)
	| Write   e     -> write @@ expr_eval e;
                           (state, `Continue)
	| Read    x     -> let y = read() in
                           ((x, y) :: state, `Continue)
        | If(e, s1, s2) -> if (expr_eval e) != 0 then eval' state s1 else eval' state s2
        | While(e, s1)  -> if (expr_eval e) != 0 then eval' state @@ Seq (s1, While (e, s1)) else (state, `Continue)
        | Repeat(s, e)  -> eval' state @@ Seq (s, (While (Binop("==", e, Const 0), s)))
        | Run(n, p)     -> expr_eval @@ Call (n, p);
                           (state, `Continue)
        | Return v      -> (state, `Return v)
      in
      let (_, ret) = eval' ([]) stmt in ()
      (*match ret with
      | `Continue -> ()
      | _ -> failwith "nonzero return code"*)

  end
