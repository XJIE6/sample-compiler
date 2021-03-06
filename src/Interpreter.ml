open Language.Expr
open Language

type ret = 
| Continue 
| Return of Language.Expr.t

module Expr =
  struct

    let eval' (Int left) (Int right) op =
      let int   f = fun x y -> if f x y then 1 else 0 in
      let bool  f =
        let bool' x = if x == 0 then false else true in
        fun x y -> f (bool' x) (bool' y)
      in
       Int (match op with
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
                                                         
    let rec eval ((state, builtins, def, stmt_eval) as c) = function
      | Const n -> Int n
      | Var   x -> List.assoc x state
      | Ptr   p -> String p
      | EvalPtr (e, p) -> let String n = eval c e in
                            (eval c @@ Call(n, p))
      | Binop (op, l, r) -> eval' (eval c l)  (eval c r) op
      | Call (n, p) -> (let rec get_state = function
                          | (state, [], []) -> state
                          | (state, par::p', name::names') ->  get_state ((name, eval c par)::state, p', names')                                     
                        in
                        try 
                          (let (names, body) = List.assoc n def in
                           let fun_state  = get_state ([], p, names) in
                           let (fun_state', ret) = stmt_eval fun_state body in
                           match ret with
                           | Return e -> eval (fun_state', builtins, def, stmt_eval) e
                           | _ -> failwith "no return"
                          )
                        with Not_found -> 
                          let f = List.assoc n builtins in
                            Int (f @@ List.map (fun e -> let Int res = eval c e in res) p)
                        )
      | _ -> failwith "no, here"

  end
module Stmt =
  struct

    open Language.Stmt

    let eval builtins def stmt =
      let rec eval' state stmt =
        let expr_eval = Expr.eval (state, builtins, def, eval') in
	       match stmt with
        	| Skip          -> (state, Continue)
        	| Seq    (l, r) -> let (state', ret) as c = eval' state l in
                                   (match ret with
                                   | Continue -> eval' state' r
                                   | _ -> c)
        	| Assign (x, e) -> ((x, expr_eval e) :: state, Continue)
          | If(e, s1, s2) ->
            if (let (Int x) = expr_eval e in x != 0)
            then eval' state s1
            else eval' state s2
          | While(e, s1)  ->
            if (let (Int x) = expr_eval e in x != 0)
            then eval' state @@ Seq (s1, While (e, s1))
            else (state, Continue)
          | Repeat(s, e)  -> eval' state @@ Seq (s, (While (Binop("==", e, Const 0), s)))
          | Run(n, p)     -> expr_eval @@ Call (n, p);
                             (state, Continue)
          | Return v      -> (state, Return v)
        in
        let (_, ret) = eval' [] stmt in
        match ret with
        | Continue -> ()
        | _ -> failwith "nonzero return code"

  end
