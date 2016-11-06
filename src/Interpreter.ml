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
                                                         
    let rec eval state = function
      | Const n -> n
      | Var   x -> state x
      | Binop (op, l, r) -> eval' (eval state l) (eval state r) op                   
     
  end
  
module Stmt =
  struct

    open Language.Stmt

    let eval input stmt =
      let rec eval' ((state, input, output) as c) stmt =
	let state' x = List.assoc x state in
	match stmt with
	| Skip          -> c
	| Seq    (l, r) -> eval' (eval' c l) r
	| Assign (x, e) -> ((x, Expr.eval state' e) :: state, input, output)
	| Write   e     -> (state, input, output @ [Expr.eval state' e])
	| Read    x     ->
	    let y::input' = input in
	    ((x, y) :: state, input', output)
        | If(e, s1, s2) -> if (Expr.eval state' e) != 0 then eval' c s1 else eval' c s2
        | While(e, s1)  -> if (Expr.eval state' e) != 0 then eval' (eval' c s1) (While (e, s1)) else c
        | Repeat(s, e)  -> eval' (eval' c s) (While (Binop("==", e, Const 0), s))
      in
      let (_, _, result) = eval' ([], input, []) stmt in
      result

  end
