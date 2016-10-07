module Expr =
  struct

    open Language.Expr

    let rec eval state = function
    | Const  n -> n
    | Var    x -> state x
    | Binop  (op, l, r) ->
       let (left, right) = (eval state l, eval state r) in
       match op with
       |"+" -> left + right
       |"-" -> left - right
       |"*" -> left * right
       |"/" -> left / right
       |"%" -> left mod right
       |">" -> if left > right then 1 else 0
       |"<" -> if left < right then 1 else 0
       |">=" -> if left >= right then 1 else 0
       |"<=" -> if left <= right then 1 else 0
       |"==" -> if left == right then 1 else 0
       |"!=" -> if left != right then 1 else 0
       |"&&" -> if left != 0 && right != 0 then 1 else 0
       |"!!" -> if left != 0 || right != 0 then 1 else 0
 
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
      in
      let (_, _, result) = eval' ([], input, []) stmt in
      result

  end
