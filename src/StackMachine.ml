type i =
| S_READ
| S_WRITE
| S_PUSH  of int
| S_LD    of string
| S_ST    of string
| S_BINOP of string

module Interpreter =
  struct

    let run input code =
      let rec run' (state, stack, input, output) code =
	match code with
	| []       -> output
	| i::code' ->
	    run'
              (match i with
              | S_READ ->
		  let y::input' = input in
		  (state, y::stack, input', output)
              | S_WRITE ->
		  let y::stack' = stack in
		  (state, stack', input, output @ [y])
              | S_PUSH n ->
		  (state, n::stack, input, output)
              | S_LD x ->
		  (state, (List.assoc x state)::stack, input, output)
              | S_ST x ->
		  let y::stack' = stack in
		  ((x, y)::state, stack', input, output)
              | S_BINOP s ->
                 let x::y::stack' = stack in
                 let bool x = if x == 0 then false else true in
                 let argBool op = fun x y -> op (bool x) (bool y) in
                 let int func = fun x y -> if func x y then 1 else 0 in
                 let op = match s with
                   | "+" -> (+)
                   | "-" -> (-)
                   | "*" -> ( * ) 
                   | "/" -> (/)
                   | "%" -> (mod)
                   | "<" -> int (<)
                   | ">" -> int (>)
                   | "<=" -> int (<=)
                   | ">=" -> int (>=)
                   | "!=" -> int (!=)
                   | "==" -> int (==)
                   | "&&" -> int (argBool (&&))
                   | "!!" -> int (argBool (||))
                   | _ -> failwith "wrong operation"
                 in
                 (state, (op x y)::stack', input, output)
              )
              code'
      in
      run' ([], [], input, []) code
	
  end

module Compile =
  struct

    open Language.Expr
    open Language.Stmt

    let rec expr = function
    | Var   x -> [S_LD   x]
    | Const n -> [S_PUSH n]
    | Binop (s, x, y) -> expr y @ expr x @ [S_BINOP s] (*wrong argument sequence*)

    let rec stmt = function
    | Skip          -> []
    | Assign (x, e) -> expr e @ [S_ST x]
    | Read    x     -> [S_READ; S_ST x]
    | Write   e     -> expr e @ [S_WRITE]
    | Seq    (l, r) -> stmt l @ stmt r

  end
