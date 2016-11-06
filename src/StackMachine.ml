type i =
| S_READ
| S_WRITE
| S_PUSH  of int
| S_LD    of string
| S_ST    of string
| S_BINOP of string
| S_JMP   of string
| S_CJMP  of string*string
| S_LBL   of string
               

module Interpreter =
  struct

    open Interpreter.Expr

    let run input code =
      let jmp s =
        let rec jmp' s commands =
          match  commands with
          | [] -> failwith ("no such lable: " ^ s)
          | cmd::commands' ->
             match cmd with
             | S_LBL s' when s = s' -> commands'
             | _  -> jmp' s commands'
        in jmp' s code
      in 
      let rec run' (state, stack, input, output, code) =
	(match code with
	| []       -> output
	| i::code' ->
	    run'
              (match i with
              | S_READ ->
		  let y::input' = input in
		  (state, y::stack, input', output, code')
              | S_WRITE ->
		  let y::stack' = stack in
		  (state, stack', input, output@[y], code')
              | S_PUSH n ->
		  (state, n::stack, input, output, code')
              | S_LD x ->
		  (state, (List.assoc x state)::stack, input, output, code')
              | S_ST x ->
		  let y::stack' = stack in
		  ((x, y)::state, stack', input, output, code')
              | S_BINOP s ->
                 let y::x::stack' = stack in
                 (state, (Interpreter.Expr.eval' x y s)::stack', input, output, code')
              | S_JMP s -> (state, stack, input, output, jmp s)
              | S_CJMP (c, s) -> 
                 let x::stack' = stack in
                 (match c with
                 |"z"  when x =  0 -> (state, stack, input, output, jmp s) 
                 |"nz" when x <> 0 -> (state, stack, input, output, jmp s)
                 | _               -> (state, stack, input, output, code')
                 )
              | S_LBL s -> (state, stack, input, output, code')
              )
          )
      in
      run' ([], [], input, [], code)
	
  end

module Compile =
  struct

    open Language.Expr
    open Language.Stmt

    let rec expr = function
    | Var   x -> [S_LD   x]
    | Const n -> [S_PUSH n]
    | Binop (s, x, y) -> expr x @ expr y @ [S_BINOP s]

    let counter =
      let count = ref (0) in
      fun () ->
      incr count;
      !count
       
    let rec  stmt = function
    | Skip           -> []
    | Assign  (x, e) -> expr e @ [S_ST x]
    | Read     x     -> [S_READ; S_ST x]
    | Write    e     -> expr e @ [S_WRITE]
    | Seq     (l, r) -> stmt l @ stmt r
    | If (c, s1, s2) ->
       let lbl1 = ("L" ^ string_of_int(counter())) in
       let lbl2 = ("L" ^ string_of_int(counter())) in
       expr c @ [S_CJMP ("z", lbl1)] @ stmt s1 @ [S_JMP lbl2; S_LBL lbl1] @ stmt s2 @ [S_LBL lbl2]
    | While  (c, s1) ->
       let lbl1 = "L" ^ string_of_int(counter()) in
       let lbl2 = "L" ^ string_of_int(counter()) in
       [S_LBL lbl1] @ expr c @ [S_CJMP ("z", lbl2)] @ stmt s1 @ [S_JMP lbl1; S_LBL lbl2]
    | Repeat (s, c) ->
       let lbl  = "L" ^ string_of_int(counter()) in
       [S_LBL lbl] @ stmt s @ expr c @ [S_CJMP ("z", lbl)]
  end
