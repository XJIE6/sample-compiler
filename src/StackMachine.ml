open Language

type i =
| S_PUSH  of int
| S_SPUSH
| S_LD    of string
| S_PLD   of string
| S_ST    of string
| S_BINOP of string
| S_JMP   of string
| S_CJMP  of string*string
| S_LBL   of string
| S_CALL
| S_RET
| S_FUN   of string*(string list)
| S_POP
| S_POP2

module Interpreter =
  struct
    open Interpreter.Expr
    let run builtins (main_code, fun_def) =
      let rec run'' (state, stack, code) = 
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
        let rec run' (state, stack, code) =
          (match code with
            | []       -> 0
            | i::code' ->
              if i = S_RET then 
                let (Int s)::stack' = stack in s
              else run'
                (match i with
                | S_POP2 -> (match stack with
                          | a::b::stack' -> (state, a::stack', code')
                          | _ -> (*Printf.printf "FAIL";*)
                                  (state, stack, code'))
                | S_POP -> let a::stack' = stack in
                  (*Printf.printf "pop";*)
                  (state, stack', code')
                | S_PUSH n ->
                  (*Printf.printf "push %d\n" n;*)
                  (state, (Int n)::stack, code')
                | S_SPUSH ->
                  (*Printf.printf "spush\n";*)
                  (state, stack, code')
                | S_LD x ->
                  (*Printf.printf "ld %s\n" x;*)
                  (state, (List.assoc x state)::stack, code')
                | S_PLD n ->
                  (*Printf.printf "pld %s\n" n;*)
                  (state, (String n)::stack, code')
                | S_ST x ->
                  (*Printf.printf "st %s\n" x;*)
                  let y::stack' = stack in
                  ((x, y)::state, stack', code')
                | S_BINOP s ->
                  let (Int y)::(Int x)::stack' = stack in
                  (state, (Int (Interpreter.Expr.eval' x y s))::stack', code')
                | S_JMP s -> 
                  (state, stack, jmp s)
                | S_CJMP (c, s) -> 
                   let (Int x)::stack' = stack in
                   (match c with
                   |"z"  when x =  0 -> (state, stack, jmp s) 
                   |"nz" when x <> 0 -> (state, stack, jmp s)
                   | _               -> (state, stack, code')
                   )
                | S_LBL s -> (state, stack, code')
                | S_CALL -> 
                    let (String n)::stack' = stack in
                      (try(
                      let fcode = List.find (fun ((S_FUN (f, _)::_)) -> f = n) fun_def in
                        let res = run'' ([], stack', fcode) in
                          (state, (Int res)::stack', code')
                      )
                    with
                    |Not_found -> 
                      (try (let f = List.assoc n builtins in
                            let rec values = function
                              | [] -> []
                              | (Int x)::arr -> x::(values arr)
                              | (String _)::arr -> values arr 
                              | _ -> failwith "wtf" in
                              (state, (Int (f @@ values stack')) ::stack', code'))
                       with 
                        |Not_found -> (*Printf.printf "no %s\n" n;*)
                                      (state, stack', code')
                      ))
                | S_FUN (f, p) -> let rec st stack = function
                     | [] -> []
                     | n::name -> let s::stack' = stack in
                        (n, s)::(st stack' name)
                    in (st stack @@ p, stack, code')
                | _ -> 
                  (*Printf.printf "OOOPS\n";*)
                  (state, stack, code')
                )
            )
          in run' (state, stack, code)
        in
        let res = run'' ([], [], main_code) in
    match res with
    | 0 -> ()
    | _ -> failwith "nonzero return code"
  end

module Compile =
  struct

    open Language.Expr
    open Language.Stmt

    let rec expr = function
      | Ptr   p -> [S_PLD  p]
      | Var   x -> [S_LD   x]
      | Const n -> [S_PUSH n]
      | Binop (s, x, y) -> expr x @ expr y @ [S_BINOP s]
      | EvalPtr (n, p) -> List.concat (List.map (fun p -> expr p @ [S_SPUSH]) (List.rev p)) @ [S_LD n; S_CALL] @ (List.map (fun p -> S_POP2)p)
      | Call (n, p) -> List.concat (List.map (fun p -> expr p @ [S_SPUSH]) (List.rev p)) @ [S_PLD n; S_CALL] @ (List.map (fun p -> S_POP2)p)

    let counter =
      let count = ref (0) in
      fun () ->
      incr count;
      !count
    
    let rec stmt fun_def st = 
      let rec stmt' = function
        | Skip           -> []
        | Assign  (x, e) -> expr e @ [S_ST x]
        | Seq     (l, r) -> stmt' l @ stmt' r
        | If (c, s1, s2) ->
          let lbl1 = ("L" ^ string_of_int(counter())) in
            let lbl2 = ("L" ^ string_of_int(counter())) in
              expr c @ [S_CJMP ("z", lbl1)] @ stmt' s1 @ [S_JMP lbl2; S_LBL lbl1] @ stmt' s2 @ [S_LBL lbl2]
        | While (c, s1) ->
          let lbl1 = "L" ^ string_of_int(counter()) in
            let lbl2 = "L" ^ string_of_int(counter()) in
              [S_LBL lbl1] @ expr c @ [S_CJMP ("z", lbl2)] @ stmt' s1 @ [S_JMP lbl1; S_LBL lbl2]
        | Repeat (s, c) ->
          let lbl  = "L" ^ string_of_int(counter()) in
            [S_LBL lbl] @ stmt' s @ expr c @ [S_CJMP ("z", lbl)]
        | Run    (n, p) -> expr (Call(n, p)) @ [S_POP]
        | Return      e -> expr e @ [S_RET]
      in ([S_FUN ("main", [])] @ stmt' st @ [S_PUSH 0; S_RET], List.map (fun (n, (a, b)) -> [S_FUN (n, a)] @ stmt' b) fun_def)
  end
