open Ostap

let parse infile =
  let s = Util.read infile in
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.ident ["skip"; "if"; "fi"; "elif"; "else"; "while"; "do"; "od"; "repeat"; "until"; "for"] s
       inherit Util.Lexers.decimal s
       inherit Util.Lexers.skip [
	 Matcher.Skip.whitespaces " \t\n";
	 Matcher.Skip.lineComment "--";
	 Matcher.Skip. nestedComment "(*" "*)"
       ] s
     end
    )
    (ostap (!(Language.Unit.parse) -EOF))

let main = 
  try
    let mode, filename =
      
      match Sys.argv.(1) with
      | "-s" -> `SM , Sys.argv.(2)
      | "-o" -> `X86, Sys.argv.(2)
      | "-i" -> `Int, Sys.argv.(2)
    in
    match parse filename with
    | `Ok (def, stmt) -> 
	(match mode with
	 | `X86 ->
             let basename = Filename.chop_suffix filename ".expr" in 
	     X86.build def stmt basename
	 | _ ->
        let builtins = [("read",  fun arr -> Printf.printf "> ";
                                             read_int());
                        ("write", fun arr -> let x::arr' = arr in
                                              Printf.printf "%d\n" x;
                                              0)] in
	    match mode with
	          | `SM -> StackMachine.Interpreter.run builtins @@ StackMachine.Compile.stmt def stmt
            (* | `SM -> let (a, b::_) = StackMachine.Compile.stmt def stmt in
                Printf.printf "%s\n" (String.concat "\n" @@ List.map
            (fun x -> match x with
                      | S_POP           -> "POP"
                      | S_POP2          -> "POP2"
                      | S_PUSH x        -> Printf.sprintf "PUSH %d" x
                      | S_LD s          -> Printf.sprintf "LD %s" s
                      | S_ST s          -> Printf.sprintf "SST %s" s
                      | S_BINOP s       -> Printf.sprintf "BINOP %s" s
                      | S_LBL s         -> Printf.sprintf "LABEL %s" s
                      | S_JMP s         -> Printf.sprintf "JMP %s" s
                      | S_CJMP (c, s)   -> Printf.sprintf "CJMP %s %s" c s
                      | S_CALL s        -> Printf.sprintf "CALL %s" s
                      | S_FUN (s, a)    -> Printf.sprintf "FUN: %s ARGS: %s" s (String.concat " " a)
                      | S_RET           -> "RET"
        ) (a @ [S_LD "lol"] @ b)) *)
            | _   -> Interpreter.Stmt.eval builtins def stmt
	 )

    | `Fail er -> Printf.eprintf "%s" er
    with 
  | Invalid_argument _ -> Printf.printf "Usage: rc.byte <name.expr>"
