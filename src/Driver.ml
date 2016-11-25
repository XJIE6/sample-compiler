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

let main = ()
  try
    let mode, filename =
      match Sys.argv.(1) with
      | "-s" -> `SM , Sys.argv.(2)
      | "-o" -> `X86, Sys.argv.(2)
      | _    -> `Int, Sys.argv.(1)
    in
    match parse filename with
    | `Ok (def, stmt) -> 
	(match mode with
	 | `X86 ->
             let basename = Filename.chop_suffix filename ".expr" in 
	     X86.build stmt basename
	 | _ ->
        let builtins = [("read",  fun arr -> if arr = [] then 
                                           (Printf.printf "> ";
                                           read_int())
                                           else failwith "params in read");
                        ("write", fun arr -> match arr with
                                            | x::[] -> (Printf.printf "%d\n" x; 0)
                                            | []   -> failwith "no args in write"
                                            | _    -> failwith "too many args in write")] in
	    match mode with
	       (* | `SM -> StackMachine.Interpreter.run builtins @@ StackMachine.Compile.stmt stmt
	        *)| _   -> Interpreter.Stmt.eval builtins def stmt
	)

    | `Fail er -> Printf.eprintf "%s" er
    with 
  | Invalid_argument _ -> Printf.printf "Usage: rc.byte <name.expr>"
