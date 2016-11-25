type opnd = R of int | S of int | M of string | L of int

let x86regs = [|
  "%eax"; 
  "%edx"; 
  "%ebx"; 
  "%ecx"; 
  "%esi"; 
  "%edi"
  |]

let x86byteRegs = [|
    "%al";
    "%bl";
    "%cl";
    "%dl";
    "%sl";
    "%dl";
  |]

let num_of_regs = Array.length x86regs
let word_size = 4

let eax = R 0
let ebx = R 2
let ecx = R 3
let edx = R 1
let esi = R 4
let edi = R 5

type instr =
| X86Binop of string * opnd * opnd
| X86Div   of opnd
| X86Mov   of opnd * opnd
| X86Cmp   of opnd * opnd
| X86Set   of string * opnd
| X86Push  of opnd
| X86Pop   of opnd
| X86Ret
| X86Cltd
| X86Call  of string
| X86Jmp   of string
| X86CJmp  of string * string
| X86Lbl   of string

module S = Set.Make (String)

class x86env =
  object(self)
    val    local_vars = ref S.empty
    method local x    = local_vars := S.add x !local_vars
    method local_vars = S.elements !local_vars

    val    allocated  = ref 0
    method allocate n = allocated := max (n+1) !allocated
    method allocated  = !allocated
  end

let allocate env stack =
  match stack with
  | []                              -> R 2
  | (S n)::_                        -> env#allocate (n+1); S (n+1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> env#allocate 0; S 0

module Show =
  struct

    let opnd = function
    | R i -> x86regs.(i)
    | S i -> Printf.sprintf "-%d(%%ebp)" (i * word_size)
    | M x -> x
    | L i -> Printf.sprintf "$%d" i

    let byteOpnd = function
      | R i -> x86byteRegs.(i)
      | op -> opnd op
                   
    let instr = function
    | X86Binop (op, s1, s2) -> Printf.sprintf "\t%s\t%s,\t%s"    op (opnd s1) (opnd s2)
    | X86Div   s            -> Printf.sprintf "\tidivl\t%s"        (opnd s)
    | X86Mov  (s1, s2)      -> Printf.sprintf "\tmovl\t%s,\t%s"  (opnd s1) (opnd s2)
    | X86Cmp  (s1, s2)      -> Printf.sprintf "\tcmpl\t%s,\t%s"  (opnd s1) (opnd s2)
    | X86Set  (op, s )      -> Printf.sprintf "\tset%s\t%s"      op (byteOpnd s)
    | X86Push  s            -> Printf.sprintf "\tpushl\t%s"      (opnd s )
    | X86Pop   s            -> Printf.sprintf "\tpopl\t%s"       (opnd s )
    | X86Ret                -> "\tret"
    | X86Call  s            -> Printf.sprintf "\tcall\t%s"       s
    | X86Cltd               -> Printf.sprintf "\tcltd"
    | X86Jmp   s            -> Printf.sprintf "\tjmp\t%s"        s
    | X86CJmp (c, s)        -> Printf.sprintf "\tj%s\t%s"        c s
    | X86Lbl   s            -> Printf.sprintf "\t%s:"            s
  end

module Compile =
  struct

    open StackMachine

    let stack_program env code =
      let rec compile stack code =
	match code with
	| []       -> []
	| i::code' ->
	    let (stack', x86code) =
              match i with
              | S_READ   -> ([eax], [X86Call "read"])
              | S_WRITE  -> ([], [X86Push (R 2); X86Call "write"; X86Pop (R 2)])
              | S_PUSH n ->
		  let s = allocate env stack in
		  (s::stack, [X86Mov (L n, s)])
              | S_LD x   ->
		  env#local x;
		  let s = allocate env stack in
		  (s::stack, [X86Mov (M x, eax); X86Mov (eax, s)])
              | S_ST x   ->
		  env#local x;
		  let s::stack' = stack in
		  (stack', [X86Mov (s, eax); X86Mov (eax, M x)])
              | S_JMP  s -> (stack,[X86Jmp  s])
              | S_CJMP (c, s) -> let x::stack' = stack in
                                 (stack',[X86Cmp (L 0, x); X86CJmp (c, s)])
              | S_LBL  s -> (stack,[X86Lbl  s])
	      | S_BINOP op ->
                 let decode_op = function
                   | "+" -> "add"
                   | "*" -> "imull"
                   | "-" -> "sub"
                   | "&&" -> "and"
                   | "!!" -> "or"
                   | ">"  -> "g"
                   | ">=" -> "ge"
                   | "<"  -> "l"
                   | "<=" -> "le"
                   | "==" -> "e"
                   | "!=" -> "ne"
                 in
                 let r::l::stack' = stack in
                 let move_param p = match p with
                   | R _ -> (p, [])
                   | _   -> (eax, [X86Mov (p, eax)])
                 in
                 let (p, move) = move_param(r) in
                 (l::stack', 
                 match op with
                 | ("+"|"-"|"*") -> move @ [X86Binop (decode_op op, p, l)]
                 | ("&&"|"!!")   -> [X86Mov(L 0, eax); X86Cmp (l, eax); X86Mov (L 0, edx); X86Set ("ne", edx);
                                     X86Cmp (r, eax); X86Set ("ne", eax); X86Binop (decode_op op, eax, edx); X86Mov(edx, l)]
                 | ("/"|"%")     -> [X86Mov (l, eax); X86Cltd; X86Div r] @ (match op with
                                                                         | "/" -> [X86Mov (eax, l)]
                                                                         | "%" -> [X86Mov (edx, l)])
                 | _             -> move @ [X86Cmp (p, l); X86Mov (L 0, eax); X86Set (decode_op op, eax); X86Mov(eax, l)])
	    in
	    x86code @ compile stack' code'
      in
      compile [] code

  end

let compile stmt =
  let env = new x86env in
  let code = Compile.stack_program env @@ StackMachine.Compile.stmt stmt in
  let asm  = Buffer.create 1024 in
  let (!!) s = Buffer.add_string asm s in
  let (!)  s = !!s; !!"\n" in
  !"\t.text";
  List.iter (fun x ->
      !(Printf.sprintf "\t.comm\t%s,\t%d,\t%d" x word_size word_size))
    env#local_vars;
  !"\t.globl\tmain";
  let prologue, epilogue =
    if env#allocated = 0
    then (fun () -> ()), (fun () -> ())
    else
      (fun () ->
         !"\tpushl\t%ebp";
         !"\tmovl\t%esp,\t%ebp";
         !(Printf.sprintf "\tsubl\t$%d,\t%%esp" (env#allocated * word_size))
      ),
      (fun () ->
         !"\tmovl\t%ebp,\t%esp";
         !"\tpopl\t%ebp"
      )
  in
  !"main:";
  prologue();
  List.iter (fun i -> !(Show.instr i)) code;
  epilogue();
  !"\txorl\t%eax,\t%eax";
  !"\tret";
  Buffer.contents asm

let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (compile stmt);
  close_out outf;
  ignore (Sys.command (Printf.sprintf "gcc-6 -m32 -o %s $RC_RUNTIME/runtime.o %s.s" name name))
