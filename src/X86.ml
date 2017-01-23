type opnd = R of int | S of int | M of string | L of int | P of string

let x86regs = [|
  "%eax";
  "%ebx";
  "%edx";
  "%ebp";
  "%esp";
  "%ecx";
  "%esi";
  "%edi"
  |]

let x86byteRegs = [|
    "%al";
    "%bl";
    "%dl";
  |]

let num_of_regs = Array.length x86regs
let word_size = 4

  let eax = R 0
  let ebx = R 1
  let edx = R 2
  let ebp = R 3
  let esp = R 4
  let ecx = R 5
  let esi = R 6
  let edi = R 7

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
| X86Call
| X86Jmp   of string
| X86CJmp  of string * string
| X86Lbl   of string

module M = Map.Make (String)

class x86env =
  object(self)
    val    local_vars = ref M.empty
    val    local_cnt  = ref 0
    val    args       = ref M.empty
    val    args_cnt   = ref 1
    
    method local x    = if not (M.mem x !local_vars) && not (M.mem x !args)
                        then local_vars := M.add x (!local_cnt + 1) !local_vars;
                             local_cnt := !local_cnt + 1;
    method local_n    = !local_cnt

    method arg x      = if not (M.mem x !local_vars) && not (M.mem x !args)
                        then args := M.add x (!args_cnt + 1) !args;
                             args_cnt := !args_cnt + 1
    
    method get_shift x = word_size * (if M.mem x !args then M.find x !args
                                          else -(M.find x !local_vars))

    val    allocated  = ref 0
    method allocate n = allocated := max n !allocated
    method allocated  = !allocated
  end

let allocate env stack =
  match stack with
  | []                              -> R 5
  | (S n)::_                        -> env#allocate (n+1); S (n+1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> env#allocate 1; S 1

module Show =
  struct
    let print env = 
      let opnd = function
        | R i -> x86regs.(i)
        | S i -> Printf.sprintf "-%d(%%ebp)" ((env#local_n + i) * word_size)
        | M x -> Printf.sprintf "%d(%%ebp)" (env#get_shift x)
        | L i -> Printf.sprintf "$%d" i
        | P n -> Printf.sprintf "$%s" n
      in
      let byteOpnd = function
        | R i -> x86byteRegs.(i)
        | op -> opnd op
      in        
      function
      | X86Binop (op, s1, s2) -> Printf.sprintf "\t%s\t%s,\t%s"    op (opnd s1) (opnd s2)
      | X86Div   s            -> Printf.sprintf "\tidivl\t%s"        (opnd s)
      | X86Mov  (s1, s2)      -> Printf.sprintf "\tmovl\t%s,\t%s"  (opnd s1) (opnd s2)
      | X86Cmp  (s1, s2)      -> Printf.sprintf "\tcmpl\t%s,\t%s"  (opnd s1) (opnd s2)
      | X86Set  (op, s )      -> Printf.sprintf "\tset%s\t%s"      op (byteOpnd s)
      | X86Push  s            -> Printf.sprintf "\tpushl\t%s"      (opnd s )
      | X86Pop   s            -> Printf.sprintf "\tpopl\t%s"       (opnd s )
      | X86Ret                -> "\tret"
      | X86Call               -> Printf.sprintf "\tcall\t*%%eax"       
      | X86Cltd               -> Printf.sprintf "\tcltd"
      | X86Jmp   s            -> Printf.sprintf "\tjmp\t%s"        s
      | X86CJmp (c, s)        -> Printf.sprintf "\tj%s\t%s"        c s
      | X86Lbl   s            -> Printf.sprintf "%s:"              s
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
              | S_PUSH n ->
                let s = allocate env stack in
                  (s::stack, [X86Mov (L n, s)])
              | S_SPUSH  ->
                let s::stack' = stack in (stack', [X86Mov (s, eax); X86Push eax])
              | S_LD x   ->
                env#local x;
                let s = allocate env stack in
                  (s::stack, [X86Mov (M x, eax); X86Mov (eax, s)])
              | S_PLD n  ->
                let s = allocate env stack in
                  (s::stack, [X86Mov (P n, s)])
              | S_ST x   ->
                env#local x;
                let s::stack' = stack in
                  (stack', [X86Mov (s, eax); X86Mov (eax, M x)])
              | S_POP2   -> (stack, [X86Pop eax])
              | S_POP    ->
                  let s::stack' = stack in
                  (stack', [X86Mov (s, eax)])
              | S_JMP  s -> (stack,[X86Jmp  s])
              | S_CJMP (c, s) -> 
                let x::stack' = stack in
                  (stack',[X86Cmp (L 0, x); X86CJmp (c, s)])
              | S_LBL  s -> (stack,[X86Lbl  s])
              | S_RET    -> 
                let x::stack' = stack in
                  (stack', [X86Mov (x, eax); X86Pop edi; X86Pop esi; X86Pop ecx; X86Mov (ebp, esp); X86Pop ebp; X86Ret])
              | S_FUN (f, a) -> 
                List.iter (fun x -> env#arg x) a;
                (stack, [X86Lbl f; X86Push ecx; X86Push esi; X86Push edi])
              | S_CALL   ->
                let s::stack' = stack in
                  (stack, [X86Mov (s, eax); X86Call; X86Mov (eax, s)])
              | S_BINOP op ->
                 let decode_op = function
                   | "+" -> "addl"
                   | "*" -> "imull"
                   | "-" -> "subl"
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
                 let back_param p = (if p = l then 
                                    [] else
                                    [X86Mov (p, l)])
                 in
                 let (p, move) = move_param(l) in
                 let back = back_param(p) in
                 (l::stack', 
                 match op with
                 | ("+"|"-"|"*") -> move @ [X86Binop (decode_op op, r, p)] @ back
                 | ("&&"|"!!")   -> [X86Mov(L 0, eax); X86Cmp (l, eax); X86Mov (L 0, edx); X86Set ("ne", edx);
                                     X86Cmp (r, eax); X86Set ("ne", eax); X86Binop (decode_op op, eax, edx); X86Mov(edx, l)]
                 | ("/"|"%")     -> [X86Mov (l, eax); X86Cltd; X86Div r] @ (match op with
                                                                         | "/" -> [X86Mov (eax, l)]
                                                                         | "%" -> [X86Mov (edx, l)])
                 | _             -> move @ [X86Cmp (r, p); X86Mov (L 0, eax); X86Set (decode_op op, eax); X86Mov(eax, l)])
      in
      x86code @ compile stack' code'
      in
      compile [] code

  end

let compile def stmt =
  let (s_main, s_funcs) = StackMachine.Compile.stmt def stmt in
  let funcs = List.map 
     (fun f -> let env = new x86env in
        (env, Compile.stack_program env f)
      ) s_funcs  in
  let main = 
      let env = new x86env in
        (env, Compile.stack_program env s_main) in
  let asm  = Buffer.create 1024 in
  let (!!) s = Buffer.add_string asm s in
  let (!)  s = !!s; !!"\n" in
  !"\t.text";
  !"\t.globl\tmain";
  let prologue env =
      !"\tpushl\t%ebp";
      !"\tmovl\t%esp,\t%ebp";
      !(Printf.sprintf "\tsubl\t$%d,\t%%esp" ((env#local_n + env#allocated) * word_size))
  in

  List.iter (fun (env, code) ->
      !(Show.print env @@ List.hd code);
      prologue env;
      List.iter (fun i -> !(Show.print env i)) @@ List.tl code;
  ) funcs;
  let (env, code) = main in
  !(Show.print env @@ List.hd code);
  prologue env;
  List.iter (fun i -> !(Show.print env i)) @@ List.tl code;
  Buffer.contents asm

let build def stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (compile def stmt);
  close_out outf;
  match Sys.command (Printf.sprintf "gcc -m32 -o %s $RC_RUNTIME/runtime.o %s.s" name name) with
  | 0 -> ()
  | _ -> failwith "gcc failed with non-zero exit code"