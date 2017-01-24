open Ostap 
open Matcher

type var =
| Int of int
| String of string

module Expr =
  struct

  type t =
    | Const   of int
    | Var     of string
    | Ptr     of string
    | Binop   of string * t * t
    | Call    of string * t list
    | EvalPtr of t * t list

  ostap (
      ori:
        l:andi suf:(("!!") andi)* {
           List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
      }
      | andi;
      andi:
        l:eqsi suf:(("&&") eqsi)* {
           List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
      }
      | eqsi;
                                  
      eqsi:
        l:addi suf:(("<=" | "<" | "==" | "!=" | ">=" | ">") addi)* {
           List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
        }
      | addi;

      addi:
        l:mulli suf:(("+" | "-") mulli)* {
          List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
        }
      | mulli;

      mulli:
        l:primary suf:(("*" | "/" | "%") primary)* {
           List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
        }
      | primary;

      primary:
        n:DECIMAL {Const n}
      | x:funcs "->" args:(-"(" !(Util.list0 ori) -")") {EvalPtr (x, args)}
      | funcs;

      funcs:
        "&" x:IDENT {Ptr x}
      | x:IDENT args:(-"(" !(Util.list0 ori) -")")?
                                                    { match (args) with
                                                      | (Some args) -> Call (x, args)
                                                      | (None) -> Var x
                                                    }
      | -"(" ori -")"
)

  end

module Stmt =
  struct

    type t =
    | Skip
    | Assign of string * Expr.t
    | Seq    of t * t
    | If     of Expr.t * t * t
    | While  of Expr.t * t
    | Repeat of t * Expr.t
    | Run    of string * Expr.t list
    | Return of Expr.t

    ostap (
      parse: s:simple d:(-";" parse)? {
	match d with None -> s | Some d -> Seq (s, d)
                                    };
      expr:!(Expr.ori);
      simple:
      x:IDENT s:("(" args:!(Util.list0 expr) ")" {Run (x, args)} |
                ":=" e:expr {Assign (x, e)}
                ) {s}
      | %"skip"                        {Skip}
      | %"return" e:expr        {Return e}
      | %"if" c:expr
        %"then" s:(parse)
        elif:(%"elif" expr %"then" parse)*
        els:(%"else" parse)? %"fi"
        {If(c, s, List.fold_right (fun (c, s) elif -> If (c, s, elif)) elif (match els with
                                                                             |None   -> Skip
                                                                             |Some s -> s))}
      | %"while" c:expr %"do" s:(parse) %"od" {While(c, s)}
      | %"repeat" s:(parse) %"until" c:expr {Repeat(s, c)}
      | %"for" s1:(parse) "," c:expr "," s2:(parse) %"do" s:(parse) %"od"
        {Seq(s1, While(c, Seq(s, s2)))}
    )

  end

module Def = 
  struct

    type t = string * (string list * Stmt.t)

    ostap (
      param: IDENT;                    
      parse: %"fun" name: IDENT "(" args: !(Util.list0 param) ")" %"begin" body: !(Stmt.parse) %"end" {(name, (args, body))}
    )
  end

module Unit =
  struct

    type t = Def.t list * Stmt.t

    ostap (
      parse: !(Def.parse)* !(Stmt.parse)
    )
  end
