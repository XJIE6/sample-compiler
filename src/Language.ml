open Ostap 
open Matcher

module Expr =
  struct

  type t =
    | Const of int
    | Var   of string
    | Binop of string * t * t
    | Call  of string * t list

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
      | x:IDENT args:(-"(" !(Util.list0 ori) -")")?
                                                    { match args with
                                                      | None -> Var x
                                                      | Some args -> Call (x, args)
                                                    }
      | -"(" ori -")"
)

  end

module Stmt =
  struct

    type t =
    | Skip
    | Read   of string
    | Write  of Expr.t
    | Assign of string * Expr.t
    | Seq    of t * t
    | If     of Expr.t * t * t
    | While  of Expr.t * t
    | Repeat of t * Expr.t
    | Run   of string * Expr.t list
    | Return of Expr.t

    ostap (
      parse: s:simple d:(-";" parse)? {
	match d with None -> s | Some d -> Seq (s, d)
                                    };
      expr:!(Expr.ori);
      simple:
      x:IDENT s:(":=" e:expr {Assign (x, e)} |
                 "(" args:!(Util.list0 expr) ")" {Run (x, args)}
                ) {s}
      | %"read"  "(" x:IDENT ")"       {Read x}
      | %"write" "(" e:!(Expr.ori) ")" {Write e}
      | %"skip"                        {Skip}
      | %"return" e:!(Expr.ori)        {Return e}
      | %"if" c:!(Expr.ori)
        %"then" s:(parse)
        elif:(%"elif" !(Expr.ori) %"then" parse)*
        els:(%"else" parse)? %"fi"
        {If(c, s, List.fold_right (fun (c, s) elif -> If (c, s, elif)) elif (match els with
                                                                             |None   -> Skip
                                                                             |Some s -> s))}
      | %"while" c:!(Expr.ori) %"do" s:(parse) %"od" {While(c, s)}
      | %"repeat" s:(parse) %"until" c:!(Expr.ori) {Repeat(s, c)}
      | %"for" s1:(parse) "," c:!(Expr.ori) "," s2:(parse) %"do" s:(parse) %"od"
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
