open Ostap 
open Matcher

module Expr =
  struct

    type t =
    | Const of int
    | Var   of string
    | Binop of string * t * t

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
      | x:IDENT   {Var   x}
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

    ostap (
      parse: s:simple d:(-";" parse)? {
	match d with None -> s | Some d -> Seq (s, d)
      };
      simple:
        x:IDENT ":=" e:!(Expr.ori)     {Assign (x, e)}
      | %"read"  "(" x:IDENT ")"         {Read x}
      | %"write" "(" e:!(Expr.ori) ")" {Write e}
      | %"skip"                          {Skip}
      | %"if" c:!(Expr.ori) "then" s1:(parse) "else" s2:(parse) "fi" {If(c, s1, s2)}
      | %"while" c:!(Expr.ori) "do" s:(parse) "od" {While(c, s)}
    )

  end
