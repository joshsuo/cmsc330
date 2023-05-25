open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* PASTE YOUR PARSERS FROM P4A HERE *)

(* get id from Tok_ID *)
let getId id = 
  match id with
  |Some (Tok_ID a) -> a
  |_ -> raise(InvalidInputException("no id"))

let rec parse_expr toks = 
  let first = lookahead toks in 
  match first with
  |(Some Tok_Let) -> parse_let_expr toks 
  |(Some Tok_Fun) -> parse_fun_expr toks 
  |(Some Tok_If) -> parse_if_expr toks 
  |_ -> parse_or_expr toks 

and parse_let_expr toks = 
    let lst = match_token toks Tok_Let in 
    if (lookahead lst) = (Some Tok_Rec) then
      let lst2 = match_token lst Tok_Rec in 
      let e3 = getId (lookahead lst2) in
      let lst3 = match_token lst2 (Tok_ID e3) in 
      let lst4 = (match_token lst3 Tok_Equal) in 
      let (lst5, e5) = parse_expr lst4 in         
      let lst6 = (match_token lst5 Tok_In) in 
      let (lst7, e7) = parse_expr lst6 in 
      (lst7, Let(e3,true,e5,e7 )) 
    else
      let e3 = getId (lookahead lst) in
      let lst3 = match_token lst (Tok_ID e3) in 
      let lst4 = (match_token lst3 Tok_Equal) in 
      let (lst5, e5) = parse_expr lst4 in 
      let lst6 = (match_token lst5 Tok_In) in 
      let (lst7, e7) = parse_expr lst6 in 
      (lst7, Let(e3,false,e5,e7)) 

and parse_fun_expr toks = 
  let lst = (match_token toks Tok_Fun) in
  let e2 = getId (lookahead lst) in
  let lst2 = match_token lst (Tok_ID e2) in 
  let lst3 = (match_token lst2 Tok_Arrow) in
  let (lst4, e4) = parse_expr lst3 in 
  (lst4, Fun(e2, e4))  

and parse_if_expr toks = 
  let lst = (match_token toks Tok_If) in 
  let (lst2, e2) = parse_expr lst in 
  let lst3 = (match_token lst2 Tok_Then) in 
  let (lst4, e4) = parse_expr lst3 in 
  let lst5 = (match_token lst4 Tok_Else) in 
  let (lst6, e6) = parse_expr lst5 in 
  (lst6, If(e2, e4, e6))      

and parse_or_expr toks = 
  let (lst, e1) = parse_and_expr toks in 
  match lst with 
  |Tok_Or::t -> let lst2 = (match_token lst Tok_Or) in 
    let (lst3,e2) = parse_or_expr lst2 in (lst3, Binop(Or, e1, e2))
  |_ -> (lst, e1)

and parse_and_expr toks = 
  let (lst, e1) = parse_eq_expr toks in 
  match lst with 
  |Tok_And::t -> let lst2 = (match_token lst Tok_And) in 
    let (lst3,e2) = parse_and_expr lst2 in (lst3, Binop(And, e1, e2))
  |_ -> (lst, e1)

and parse_eq_expr toks = 
  let (lst, e1) = parse_rel_expr toks in 
  match lst with 
  |Tok_Equal::t -> let lst2 = (match_token lst Tok_Equal) in 
    let (lst3, e2) = parse_eq_expr lst2 in (lst3, Binop(Equal,e1,e2))
  |Tok_NotEqual::t -> let lst2 = (match_token lst Tok_NotEqual) in 
    let (lst3, e2) = parse_eq_expr lst2 in (lst3, Binop(NotEqual,e1,e2))
  |_ -> (lst, e1)

and parse_rel_expr toks = 
  let (lst, e1) = parse_add_expr toks in 
  match lst with 
  |Tok_Less::t -> let lst2 = (match_token lst Tok_Less) in 
    let (lst3, e2) = parse_rel_expr lst2 in (lst3, Binop(Less, e1, e2))
  |Tok_Greater::t -> let lst2 = (match_token lst Tok_Greater) in 
    let (lst3, e2) = parse_rel_expr lst2 in (lst3, Binop(Greater, e1, e2))
  |Tok_LessEqual::t -> let lst2 = (match_token lst Tok_LessEqual) in 
    let (lst3, e2) = parse_rel_expr lst2 in (lst3, Binop(LessEqual, e1, e2))
  |Tok_GreaterEqual::t -> let lst2 = (match_token lst Tok_GreaterEqual) in 
    let (lst3, e2) = parse_rel_expr lst2 in (lst3, Binop(GreaterEqual, e1, e2))
  |_ -> (lst, e1)

and parse_add_expr toks = 
  let (lst, e1) = parse_mult_expr toks in 
  match lst with
  |Tok_Add::t -> let lst2 = match_token lst Tok_Add in 
    let (lst3, e2) = parse_add_expr lst2 in (lst3, Binop(Add, e1, e2))
  |Tok_Sub::t -> let lst2 = match_token lst Tok_Sub in 
    let (lst3, e2) = parse_add_expr lst2 in (lst3, Binop(Sub, e1, e2))
  |_ -> (lst, e1)

and parse_mult_expr toks = 
  let (lst, e1) = parse_concat_expr toks in 
  match lst with
  |Tok_Mult::t -> let lst2 = match_token lst Tok_Mult in 
    let (lst3, e2) = parse_mult_expr lst2 in (lst3, Binop(Mult, e1, e2))
  |Tok_Div::t -> let lst2 = match_token lst Tok_Div in 
    let (lst3, e2) = parse_mult_expr lst2 in (lst3, Binop(Div, e1, e2))
  |_ -> (lst, e1)

and parse_concat_expr toks = 
  let (lst, e1) = parse_unary_expr toks in 
  match lst with 
  |Tok_Concat::t -> let lst2 = (match_token lst Tok_Concat) in 
    let (lst3, e2) = (parse_concat_expr lst2) in 
      (lst3, Binop(Concat, e1, e2))
  |_ -> (lst, e1)

and parse_unary_expr toks = 
  match toks with 
  |Tok_Not::t -> let lst = (match_token toks Tok_Not) in 
    let (lst2, e2) = (parse_unary_expr lst) in (lst2, Not e2)
  |_ -> parse_fun_call_expr toks 

and parse_fun_call_expr toks =  
let (lst, e2) = parse_primary_expr toks in 
 let e = lookahead lst in
  match e with 
  |(Some (Tok_Int i)) -> let (lst2, e3) = parse_primary_expr lst in (lst2, FunctionCall(e2, e3))
  |(Some (Tok_Bool b)) -> let (lst2, e3) = parse_primary_expr lst in (lst2, FunctionCall(e2, e3))
  |(Some (Tok_String s)) -> let (lst2, e3) = parse_primary_expr lst in (lst2, FunctionCall(e2, e3))
  |(Some (Tok_ID s)) -> let (lst2, e3) = parse_primary_expr lst in (lst2, FunctionCall(e2, e3))
  |(Some Tok_LParen) -> let (lst2, e3) = parse_primary_expr lst in (lst2, FunctionCall(e2, e3))
  |None -> (lst, e2)
  |_ -> (lst, e2)

and parse_primary_expr toks = 
  match toks with
  |Tok_Int i::t -> let lst = (match_token toks (Tok_Int i)) in (lst, Value(Int i))
  |Tok_Bool b::t -> let lst = (match_token toks (Tok_Bool b)) in (lst, Value(Bool b))
  |Tok_String s::t -> let lst = (match_token toks (Tok_String s)) in (lst, Value(String s))
  |Tok_ID s::t -> let lst = (match_token toks (Tok_ID s)) in (lst, (ID s))
  |Tok_LParen::t -> let lst = (match_token toks Tok_LParen) in 
    (
      let (lst2, e2) = parse_expr lst in 
      match lst2 with 
        |Tok_RParen::t -> (t, e2)
        |_ -> raise(InvalidInputException("right paren is missing"))
    )
  |_ -> raise (InvalidInputException("error in parse_primary_expr"))

let rec parse_mutop toks = 
  let e = lookahead toks in 
  match e with
  |(Some Tok_Def) -> parse_def_utop toks 
  |(Some Tok_DoubleSemi) -> let lst2 = match_token toks Tok_DoubleSemi in (lst2, NoOp)
  |_ -> parse_expr_mutop toks

and parse_def_utop toks = 
  let lst = (match_token toks Tok_Def) in 
  let e2 = getId (lookahead lst) in
  let lst2 = match_token lst (Tok_ID e2) in 
  let lst22 = match_token lst2 Tok_Equal in 
  let (lst3, e3) = parse_expr lst22 in 
  let lst4 = match_token lst3 Tok_DoubleSemi in 
  (lst4, Def(e2, e3))

and parse_expr_mutop toks =
  let (lst, e) = parse_expr toks in 
  let lst2 = match_token lst Tok_DoubleSemi in 
    (lst2, Expr e)