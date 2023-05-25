open List
open Nfa
open Sets

(*********)
(* Types *)
(*********)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(***********)
(* Utility *)
(***********)

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)

let rec create_nfa (regexp: regexp_t) state = 
  match regexp with
  |Empty_String -> 
    {
    sigma = [];
    qs = [state];
    q0 = state;
    fs = [state];
    delta = [];
  }
  |Char c -> 
    let state2 = (fresh ()) in 
    {
    sigma = [c];
    qs = [state; state2];
    q0 = state;
    fs = [state2];
    delta = [(state, Some c, state2)];
  }
  |Union (a,b) -> 
    let state0 = state in  
    let state_a = (fresh()) in
    let a2 = create_nfa a state_a in
    let state_b = (fresh()) in
    let b2 = create_nfa b state_b in
    let fin_state = (fresh ()) in    
    { 
    sigma = (union a2.sigma b2.sigma);
    qs = [state0; fin_state] @ (union a2.qs b2.qs);
    q0 = state0;
    fs = [fin_state];
    delta = [(state0, None, a2.q0); (state0, None, b2.q0)] @ (union a2.delta b2.delta) @
      List.fold_left (fun acc x -> insert (x, None, fin_state) acc) [] a2.fs @
      List.fold_left (fun acc x -> insert (x, None, fin_state) acc) [] b2.fs;
  }
  |Concat (a,b) ->
    let state_a = state in
    let a2 = create_nfa a state_a in
    let state_b = (fresh()) in
    let b2 = create_nfa b state_b in
    {
    sigma = (union a2.sigma b2.sigma);
    qs = (union a2.qs b2.qs);
    q0 = state_a;
    fs = b2.fs;
    delta = insert_all (union a2.delta b2.delta) 
        (List.fold_left (fun acc x -> insert (x, None, b2.q0) acc) [] a2.fs);
  }
  |Star a ->
    let state_a = state in
    let a2 = create_nfa a state_a in
    {
      sigma = a2.sigma;
      qs = a2.qs;
      q0 = a2.q0;
      fs = a2.fs;
      delta = insert_all (
              insert_all a2.delta (List.fold_left (fun acc x -> insert (a2.q0, None, x) acc) [] a2.fs)
              ) (List.fold_left (fun acc x -> insert (x, None, a2.q0) acc) [] a2.fs)
            ;
  }
  
let regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t =
  create_nfa regexp (fresh())

(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then [Tok_END]
    else if Str.string_match re_var s pos then
      let token = Str.matched_string s in
      Tok_Char token.[0] :: tok (pos + 1) s
    else if Str.string_match re_epsilon s pos then
      Tok_Epsilon :: tok (pos + 1) s
    else if Str.string_match re_union s pos then Tok_Union :: tok (pos + 1) s
    else if Str.string_match re_star s pos then Tok_Star :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else raise (IllegalExpression ("tokenize: " ^ s))
  in
  tok 0 str

let tok_to_str t =
  match t with
  | Tok_Char v -> Char.escaped v
  | Tok_Epsilon -> "E"
  | Tok_Union -> "|"
  | Tok_Star -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list =
    match tok_list with
    | [] -> raise (IllegalExpression "lookahead")
    | h :: t -> (h, t)
  in
  let rec parse_S l =
    let a1, l1 = parse_A l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Union ->
        let a2, l2 = parse_S n in
        (Union (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_A l =
    let a1, l1 = parse_B l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Char c ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_Epsilon ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_LParen ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_B l =
    let a1, l1 = parse_C l in
    let t, n = lookahead l1 in
    match t with Tok_Star -> (Star a1, n) | _ -> (a1, l1)
  and parse_C l =
    let t, n = lookahead l in
    match t with
    | Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
        let a1, l1 = parse_S n in
        let t2, n2 = lookahead l1 in
        if t2 = Tok_RParen then (a1, n2)
        else raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let rxp, toks = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")


let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str


