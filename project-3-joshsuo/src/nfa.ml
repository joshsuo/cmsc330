open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  List.fold_left (fun acc (q1,a,q2) -> 
    if elem q1 qs = true && a = s then (insert q2 acc) else acc) [] nfa.delta

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec find_none nfa qs acc = 
    let none_states = (move nfa qs None) in
    let check = diff none_states acc in
    if check <> [] then find_none nfa none_states (insert_all acc qs) else (insert_all qs acc)
  in find_none nfa qs []

let accept (nfa: ('q,char) nfa_t) (s: string) : bool = 
  if s = "" then 
    (intersection (e_closure nfa [nfa.q0]) nfa.fs) <> []
  else
    let letters = explode s in
    let rec accept_aux letters qs = 
      match letters with
      [] -> (intersection qs nfa.fs) <> []
      |h::t -> accept_aux t (e_closure nfa (move nfa qs (Some h)))
    in accept_aux letters (e_closure nfa [nfa.q0])

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_left (fun a x ->
    a@[List.fold_left (fun a2 (q1,s1,q2) ->
      if List.mem q1 qs && s1 = Some x then a2@(e_closure nfa [q2]) else a2 ) [] nfa.delta] ) [] nfa.sigma
  (*let rec states nfa_delta qs = 
    match nfa_delta with
    [] -> []
    |(q1,a,q2)::t -> if List.mem q1 qs then e_closure nfa [q2]::states t qs else states t qs
  in states nfa.delta qs*)

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.fold_left (fun acc x ->
    acc@[(qs, Some x , (List.fold_left (fun acc2 (q1,s,q2) ->
      if elem q1 qs && s = Some x then acc2@(e_closure nfa [q2]) else acc2 ) [] nfa.delta))] ) [] nfa.sigma
  (*let rec states nfa_delta qs =
    match nfa_delta with
    [] -> []
    |(q1,a,q2)::t -> if List.mem q1 qs then (qs,a,e_closure nfa [q2])::states t qs else states t qs
  in states nfa.delta qs*)

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if intersection nfa.fs qs <> [] then [qs] else []
  (*let rec states nfa qs = 
    match qs with
    [] -> false
    |h::t -> if List.mem h nfa.fs then true else states nfa t
  in if states nfa qs then [qs] else []*)

(* OPTIONAL *)
let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t = 
    failwith "unimplemented"

let rec removeAll lst x = 
   match lst with
   [] -> lst
   |h::t -> if h = x then removeAll t x else h::(removeAll t x)

let rec dfa_states (nfa: ('q,'s) nfa_t) states acc = 
  match states with
  [] -> acc
  |h::t -> if h = [] || List.mem h acc then dfa_states nfa t acc 
  else if (removeAll (new_states nfa h) []) = []
    then dfa_states nfa t (acc@[h]) else dfa_states nfa (new_states nfa h) (acc@[h])

let rec fin_state (nfa: ('q,'s) nfa_t) states acc = 
  match states with
  [] -> acc
  |h::t -> if (intersection h nfa.fs) <> [] then fin_state nfa t (acc@[h]) else fin_state nfa t acc

let rec delta_clear dfa_delta accum =
  match dfa_delta with
  [] -> accum
  |(q1,a,q2)::t -> if q2 = [] then delta_clear t accum else delta_clear t (accum@[(q1,a,q2)])

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =  
  let dfa_qs = dfa_states nfa (new_states nfa (e_closure nfa [nfa.q0])) [(e_closure nfa [nfa.q0])] in
  let dfa_fs = fin_state nfa dfa_qs [] in
  let dfa_delta = List.fold_left (fun acc x -> acc@(new_trans nfa x)) [] dfa_qs in
  let new_delta = delta_clear dfa_delta [] in
  let dfa = {
    sigma = nfa.sigma;
    qs = dfa_qs;
    q0 = e_closure nfa [nfa.q0];
    fs = dfa_fs;
    delta = new_delta;
    } in dfa
