open Funs

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (a, b, c) = 
  (c,b,a)

let is_even x = 
  (x mod 2 = 0)

let area (a, b) (x, y) = 
  (abs (a-x))*(abs (b-y))

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = match n with
0 -> 0
|1 -> 1
|n -> fibonacci(n-1) + fibonacci(n-2)

let rec pow x p = 
  if p < 1 then 1 else x * (pow x (p-1))

let rec is_prime x = 
  if x <= 1 then false
  else
    let rec aux x next = 
      next <= 1 || ((x mod next) <> 0) && aux x (next-1)
    in aux x (x-1)

let rec maxFuncChain init funcs = 
  let rec chain curr tester total max funcs = match funcs with
  [] -> max
  |h::t -> 
    let newTotal =  (h total)
  in let newCurr = (h init)
  in let newTest = (h tester)
  in let newMax =
    if newTest > max || newCurr > max then
      if newTest >= newCurr then newTest else newCurr
    else max
  in let newerMax =
    if newTotal > newMax then newTotal else newMax
  in chain init newerMax newTotal newerMax t
in chain init init init init funcs

(*****************)
(* Part 3: Lists *)
(*****************)

let reverse lst = 
  let rec rev acc lst = match lst with
  [] -> acc
  |h::t -> rev(h::acc) t
    in rev [] lst

let rec merge lst1 lst2 = 
  match lst1, lst2 with 
  [],_ -> lst2
  |_,[] -> lst1
  |h1::t1, h2::t2 -> if h1 < h2 then h1::merge t1 lst2 else h2::merge lst1 t2

let rec is_palindrome lst = 
  let reversed = reverse lst in
  if reversed = lst then true else false

let jumping_tuples lst1 lst2 = 
  let rec jump acc index lst1 lst2 = match lst1, lst2 with
  [],[] -> []
  |[], (_,_)::t2 -> acc
  |(_,_)::t1,[] -> acc
  |(h1,_)::t1, (_,h2)::t2 -> if index mod 2 = 0 then h2::(jump acc (index+1) t1 t2) else h1::(jump acc (index+1) t1 t2)
  in jump [] 0 lst1 lst2

let rec flatten lst = 
  match lst with
  [] -> []
  |h::t -> h @ flatten t
  
let rec square_primes lst = 
  match lst with
  [] -> []
  |h::t -> if is_prime h = true then (h, h*h)::(square_primes t) else square_primes t

let rec partition p lst = 
  let check elements (isP, notP) = 
    if p elements then (elements::isP, notP)
    else (isP, elements::notP)
  in fold_right check lst([],[])

(*****************)
(* Part 4: HOF *)
(*****************)

let is_present lst x = 
  let checked y = if (x = y) then 1 else 0 in
  map checked lst 

let count_occ lst target = 
  let a = is_present lst target in
  let add x y = x + y in
  fold add 0 a

let uniq lst = 
  let comb x acc =
    let func a b = a || x = b in
    if fold func false acc then acc else x::acc in
  fold_right comb lst []

