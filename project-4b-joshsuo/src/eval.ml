open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(*type values = Int of int|Bool of bool|String of string*)

(* Adds mapping [x:v] to environment [env] *)
let ref_extend env x v = (x, ref v)::env

(*let extend env x v = (x,v)::env*)

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

(*let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x*)

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
(*let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)*)

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with
  |Value value -> value
  |ID id -> getID env e
    (*match e with 
    |ID id -> lookup env id
    |_ -> raise (TypeError("expected ID"))*)
  |Fun(id, exp) -> getFun env e
  |Not not -> getNot env e
  |Binop(_,_,_) -> getBinop env e
  |If(_,_,_) -> getIf env e
  |FunctionCall(exp1, exp2) -> getFunCall env e
  |Let(_,_,_,_) -> getLet env e
  (*|_ -> raise(TypeError("Type error"))*)

  (* ID *)
  and getID env e = 
    (match e with 
    |ID id -> ref_lookup env id
    |_ -> raise (TypeError("expected ID"))
    )

  (* Fun *)
  and getFun env e =
    (match e with
    |Fun(id, exp) -> Closure(env, id, exp)
    |_ -> raise(TypeError("expected Fun"))
    )

  (* Not *)
  and getNot env e = 
    (match e with 
    |Not exp -> 
      (match eval_expr env exp with
      |(Bool true) -> (Bool false)
      |(Bool false) -> (Bool true)
      |_ -> raise(TypeError("expected Bool"))
      )
      (*|Bool bool -> if bool = true then (Bool false) else (Bool true)
      |Not exp2 -> if getNot env exp2 = (Bool true) then (Bool false) else (Bool true)
      |ID id -> 
        match ref_lookup env id with
        |(Bool b) -> if b = true then (Bool false) else (Bool true)
        |_ -> raise(TypeError("expected Bool"))
      |_ -> raise(TypeError("expected ID or Not"))*)
    |_ -> raise(TypeError("expected Not"))
    )

  and getBinop env e = 
    (match e with
    (* Add *)
    |Binop(Add, e1, e2) -> 
      (match eval_expr env e1 with
      |Int i1 -> 
        (match eval_expr env e2 with
        |Int i2 -> Int (i1 + i2)
        |_ -> raise(TypeError("expected Int"))
        )
      |_ -> raise(TypeError("expected Int"))
      )

    (* Sub *)
    |Binop(Sub, e1, e2) -> 
      (match eval_expr env e1 with
      |Int i1 -> 
        (match eval_expr env e2 with
        |Int i2 -> Int (i1 - i2)
        |_ -> raise(TypeError("expected Int"))
        )
      |_ -> raise(TypeError("expected Int"))
      )

    (* Mult *)
    |Binop(Mult, e1, e2) -> 
      (match eval_expr env e1 with
      |Int i1 -> 
        (match eval_expr env e2 with
        |Int i2 -> Int (i1 * i2)
        |_ -> raise(TypeError("expected Int"))
        )
      |_ -> raise(TypeError("expected Int")) 
      )

    (* Div *)
    |Binop(Div, e1, e2) -> 
      (match eval_expr env e1 with
      |Int i1 -> 
        (match eval_expr env e2 with
        |Int i2 -> if i2 = 0 then raise(DivByZeroError) else Int (i1 / i2)
        |_ -> raise(TypeError("expected Int"))
        )
      |_ -> raise(TypeError("expected Int"))
      )

    (* Greater *)
    |Binop(Greater, e1, e2) -> 
      (match eval_expr env e1 with
      |Int i1 -> 
        (match eval_expr env e2 with
        |Int i2 -> if i1 > i2 then (Bool true) else (Bool false)
        |_ -> raise(TypeError("expected Int"))
        )
      |_ -> raise(TypeError("expected Int"))
      )

    (* Less *)
    |Binop(Less, e1, e2) -> 
      (match eval_expr env e1 with
      |Int i1 -> 
        (match eval_expr env e2 with
        |Int i2 -> if i1 < i2 then (Bool true) else (Bool false)
        |_ -> raise(TypeError("expected Int"))
        )
      |_ -> raise(TypeError("expected Int"))
      )

    (* GreaterEqual *)
    |Binop(GreaterEqual, e1, e2) -> 
      (match eval_expr env e1 with
      |Int i1 -> 
        (match eval_expr env e2 with
        |Int i2 -> if (i1 > i2 || i1 = i2) then (Bool true) else (Bool false)
        |_ -> raise(TypeError("expected Int"))
        )
      |_ -> raise(TypeError("expected Int"))
      )

    (* LessEqual *)
    |Binop(LessEqual, e1, e2) -> 
      (match eval_expr env e1 with
      |Int i1 -> 
        (match eval_expr env e2 with
        |Int i2 -> if (i1 < i2 || i1 = i2) then (Bool true) else (Bool false)
        |_ -> raise(TypeError("expected Int"))
        )
      |_ -> raise(TypeError("expected Int"))
      )

    (* Concat *)
    |Binop(Concat, e1, e2) -> 
      (match eval_expr env e1 with
      |String s1 -> 
        (match eval_expr env e2 with
        |String s2 -> String (s1 ^ s2)
        |_ -> raise(TypeError("expected String"))
        )
      |_ -> raise(TypeError("expected String"))
      )

    (* Equal *)
    |Binop(Equal, e1, e2) -> 
      let exp2 = eval_expr env e2 in
      (match eval_expr env e1 with
      |Int i1 -> 
        (match exp2 with
        |Int i2 -> if i1 = i2 then (Bool true) else (Bool false)
        |_ -> raise(TypeError("cannot compare int to other types"))
        )
      |Bool b1 ->
        (match exp2 with
        |Bool b2 -> if b1 = b2 then (Bool true) else (Bool false)
        |_ -> raise(TypeError("cannot compare bool to other types"))
        )
      |String s1 ->
        (match exp2 with
        |String s2 -> if s1 = s2 then (Bool true) else (Bool false)
        |_ -> raise(TypeError("cannot compare string to other types"))
        )
      |Closure(_,_,_) -> raise(TypeError("expected value, not closure"))
      )

    (* NotEqual *)
    |Binop(NotEqual, e1, e2) -> 
      let exp2 = eval_expr env e2 in
      (match eval_expr env e1 with
      |Int i1 -> 
        (match exp2 with
        |Int i2 -> if i1 <> i2 then (Bool true) else (Bool false)
        |_ -> raise(TypeError("cannot compare int to other types"))
        )
      |Bool b1 ->
        (match exp2 with
        |Bool b2 -> if b1 <> b2 then (Bool true) else (Bool false)
        |_ -> raise(TypeError("cannot compare bool to other types"))
        )
      |String s1 ->
        (match exp2 with
        |String s2 -> if s1 <> s2 then (Bool true) else (Bool false)
        |_ -> raise(TypeError("cannot compare string to other types"))
        )
      |Closure(_,_,_) -> raise(TypeError("expected value, not closure"))
      )

    (* Or *)
    |Binop(Or, e1, e2) ->
      (match eval_expr env e1 with
      |Bool b1 ->
        (match eval_expr env e2 with
        |Bool b2 -> if (b1 = true || b2 = true) then (Bool true) else (Bool false)
        |_ -> raise(TypeError("expected Bool"))
        )
      |_ -> raise(TypeError("expected Bool"))
      )

    (* And *)
    |Binop(And, e1, e2) ->
      (match eval_expr env e1 with
      |Bool b1 ->
        (match eval_expr env e2 with
        |Bool b2 -> if (b1 = true && b2 = true) then (Bool true) else (Bool false)
        |_ -> raise(TypeError("expected Bool"))
        )
      |_ -> raise(TypeError("expected Bool"))
      )
    |_ -> raise(TypeError("expected Binop"))
    )

  (* If *)
  and getIf env e = 
    (match e with
    |If(e1, e2, e3) -> 
      let guard = eval_expr env e1 in
      (match guard with
      |(Bool true) -> eval_expr env e2
      |(Bool false) -> eval_expr env e3
      |_ -> raise(TypeError("expected Bool"))
      )
    |_ -> raise(TypeError("expected If"))
    )

  (* FunctionCall *)
  and getFunCall env e = 
    (match e with
    |FunctionCall(e1, e2) -> 
      (match eval_expr env e1 with
      |Closure(env2, id, exp) -> 
        let value = eval_expr env e2 in
        eval_expr (ref_extend env2 id value) exp
      |_ -> raise(TypeError("expected Closure"))
      )
    |_ -> raise(TypeError("expected FunctionCall"))
    )

  (* Let *)
  and getLet env e = 
    (match e with
    |Let(name, isRec, init, body) ->
      (match isRec with
      |false -> 
        let value = eval_expr env init in
        eval_expr (ref_extend env name value) body
      |true -> 
        let newEnv = (ref_extend_tmp env name) in
        let value = eval_expr newEnv init in
        ref_update newEnv name value;
        eval_expr newEnv body
      )
    |_ -> raise(TypeError("expected Let"))
    )




(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)

let eval_mutop env m = 
  match m with
  |Def(name, e) -> 
    let newEnv = ref_extend_tmp env name in
    let value = eval_expr newEnv e in
    ref_update newEnv name value;
    (newEnv, Some value)
  |Expr exp -> 
    let evaluate = eval_expr env exp in 
    (env, Some evaluate)
  |NoOp -> (env, None)
