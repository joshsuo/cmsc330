type person = { name: string;
                age: int;
                hobbies: string list }

(* Define the type of db below *)
type db = person list

let newDatabase : db = []

let insert (person:person) (db:db) :db = db @ [person]
(* let newDB = db.people @ [person] - adds to back *)

let rec remove (name:string) (db:db) :db = 
  let peopleList = match db with
  [] -> []
  |h::t -> if h.name = name then remove name t else h::(remove name t)
  in peopleList

type comparator = person -> person -> int

let rec sort (comparator) (db:db) : person list = 
  let sorter = match db with
  |h1::h2::t ->
    if comparator h1 h2 <> -1 then 
      h2::(sort comparator (h1::t))
    else
      h1::(sort comparator (h2::t))
  |t -> t
    in
      if db = sorter then
        db @ []
      else
        sort comparator sorter

type condition =
  | True
  | False
  | Age of (int -> bool)
  | Name of (string -> bool)
  | Hobbies of (string list -> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition

let rec query condition (db:db) : person list = 
  match db with
  [] -> []
  |h::t -> match condition with
    True -> db
    |False -> []
    |Age f -> if f h.age = true then h::(query condition t) else query condition t
    |Name f -> if f h.name = true then h::(query condition t) else query condition t
    |Hobbies f -> if f h.hobbies = true then h::(query condition t) else query condition t
    |And(c1,c2) -> if (query c1 [h]<>[] && query c2 [h]<>[]) then h::(query condition t) else query condition t
    |Or(c1,c2) -> if (query c1 [h]<>[] || query c2 [h]<>[]) then h::(query condition t) else query condition t
    |Not (c1) -> if query c1 [h]<>[] then (query condition t) else h::query condition t
    |If (c1,c2,c3) -> if query c1 [h] <> [] then
        if query c2 [h]<>[] then h::(query condition t) else query condition t
      else
        if query c3 [h]<>[] then h::(query condition t) else query condition t

let queryBy condition db comparator = 
  let (conditioned:person list) :db = query condition db in
  sort comparator conditioned

let update condition (db:db) personData :db = 
  let (conditioned:person list) = query condition db in
  let updated = List.map personData conditioned in 
  let (not_conditioned:person list) = (query (Not(condition)) db) in
  not_conditioned @ updated

let deleteAll condition db :db = 
  query (Not(condition)) db
