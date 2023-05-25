type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf

(* Part 2 *)

let rec tree_fold f init tree =
  match tree with
  Leaf -> init
  |Node(left, x, right) ->
    f (tree_fold f init left) x (tree_fold f init right)

let map tree f = 
  tree_fold (fun left x right -> Node(left, f x, right)) Leaf tree

let mirror tree = 
  tree_fold (fun left x right -> Node(right, x, left)) Leaf tree

let in_order tree = 
  tree_fold (fun left x right -> left @ x:: right) [] tree

let pre_order tree = 
  tree_fold (fun left x right -> x:: left @ right) [] tree

let compose tree =
  tree_fold (fun left x right -> fun value -> (right (x (left value))) ) (fun init -> init) tree;;

let depth tree = 
  tree_fold (fun left x right -> 1 + (max left right)) 0 tree

(* Assume complete tree *)
let trim tree n = 
  if depth tree <= n then tree 
  else
    let newTree = Leaf in
    let fullDepth = depth tree in
    let func1 = (fun left v right -> if (fullDepth - (depth left) <= n)
      then Node(left, (v,1), right) else Node(left, (v,0), right)) in
    let func2 = (fun left x right -> match x with
      (v,t) -> if t = 1 then Node(left, v, right) else Leaf) in 
    tree_fold func2 newTree (tree_fold func1 newTree tree)

(* Part 3 *)

(* optional *)
let split lst v = 
  let rec split_aux lst v lst1 lst2 state =
    match lst with
    | [] -> (lst1 , lst2)
    | h::t -> 
        if state = 0 then
          if h=v then 
            split_aux t v lst1 lst2 1
          else
            split_aux t v (lst1@[h]) lst2 0
        else
          split_aux t v lst1 (lst2@[h]) 1
    in split_aux lst v [] [] 0

let rec from_pre_in pre in_ord = 
  match pre with
  | [] -> Leaf
  | h::t -> 
      let split_n lst n =    
        let rec split_aux lst n lst1 lst2 count =
          match lst with
          | [] -> (lst1 , lst2)
          | h::t -> if count > n then split_aux t n lst1 (lst2@[h]) (count+1)
              else split_aux t n (lst1@[h]) lst2 (count+1)
          in split_aux lst n [] [] 1 in
      let (in_ord_left, in_ord_right) = (split in_ord h) in
      let (pre_left, pre_right) = split_n t (List.length in_ord_left) in
        Node(from_pre_in pre_left in_ord_left, h, from_pre_in pre_right in_ord_right)
