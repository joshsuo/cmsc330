open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
  let rec next str pos = 
    if pos >= String.length str
      then []

    (*(* Tok_LParen *)
    else if Str.string_match (Str.regexp "(") str pos
      then (Tok_LParen)::(next str (pos+1))*)

    (* Tok_RParen *)
    else if Str.string_match (Str.regexp ")") str pos
      then (Tok_RParen)::(next str (pos+1))

    (* Tok_Equal *)
    else if Str.string_match (Str.regexp "=") str pos
      then (Tok_Equal)::(next str (pos+1))

    (* Tok_NotEqual *)
    else if Str.string_match (Str.regexp "<>") str pos
      then (Tok_NotEqual)::(next str (pos+2))

    (* Tok_GreaterEqual *)
    else if Str.string_match (Str.regexp ">=") str pos
      then (Tok_GreaterEqual)::(next str (pos+2))

    (* Tok_LessEqual *)
    else if Str.string_match (Str.regexp "<=") str pos
      then (Tok_LessEqual)::(next str (pos+2))

    (* Tok_Greater *)
    else if Str.string_match (Str.regexp ">") str pos
      then (Tok_Greater)::(next str (pos+1))

    (* Tok_Less *)
    else if Str.string_match (Str.regexp "<") str pos
      then (Tok_Less)::(next str (pos+1))

    (* Tok_Or *)
    else if Str.string_match (Str.regexp "||") str pos
      then (Tok_Or)::(next str (pos+2))

    (* Tok_And *)
    else if Str.string_match (Str.regexp "&&") str pos
      then (Tok_And)::(next str (pos+2))

    (* Tok_Not *)
    else if Str.string_match (Str.regexp "not") str pos
      then (Tok_Not)::(next str (pos+3))

    (* Tok_If *)
    else if Str.string_match (Str.regexp "if") str pos
      then (Tok_If)::(next str (pos+2))

    (* Tok_Then *)
    else if Str.string_match (Str.regexp "then") str pos
      then (Tok_Then)::(next str (pos+4))

    (* Tok_Else *)
    else if Str.string_match (Str.regexp "else") str pos
      then (Tok_Else)::(next str (pos+4))

    (* Tok_Arrow *)
      else if Str.string_match (Str.regexp "->") str pos
        then (Tok_Arrow)::(next str (pos+2))

    (* Tok_Add *)
    else if Str.string_match (Str.regexp "+") str pos
      then (Tok_Add)::(next str (pos+1))

    (* Tok_Sub *)
    else if Str.string_match (Str.regexp "-") str pos
      then (Tok_Sub)::(next str (pos+1))

    (* Tok_Mult *)
    else if Str.string_match (Str.regexp "*") str pos
      then (Tok_Mult)::(next str (pos+1))

    (* Tok_Div *)
    else if Str.string_match (Str.regexp "/") str pos
      then (Tok_Div)::(next str (pos+1))

    (* Tok_Concat *)
    else if Str.string_match (Str.regexp "\\^") str pos
      then (Tok_Concat)::(next str (pos+1))

    (* Tok_Let *)
    else if Str.string_match (Str.regexp "let") str pos
      then (Tok_Let)::(next str (pos+3))

    (* Tok_Def *)
    else if Str.string_match (Str.regexp "def") str pos
      then (Tok_Def)::(next str (pos+3))

    (* Tok_In *)
    else if Str.string_match (Str.regexp "in") str pos
      then (Tok_In)::(next str (pos+2))

    (* Tok_Rec *)
    else if Str.string_match (Str.regexp "rec") str pos
      then (Tok_Rec)::(next str (pos+3))

    (* Tok_Fun *)
    else if Str.string_match (Str.regexp "fun") str pos
      then (Tok_Fun)::(next str (pos+3))

    (* Tok_DoubleSemi *)
    else if Str.string_match (Str.regexp ";;") str pos
      then (Tok_DoubleSemi)::(next str (pos+2))

    (* Tok_Bool *)
    else if Str.string_match (Str.regexp "true\\|false") str pos
      then let s = Str.matched_string str in 
        (Tok_Bool (bool_of_string s))::(next str (pos+(String.length s)))

    (* Tok_Int *)
    else if Str.string_match (Str.regexp "[0-9]+") str pos
      then let i = Str.matched_string str in
        (Tok_Int (int_of_string i))::(next str (pos+(String.length i)))

    (* Tok_Int Neg*)
    else if Str.string_match (Str.regexp "(-[0-9]+)") str pos
      then let i = Str.matched_string str in
      let newInt = String.sub i 1 (String.length i -2) in 
        (Tok_Int (int_of_string newInt))::(next str (pos+(String.length i)))

    (* Tok_LParen *)
    else if Str.string_match (Str.regexp "(") str pos
      then (Tok_LParen)::(next str (pos+1))

    (* Tok_String *)
    else if Str.string_match (Str.regexp "\"[^\"]*\"") str pos
      then let s = Str.matched_string str in
        let newStr = (String.sub s 1 (String.length s -2)) in
        (Tok_String (newStr))::(next str (pos+(String.length s)))

    (* Tok_ID *)
    else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") str pos
      then let s = Str.matched_string str in
        (Tok_ID (s))::(next str (pos+(String.length s)))

    (* white space *)
    else if Str.string_match (Str.regexp "[ \t\n]+") str pos
      then let s = Str.matched_string str in
        (next str (pos+(String.length s)))

    (* ERROR *)
    else 
			raise (InvalidInputException "Invalid Input Exception")

  in next input 0
  