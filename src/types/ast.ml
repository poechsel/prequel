type attribute = string option * string
type attribute_renamed = attribute * string option


type binop = 
  | And
  | Or

type compop =
  | Leq
  | Eq
  | Neq
  | Geq
  | Lt
  | Gt

type 'a relation =
  | AstSubQuery of 'a query
  | AstTable of string

and 'a query =
  | AstSelect of attribute_renamed list * 'a relation list * 'a * 'a option
  | AstMinus of 'a query * 'a query
  | AstUnion of 'a query * 'a query

type cond =
  | AstBinOp of binop * cond * cond
  | AstCompOp of compop * attribute * attribute
  | AstIn of attribute * cond query
  | AstNotIn of attribute * cond query

type disj = 
  | DisjCompOp of compop * attribute * attribute
  | DisjIn of attribute * (disj list list) query
  | DisjNotIn of attribute * (disj list list) query




(* only for debug, dead code *)
let print_disj_form query =
  let rec print_query query = 
    match query with
    | AstSelect(_, _, cond, _) ->
      print_cond cond
    | _ -> ()
  and print_cond cond = 
    print_string @@ List.fold_left (fun a b ->
        (match a with "" -> "" | _ -> a ^ " \\/ ") ^
        (List.fold_left (fun x y ->
          "(" ^ (match x with "" -> "" | _ -> x ^ " /\\ ") ^ print_attr y ^ ")"
          ) "" b)
      ) "" cond
  and print_attr a =
    match a with
    | DisjCompOp(_, (_, a), _) -> a
    | _ -> ""
    
  in 
  print_query query
