open AlgebraTypes
open Ast

let attributes_of_condition cond =
  let rec aux c acc =
    match c with
    | AlgBinOp(_, a, b) ->
      aux a acc
      |> aux b
    | AlgAtom (Attribute x) ->
      x :: acc
    | _ ->
      acc
  in aux cond [] 
     |> List.sort_uniq Pervasives.compare 

let rec push_down_select query = 
  query
