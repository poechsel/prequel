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


let list_sym_diff l1 l2 = 
  let rec aux l acc = 
    match l with
    | [] -> acc
    | x::y::tl when x = y -> aux tl acc
    | x::tl -> aux tl (x::acc)
  in aux (List.sort Pervasives.compare @@ l1 @ l2) []

let is_sublist l1 l2 =
  let _ = Printf.printf "%s sublist %s\n" 
    (String.concat "," (List.map (fun x -> x |> Debug.string_of_header) l1))
    (String.concat "," (List.map (fun x -> x |> Debug.string_of_header) l2))
    in
    (* return true if l1 is a sublist of l2 *)
  let rec aux l1 l2 = 
    match l1, l2 with 
    | [], _ -> true
    | _, [] -> false
    | x1::t1, x2::t2 when x1 = x2 ->
        aux t1 t2
    | t1, x2::t2 ->
      aux t1 t2
  in aux (List.sort Pervasives.compare l1) (List.sort Pervasives.compare l2)

let push_down_select query = 
  let tbl = Hashtbl.create 10 in
  let _ = MetaQuery.get_headers ~f:(fun uid header -> Hashtbl.add tbl uid (Array.to_list header)) query in
  let rec push_down can_be_pushed query =
    (*TODO optimize
      let header = Hashtbl.find tbl (MetaQuery.get_uid_from_alg query) in*)
    let header = MetaQuery.get_headers query |> Array.to_list in
    let _ = 
      Printf.printf "(%d) %s\n"
        (MetaQuery.get_uid_from_alg query)
        (String.concat " " (List.map Debug.string_of_header header)) in
    let push_further_down, insert_before = 
      can_be_pushed |> 
      List.partition (fun (cond, attrs) ->
         is_sublist attrs header
        )
    in 
    let to_insert, req = 
      match query with
      | AlgUnion(u, a, b) ->
        let i1, a' = push_down push_further_down a in
        let i2, b' = push_down push_further_down b in
        list_sym_diff i1 i2, AlgUnion(u, a', b')

      | AlgMinus(u, a, b) ->
        let i1, a' = push_down push_further_down a in
        let i2, b' = push_down push_further_down b in
        list_sym_diff i1 i2, AlgMinus(u, a', b')
      
      | AlgProduct(u, a, b) ->
        let i1, a' = push_down push_further_down a in
        let i2, b' = push_down push_further_down b in
        list_sym_diff i1 i2, AlgProduct(u, a', b')
       
      | AlgProjection(u, a, headers) ->
        let i, a' = push_down push_further_down a in
        i, AlgProjection(u, a', headers)

      | AlgRenameTable(u, a, name) ->
        let i, a' = push_down push_further_down a in
        i, AlgRenameTable(u, a', name)

      | AlgSelect(u, a, filter) ->
        let attrs = attributes_of_condition filter in
        let i, a' = push_down ((filter, attrs) :: push_further_down) a in
        i, a'

      | AlgInput(u, str) ->
        can_be_pushed, query

    in insert_before, 
       List.fold_left (fun a (cond, _) -> AlgSelect(AlgebraTypes.new_uid (), a, cond))
         req
         to_insert
  in snd @@ push_down [] query
