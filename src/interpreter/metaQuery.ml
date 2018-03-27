open AlgebraTypes

let get_uid_from_alg a = 
  match a with
  | AlgInput(u, _) 
  | AlgUnion(u, _, _)
  | AlgMinus(u, _, _)
  | AlgProjection(u, _, _)
  | AlgProduct(u, _, _)
  | AlgJoin(u, _, _)
  | AlgSelect(u, _, _)
  | AlgAddColumn(u, _, _, _)
  | AlgRename(u, _, _)
  | AlgOrder(u, _, _) ->
    u

let rec get_subtree_from_uid uid tree = 
  (* get the subtrees having a specified uid *)
  if get_uid_from_alg tree = uid then
    Some tree
  else 
    match tree with
  | AlgUnion(_, a, b) 
  | AlgProduct(_, a, b) 
  | AlgJoin(_, (a, _), (b, _))
  | AlgMinus(_, a, b) ->
    begin match (get_subtree_from_uid uid a) with
      | None -> get_subtree_from_uid uid b
      | Some x -> Some x
    end 
  | AlgAddColumn(_, a, _, _) 
  | AlgOrder(_, a, _) 
  | AlgProjection(_, a, _) 
  | AlgRename(_, a, _)
  | AlgSelect(_, a, _) ->
    get_subtree_from_uid uid a
  | AlgInput _ ->
    None



let rec get_headers ?(f=(fun _ _ -> ())) query =
  let res = match query with
    | AlgInput(_, str) ->
      InputCachedFile.get_headers str
    | AlgUnion(_, a, b) ->
      Union.get_headers (get_headers ~f:f a) (get_headers ~f:f b)
    | AlgMinus(_, a, b) ->
      Minus.get_headers (get_headers ~f:f a) (get_headers ~f:f b)
    | AlgJoin(_, (a, _), (b, _)) ->
      Join.get_headers (get_headers ~f:f a) (get_headers ~f:f b)
    | AlgProjection(_, a, headers) ->
      let _ = get_headers ~f:f a in
      headers
    | AlgSelect(_, a, filter) ->
      Select.get_headers (get_headers ~f:f a)
    | AlgProduct(_, a, b) ->
      Product.get_headers (get_headers ~f:f a) (get_headers ~f:f b)
    | AlgRename(_, a, b) ->
      let h = get_headers ~f:f a in
      let tbl = Rename.build_rename_map b h in
      Rename.get_headers tbl h
    | AlgAddColumn(_, a, _, n) ->
      AddColumn.get_headers (get_headers ~f:f a) n
    | AlgOrder(_, a, criterion) ->
      Select.get_headers (get_headers ~f:f a)
  in 
  let _ = f (get_uid_from_alg query) res in
  res


(* caching isn't trivial to implement for us.
   Before compilations, we renamed all columns name to uid
   in order to avoid having several columns with the same name.

   Therefore, we can't have two identical subtrees: table names will
   not be the sames.

   Thus, we must redefine a new equality.
   We say that two trees A and B are equals, if 
   by renamming the tables of B we can manage to become A.

   To implement this we uses a custom made environnmenet
   *)

module EnvRename = struct
  module Env = Map.Make(struct
      type t = string
      let compare = Pervasives.compare 
    end )
    type t = string Env.t
    let union env a b =
      if Env.mem a env then
        env, Env.find a env = b
      else begin
        Env.add a b env, true
      end 

    let merge env env' = 
      try
        let f = fun k a b -> if a <> b then raise Not_found else Some a in
        (Env.union f env env'), true
      with _ ->
        env, false
end 

let rec equal_expr env a b =
  match a, b with
  | AlgAtom(Ast.Attribute (a, b)), AlgAtom(Ast.Attribute (a', b')) ->
    let e, s = EnvRename.union env a a' in
    e, s && b = b'
  | AlgBinOp(op, a, b), AlgBinOp(op', a', b') ->
    let e, s = equal_expr env a a' in
    let e', s' = equal_expr env b b' in
    let e, o = EnvRename.merge e e' in
    e,
    o && s && s' && op = op'
  | x, x' ->
    env, Pervasives.compare x x' = 0



let rec equal_algebra env a b = 
  let _ = Printf.printf "%s <-> %s\n" (show_algebra a) (show_algebra b) in
  let e, s = match a, b with
  | AlgProduct(_, a, b), AlgProduct(_, a', b') 
  | AlgMinus(_, a, b), AlgMinus(_, a', b') 
  | AlgUnion(_, a, b), AlgUnion(_, a', b')  ->
    let e, s = equal_algebra env a a' in
    let e', s' = equal_algebra env a a' in
    let e, s'' = EnvRename.merge e e' in
    e,
    s && s' && s''
  | AlgAddColumn(_, a, b, c), AlgAddColumn(_, a', b', c') ->
    let e, s = equal_algebra env a a' in
    let e', s' = equal_expr env b b' in
    let e'', s'' = equal_expr env b b' in
    let e, o = EnvRename.merge e e' in
    let e, o' = EnvRename.merge e e'' in
    e, s && s' && s'' && o && o'
  | AlgOrder(_, a, b), AlgOrder(_, a', b') ->
    if Array.length b <> Array.length b' then
      env, false
    else
      let e, s = 
        List.combine (List.map fst (Array.to_list b)) (List.map fst (Array.to_list b'))
        |> List.fold_left (fun (e, s) (a, a') -> 
            let e', s' = equal_expr env a a' in
            let e, o = EnvRename.merge e e' in
            e, s' && o
          ) (env, true)
      in 
      let e', s' = equal_algebra env a a' in
      let e, o = EnvRename.merge e e' in
      e,
      o && s && s'
  | AlgProjection(_, a, b), AlgProjection(_, a', b') ->
    if Array.length b <> Array.length b' then
      env, false
    else
      let e, s  = 
        List.combine (Array.to_list b) (Array.to_list b')
        |> List.fold_left (fun (e, a) ((b, x), (b', x')) ->
            let e, s' = EnvRename.union e b b'  in
            e,
            a && s' && x = x'
          ) (env, true)
      in let e', s' = equal_algebra env a a'
      in let e, o = EnvRename.merge e e'
      in e, o && s && s'
  | AlgRename(_, a, b), AlgRename(_, a', b') ->
    if List.length b <> List.length b' then
      env, false
    else
      let e, s = 
        List.combine b b' 
        |> List.fold_left (fun (e, s) (((a, x), (na, nx)), ((a', x'), (na', nx'))) ->
            let e', s' = EnvRename.union e a a' in
            let e'', s'' = EnvRename.union e na na' in
            let e, s = EnvRename.merge e e'' in
            e, s && s'' && s' && (nx = nx') && (x = x')
          ) (env, true)
      in let e', s' = equal_algebra env a a'
      in let e, o = EnvRename.merge e e' 
      in e, o && s && s'
  | AlgSelect(_, a, b), AlgSelect(_, a', b') ->
    let e', s' = equal_algebra env a a' in
    let e'', s'' = equal_expr env b b' in
    let e, o = EnvRename.merge e' e'' in
    e,
    o && s' && s''
  | AlgJoin(_, (a, ea), (b, eb)), AlgJoin(_, (a', ea'), (b', eb')) ->
    let e, s = equal_algebra env a a' in
    let e', s' = equal_algebra env b b' in
    let ee, ss = equal_expr env ea ea' in
    let ee', ss' = equal_expr env eb eb' in
    let e, o = EnvRename.merge e e' in
    let ee, oo = EnvRename.merge ee ee' in
    let e, o' = EnvRename.merge e ee in
    e,
    o && o' && oo && s' && s && ss && ss'
  | AlgInput(_, n), AlgInput(_, n') ->
    env, n = n'
  | _, _ ->
    env, false

  in 
    let _ = if s then print_endline "equal" in
  e, s



let rec normalize_expr expr = 
  match expr with
  | AlgAtom(Ast.Attribute((_, b))) ->
    AlgAtom(Ast.Attribute(("", b)))
  | AlgBinOp(op, a, b) ->
    AlgBinOp(op, normalize_expr a, normalize_expr b)
  | expr -> expr 

let rec normalize alg = 
  match alg with
  | AlgInput(u, str) ->
    AlgInput(0, str)
  | AlgUnion(_, a, b) ->
    AlgUnion(0, normalize a, normalize b)
  | AlgProduct(_, a, b) ->
    AlgProduct(0, normalize a, normalize b)
  | AlgMinus(_, a, b) ->
    AlgMinus(0, normalize a, normalize b)
  | AlgJoin(_, (a, ea), (b, eb)) ->
    AlgJoin(0, (normalize a, normalize_expr ea), (normalize b, normalize_expr eb))
  | AlgProjection(_, a, headers) ->
    AlgProjection(0, normalize a, Array.map (fun (a, b) -> "", b) headers)
  | AlgSelect(_, a, filter) ->
    AlgSelect(0, normalize a, normalize_expr filter)
  | AlgRename(_, a, b) ->
    AlgRename(0, normalize a, List.map (fun ((_, b), (_, d)) -> ("", b), ("", d)) b)
  | AlgAddColumn(_, a, b, n) ->
    AlgAddColumn(0, normalize a, normalize_expr b, n)
  | AlgOrder(_, a, criterion) ->
    AlgOrder(0, normalize a, Array.map (fun (a, o) -> normalize_expr a, o) criterion)

module AlgHashtbl = Hashtbl.Make(struct
    type t = algebra
    let equal i j = let _ = print_endline "==================" in snd (equal_algebra (EnvRename.Env.empty) i j)
    let hash i = normalize i |> Hashtbl.hash 
  end)





let rec initialize_caching tbl alg = 
  let _ = 
    if AlgHashtbl.mem tbl alg then
      let _ = print_endline "> found" in
      let base_uid, path = AlgHashtbl.find tbl alg in
      if path = "" then
         AlgHashtbl.add tbl alg (get_uid_from_alg alg, TempManager.new_temp ())
      else ()
    else
      AlgHashtbl.add tbl alg (get_uid_from_alg alg, "")
  in 
  let aux = initialize_caching tbl in
  match alg with
  | AlgUnion(_, a, b) 
  | AlgProduct(_, a, b) 
  | AlgJoin(_, (a, _), (b, _))
  | AlgMinus(_, a, b) ->
    aux a; aux b
  | AlgAddColumn(_, a, _, _) 
  | AlgOrder(_, a, _) 
  | AlgProjection(_, a, _) 
  | AlgRename(_, a, _)
  | AlgSelect(_, a, _) ->
    aux a
  | AlgInput _ ->
    ()



let feed_from_query (query : algebra) : feed_interface = 
  let alg_full = query in
  let tbl = AlgHashtbl.create 10 in
  let () = initialize_caching tbl query in 
  let rec feed_from_query query =
    (* convert a query to a feed *)
    let aux query = 
    match query with
    | AlgInput(_, str)   -> 
      new InputCachedFile.inputCachedFile str
    | AlgUnion(_, a, b) ->
      new Union.union (feed_from_query a) (feed_from_query b)
    | AlgMinus(_, a, b) ->
      new Minus.minus (feed_from_query a) (feed_from_query b)
    | AlgProjection(_, a, headers) ->
      new Projection.projection (feed_from_query a) headers
    | AlgSelect(_, a, filter) ->
      let sub = feed_from_query a in
      let filter = Arithmetics.compile_filter (get_headers a) filter in
      new Select.select sub filter
    | AlgJoin(_, (a, expr_a), (b, expr_b)) ->
      let sub_a = feed_from_query a in
      let eval_a = Arithmetics.compile_value (get_headers a) expr_a in
      let sub_b = feed_from_query b in
      let eval_b = Arithmetics.compile_value (get_headers b) expr_b in
      (* WE MUST SORT the right hand side *)
    (*
    let sub_b = new ExternalSort.sort sub_b [|expr_b|] in
    new Join.joinSorted (sub_a, eval_a) (sub_b, eval_b)
       *)
      (* fastest for small tables *)
      new Join.joinHash (sub_a, eval_a) (sub_b, eval_b)
    | AlgAddColumn(_, a, expr, n) ->
      new AddColumn.addColumn (feed_from_query a) (Arithmetics.compile_value (get_headers a) expr) n
    | AlgProduct(_, a, b) ->
      new Product.product (feed_from_query a) (feed_from_query b)
    | AlgRename(_, a, b) ->
      new Rename.rename (feed_from_query a) (b)
    | AlgOrder(_, a, criterion) ->
      let headers = get_headers a in
      let sub = feed_from_query a in
      let compiled = Array.map
          (fun (v, ord) -> (Arithmetics.compile_value headers v, ord))
          criterion in
      new ExternalSort.sort sub compiled

    in

    if AlgHashtbl.mem tbl query then
      let cache_uid, path = AlgHashtbl.find tbl query in
      if path <> "" then
        if cache_uid = get_uid_from_alg query then
          let c = aux query in
          new Materialize.materialize c path
        else
          let headers_cache = 
            match (get_subtree_from_uid cache_uid alg_full) with
            | None -> failwith "error"
            | Some x -> get_headers x
          in
          let headers = get_headers query in
          (* bug when the sort query has headers having attributes
             occuring several times *)
          let rename = Array.map2 (fun source n -> ("", snd source), n) headers_cache headers in
          let _ = print_endline "duplicate :D" in
          new Rename.rename (new InputCachedFile.inputCachedFile path) (rename |> Array.to_list)
      else 
        aux query
    else 
      aux query
  in feed_from_query query
