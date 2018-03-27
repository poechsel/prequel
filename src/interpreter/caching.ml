open AlgebraTypes


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
  (* environnmenet used to lazilly check if two
     subtrees represents the same subtree (ie their tables 
     looks like the same *)
  module Env = Map.Make(struct
      type t = string
      let compare = Pervasives.compare 
    end )
    type t = string Env.t

    (* unify to tables uid
        Returns the new env and True if the uids can be unified without
        breaking coherence,
        False otherwize
    *)
    let union env a b =
      if Env.mem a env then
        env, Env.find a env = b
      else begin
        Env.add a b env, true
      end 

    (* merge two env
        Returns the new env and true if we can merge the two env 
       without breaking coherence.
       False otherwise
    *)
    let merge env env' = 
      try
        let f = fun k a b -> if a <> b then raise Not_found else Some a in
        (Env.union f env env'), true
      with _ ->
        env, false
end 


let rec equal_expr env a b =
  (* check if two exprs are equals 
    Returns the current env and true if a '=' b, false otherwize
  *)
  match a, b with
  | AlgAtom(Ast.Attribute (a, b)), AlgAtom(Ast.Attribute (a', b')) ->
    (* here we just want to unify a and a'*)
    let e, s = EnvRename.union env a a' in
    e, s && b = b'
  | AlgBinOp(op, a, b), AlgBinOp(op', a', b') ->
    (* we proceed recursively*)
    let e, s = equal_expr env a a' in
    let e', s' = equal_expr env b b' in
    let e, o = EnvRename.merge e e' in
    e,
    o && s && s' && op = op'
  | x, x' ->
    env, Pervasives.compare x x' = 0



let rec equal_algebra env a b = 
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
  e, s


(* We also need a hash function to build a hashtbl.
   Unfortunately, the buildin hashfunction hashes on the
   whole structure. Do to the uids we do not want it.

   In order to build a hash function for our algebras / trees, 
   we first 'normalize them' (replace uids by a constant value).
   This will probably give us false positive, but it is not a major 
   issue
   *)

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



(* our caching hashtbl *)
module AlgHashtbl = Hashtbl.Make(struct
    type t = algebra
    let equal i j = snd (equal_algebra (EnvRename.Env.empty) i j)
    let hash i = normalize i |> Hashtbl.hash 
  end)





let create alg = 
  (* create the cache structure*)
  let tbl = AlgHashtbl.create 10 in
  let rec main alg = 
    let _ = 
      if AlgHashtbl.mem tbl alg then
        let base_uid, path = AlgHashtbl.find tbl alg in
        if path = "" then
          (* we store tuples in the hashtbl of the form (uid of the 
             tree which is the representant of this type of tree, 
             name of the temp file
             *)
          AlgHashtbl.add tbl alg (base_uid, TempManager.new_temp ())
        else ()
      else
        AlgHashtbl.add tbl alg (get_uid_from_alg alg, "")
    in 
    match alg with
    | AlgUnion(_, a, b) 
    | AlgProduct(_, a, b) 
    | AlgJoin(_, (a, _), (b, _))
    | AlgMinus(_, a, b) ->
      let _ = main a in
      main b
    | AlgAddColumn(_, a, _, _) 
    | AlgOrder(_, a, _) 
    | AlgProjection(_, a, _) 
    | AlgRename(_, a, _)
    | AlgSelect(_, a, _) ->
      main a
    | AlgInput _ ->
      ()
  in main alg; tbl



type caching_status =
  | No_cache
  | Materialized of string
  | UnMaterialized of string 

let use_cache tbl query = 
  (* give us the caching status of `query` for a cache `tbl`
     Returns No_cache if this tree is not to be cached.
     Materialized if its result needs to be saved.
     UnMaterialized if the results of the query have been cached 
     *)
    if AlgHashtbl.mem tbl query then
      let cache_uid, path = AlgHashtbl.find tbl query in
      if path <> "" then
        if cache_uid = get_uid_from_alg query then
          Materialized path
        else
          UnMaterialized path
      else 
        No_cache
    else 
        No_cache
