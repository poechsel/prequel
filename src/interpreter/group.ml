open Ast
open AlgebraTypes

(** A grouping operator.

    `sub`: The source feed.
      It should already be sorted by `keys` in ascending order.

    `keys`: The non-aggregate expressions on which to group.
      Those expressions must be pre-compiled to functions using
      `Arithmetics.compile_value`. Two lines will be considered
      part of the same group if they share the same value for
      all those expressions.

    `exports`: The aggregate expressions to export.
      Because we might want to use aggregation functions in the
      SELECT or HAVING clauses, we want to be able to tell the
      grouping operator to compute some of those functions for
      each group, and add as new columns them to the result. *)

class group
  (sub: feed_interface)
  (keys : (feed_result -> Ast.atom) array)
  (exports : (string * (Ast.aggop * header)) array) =

  (** in_same_group : feed_result -> feed_result -> bool
      Returns whether two rows belong to the same group,
      i.e. whether they have identical values on all the
      expressions of `keys`. *)
  let in_same_group (x : feed_result) (y : feed_result) =
    Array.for_all (fun f -> f x = f y) keys in

  let aggregates_empty =
    exports
    |> Array.map snd
    |> Array.map (fun (op, attr) ->
        op, Utils.array_find attr sub#headers, None, None) in

  let aggregates_append x =
    let (~:) i = int_of_string x.(i) in

    Array.map (function
      | Max,   i, None,   _      -> Max,   i, Some (~:i),       None
      | Max,   i, Some m, _      -> Max,   i, Some (max m ~:i), None
      | Min,   i, None,   _      -> Max,   i, Some (~:i),       None
      | Min,   i, Some m, _      -> Max,   i, Some (min m ~:i), None
      | Avg,   i, None,   _      -> Avg,   i, Some (1),         Some (~:i)
      | Avg,   i, Some s, Some n -> Avg,   i, Some (n + 1),     Some (s + ~:i)
      | Count, i, None,   _      -> Count, i, Some (1),         None
      | Count, i, Some n, _      -> Count, i, Some (n + 1),     None
      | _ -> failwith "Error while computing aggregate.") in

  let aggregates_finalize =
    Array.map (function
      | Max,   _, Some m, _ -> string_of_int m
      | Min,   _, Some m, _ -> string_of_int m
      | Avg,   _, Some n, Some s ->
          string_of_float @@ (float_of_int s) /. (float_of_int n)
      | Count, _, Some n, _ -> string_of_int n
      | _ -> failwith "Error while computing aggregate.") in

  object(self)
    inherit feed_interface

    val mutable sub = sub
    val mutable initialized = false
    val mutable current = None
    val mutable ahead = None

    method cached_next =
      current <- ahead;
      ahead <- sub#next;
      current, ahead

    method next =
      if not initialized then begin
        initialized <- true;
        ignore self#cached_next
      end;

      let rec aux agg =
        match self#cached_next with
        | None, _ -> None
        | Some x, Some y when in_same_group x y ->
            let agg' = aggregates_append x agg in
            aux agg'
        | Some x, _ ->
            let agg' = aggregates_append x agg in
            let agg'' = aggregates_finalize agg' in
            Some (Array.append x agg'')

      in aux aggregates_empty

    method reset =
      initialized <- false;
      sub#reset

    method headers =
      (* For each aggregate that appears in a surrounding SELECT
         or HAVING clause, add a new column to the result. *)
      let computed = Array.map (fun (uid, _) -> ("", uid)) exports in
      Array.append sub#headers computed
  end