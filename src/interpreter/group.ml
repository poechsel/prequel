open AlgebraTypes

(** A grouping operator.

    `sub`: The source feed.
      It should already be sorted by `keys` in ascending order.

    `keys`: The non-aggregated expressions on which to group.
      Those expressions must be pre-compiled to functions using
      `Arithmetics.compile_value`. Two lines will be considered
      part of the same group if they share the same value for
      all those expressions.

    `exports`: The aggregated expressions to export.
      Because we might want to use aggregation functions in the
      SELECT or HAVING clauses, we want to be able to tell the
      grouping operator to compute some of those functions for
      each group, and add as new columns them to the result. *)

class group
  (sub: feed_interface)
  (keys : (feed_result -> Ast.atom) array)
  (exports : (string * aggregate_expression) array) =

  (** in_same_group : feed_result -> feed_result -> bool
      Returns whether two rows belong to the same group,
      i.e. whether they have identical values on all the
      expressions of `keys`. *)
  let in_same_group (x : feed_result) (y : feed_result) =
    Array.for_all (fun f -> f x = f y) keys in

  (** compute_exports : (string array) list -> string array
      Computes the value of all the aggregated expressions
      (e.g. MIN(foo.bar)) in `exports` for a given group. *)
  let compute_exports group =
    [| |] (* TODO *)
  in

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

      let rec aux group =
        match self#cached_next with
        | None, _ -> None
        | Some x, Some y when in_same_group x y ->
            aux @@ x :: group
        | Some x, _ ->
            Some (Array.append x @@ compute_exports (x :: group))

      in aux []

    method reset =
      initialized <- false;
      sub#reset

    method headers =
      (* For each aggregate that appears in a surrounding SELECT
         or HAVING clause, add a new column to the result. *)
      let computed = Array.map (fun (uid, _) -> ("", uid)) exports in
      Array.append sub#headers computed
  end