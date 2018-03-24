let get_headers h_a h_b =
  Array.append h_a h_b

class join ((a : AlgebraTypes.feed_interface), (expr_a: AlgebraTypes.feed_result -> Ast.atom)) 
           ((b : AlgebraTypes.feed_interface), (expr_b: AlgebraTypes.feed_result -> Ast.atom)) =
  object(self)
    inherit AlgebraTypes.feed_interface

    val left = a
    val right = b
    val left_expr = expr_a
    val right_expr = expr_b
    val mutable current = None

    method next =
      let right_x = match right#next with
        | None -> let _ = right#reset in 
          let _ = current <- None in (* invalidate the previous entry -> we must compute the next one *)
          right#next
        | Some x -> Some x
      in
      let _ = match current with
      | None -> current <- left#next
      | _ -> ()
      in 
      match (current, right_x) with
      | Some x, Some y ->
        if left_expr x = right_expr y then
          Some (Array.append x y)
        else 
          self#next
      | _ -> None

    method reset =
      let _ = left#reset in
      right#reset

    method headers =
      get_headers left#headers right#headers
  end 
