let get_headers h_a h_b =
  Array.append h_a h_b

class product (a : AlgebraTypes.feed_interface) ( b : AlgebraTypes.feed_interface ) =
  object(self)
    inherit AlgebraTypes.feed_interface

    val left = a
    val right = b
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
        Some (Array.append x y)
      | _ -> None

    method reset =
      let _ = left#reset in
      right#reset

    method headers =
      get_headers left#headers right#headers
  end 
