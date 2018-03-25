let get_headers h_a h_b =
  Array.append h_a h_b


let find_all_elements elements target_value evaluator = 
  let rec binary_search low high =
    if low <= high then
        let middle = low + (high - low) / 2 in
        let v = evaluator elements.(middle) in
        if v = target_value then 
          middle
        else if v < target_value then
          binary_search (middle+1) high
        else 
          binary_search low (middle-1)
    else 
      -1
  in 
  let indice = binary_search 0 (Array.length elements - 1)
  in 
  if indice < 0 then 
    None
  else 
    let start_section = 
      let s = ref indice in
      let _ = while !s > 0 && evaluator elements.(!s-1) = target_value do
        decr s
      done 
      in !s
    in 
    let rec aux i acc =
         if i >= Array.length elements then
           List.rev acc
         else if evaluator elements.(i) <> target_value then
           List.rev acc
         else 
           aux (i+1) (elements.(i) :: acc)
    in Some (aux start_section [])


class joinSorted 
    ((a : AlgebraTypes.feed_interface), (expr_a: AlgebraTypes.feed_result -> Ast.atom)) 
    ((b : AlgebraTypes.feed_interface), (expr_b: AlgebraTypes.feed_result -> Ast.atom)) =
  object(self)
    inherit AlgebraTypes.feed_interface

    val left = a
    val right = b
    val left_expr = expr_a
    val right_expr = expr_b
    val mutable current = None
    val mutable right_interval = None
    val mutable initialized = false
    val mutable right_values = [||]

    method next =
      let _ = if (not initialized) then
          let _  = initialized <- true in
          let _ = right_values <-
              right#to_list 
              |> Array.of_list
          in ()
      in
      match right_interval with
      | None ->
        let _ = current <- left#next in begin
          match current with
          | None -> None
          | Some x -> 
            right_interval <- find_all_elements right_values (left_expr x) right_expr;
            self#next
        end 
      | Some (hd::tl) ->
        let _ = right_interval <- Some tl in
        begin match current with
          | None -> None
          | Some x ->
            Some (Array.append x hd)
        end 
      | Some [] ->
        right_interval <- None;
        self#next

    method reset =
      left#reset

    method headers =
      get_headers left#headers right#headers
  end 


class joinHash (
    (a : AlgebraTypes.feed_interface), (expr_a: AlgebraTypes.feed_result -> Ast.atom)) 
    ((b : AlgebraTypes.feed_interface), (expr_b: AlgebraTypes.feed_result -> Ast.atom)) =
  object(self)
    inherit AlgebraTypes.feed_interface

    val left = a
    val right = b
    val left_expr = expr_a
    val right_expr = expr_b
    val tbl = Hashtbl.create 100
    val mutable current = None
    val mutable current_row = []
    val mutable initialized = false

    method next =
      let _ = if (not initialized) then
          let _  = initialized <- true in
          let _ = right#iterate (fun line ->
              let value = right_expr line in
              if Hashtbl.mem tbl value then
                let t = Hashtbl.find tbl value in
                t := line :: !t
              else 
                Hashtbl.add tbl value (ref [line])
            )
          in ()
      in
      match current_row with
        | [] -> 
          current <- left#next;
          begin match current with 
            | None -> None
            | Some x -> 
              let value = left_expr x in
              let _ = current_row <- 
                  if Hashtbl.mem tbl value then
                    !(Hashtbl.find tbl value)
                  else 
                    []
              in 
              self#next
          end
        | x::tl -> 
          current_row <- tl;
          begin match current with
            | None -> None
            | Some y -> Some (Array.append y x)
          end

    method reset =
      left#reset

    method headers =
      get_headers left#headers right#headers
  end 

