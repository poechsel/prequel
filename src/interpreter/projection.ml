(* keep must be a int array *)
let get_headers keep h = 
  Array.init (Array.length keep) (fun i -> h.(i))



class projection (sub : AlgebraTypes.feed_interface) ( projection : AlgebraTypes.header array ) =
  let all_headers = sub#headers in
  let keep = Array.make (Array.length projection) 0 in
  let _ = 
    for i = 0 to Array.length projection - 1 do
        for j = 0 to Array.length all_headers - 1 do
          if all_headers.(j) = projection.(i) then
            keep.(i) <- j
        done
      done 
  in 

  object(self)
    inherit AlgebraTypes.feed_interface
    val sub = sub
    val keep = keep

    method next =
      match sub#next with
      | None -> None
      | Some x ->
        Some(Array.init (Array.length keep) (fun i -> x.(keep.(i))))

    method reset =
      sub#reset

    method headers =
      get_headers keep sub#headers
  end 
