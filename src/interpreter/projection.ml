class projection (sub : AlgebraTypes.feed_interface) ( headers : AlgebraTypes.header list ) =
  let all_headers = sub#headers in
  let keep = List.filter (fun x -> x >= 0) 
      (List.mapi (fun i x ->
           if List.exists (fun a -> a = x) headers then i
           else raise (Errors.InterpretationError 
                         (Printf.sprintf "Attribute \"%s\" doesn't exists" (snd x))
                      )
         ) all_headers) in

  object(self)
    inherit AlgebraTypes.feed_interface
    val sub = sub
    val keep = keep

    method next =
      match sub#next with
      | None -> None
      | Some x ->
        Some (List.map (fun i -> List.nth x i) keep)

    method reset =
      sub#reset

    method headers =
      List.map (fun i -> List.nth sub#headers i) keep
  end 
