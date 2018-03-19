class select (sub : AlgebraTypes.feed_interface) (selector : AlgebraTypes.expression) =
  object (self)
    inherit AlgebraTypes.feed_interface

    val filter = selector

    method next =
      let rec aux () =
        match sub#next with
        | None -> None
        | Some x ->
          let headers = self#headers in
          let tbl = Hashtbl.create 0 in
          let _ = Array.iter2 (fun h v ->
              Hashtbl.add tbl h v
            ) headers x
          in if Arithmetics.execute_filter filter tbl then
            Some x
          else aux ()
      in aux ()

    method reset = 
      sub#reset

    method headers = 
      sub#headers
        
  end 
