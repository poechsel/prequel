class select (sub : AlgebraTypes.feed_interface) (selector : AlgebraTypes.feed_result -> bool) =
  object (self)
    inherit AlgebraTypes.feed_interface

    val filter = selector

    method next =
      let rec aux () =
        match sub#next with
        | None -> None
        | Some x ->
          if selector x then
            Some x
          else aux ()
      in aux ()

    method reset = 
      sub#reset

    method headers = 
      sub#headers
        
  end 
