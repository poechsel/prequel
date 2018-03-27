class materialize sub name =
  let _ =
    let ic = open_out name in
    let _ = Printf.fprintf ic "%s\n" @@ Utils.array_concat ", " (Array.map snd sub#headers) in
    close_out ic
  in 
    
  object(self)
    inherit AlgebraTypes.feed_interface
    val path = name
    val mutable sub = sub
    val mutable initialize = false

    method next = 
      if not initialize then
        let ic = open_out path in
        let _ = sub#save ic in
        let _ = initialize <- true in
        let _ = close_out ic in
        sub <- new InputCachedFile.inputCachedFile path
      else ();
         
        sub#next

    method reset = 
      sub#reset

    method headers =
      sub#headers
  end



class unmaterialize headers name =
  object(self)
    inherit AlgebraTypes.feed_interface
    val sub = new InputCachedFile.inputCachedFile name

    method next = 
      sub#next

    method reset = 
      sub#reset

    method headers =
      headers
  end
