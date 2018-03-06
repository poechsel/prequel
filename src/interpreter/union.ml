class union (left : AlgebraTypes.feed_interface) (right : AlgebraTypes.feed_interface) =
  object(self)
    inherit AlgebraTypes.feed_interface
    val left = left
    val right = right
    
    method next = 
      match left#next with
          | None -> right#next
          | x -> x

    method reset = 
      let _ = left#reset in 
      right#reset

    method headers =
      left#headers
  end
      
