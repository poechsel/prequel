let get_headers headers =
  headers

class inputArray headers datas offset =
  object(self)
    inherit AlgebraTypes.feed_interface
    val headers = headers
    val datas = datas
    val mutable index = offset
    val offset = offset
    
    method next = 
      if index = Array.length datas then
        None
      else 
        let _ = index <- index + 1 in
        Some (datas.(index - 1))

    method reset = 
      index <- offset

    method headers =
      get_headers headers
  end
