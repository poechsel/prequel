let get_headers name h = 
  Array.map (fun i -> (name, snd i)) h

class rename_table (sub : AlgebraTypes.feed_interface) ( name : string ) =
  object(self)
    inherit AlgebraTypes.feed_interface

    val sub = sub
    val name = name

    method next = sub#next

    method reset =
      sub#reset

    method headers =
      get_headers name sub#headers
  end 
