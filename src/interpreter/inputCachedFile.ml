class inputCachedFile name =
  let path' = name ^ ".csv" in
  let file' = open_in path' in
  object(self)
    inherit AlgebraTypes.feed_interface
    val name = name
    val path = path'
    val file = file'
    val mutable csv = Csv.of_channel ?has_header:(Some true) file'
    
    method next = 
      try 
        Some (Csv.next csv)
      with End_of_file ->
        None

    method reset = 
      let _ = Pervasives.seek_in file 0 in
      csv <- Csv.of_channel ?has_header:(Some true) file

    method headers =
      List.map (fun i -> Some(name), i) @@ Csv.Rows.header csv
  end
