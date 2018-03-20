let get_headers name =
  let ic = open_in name in
  let csv = Csv.of_channel ~has_header:true ic in
  let () = close_in ic in
  Csv.Rows.header csv
  |> List.map (fun i -> "", i)
  |> Array.of_list

class inputCachedFile name =
  let path' = name in
  let file' = try
      open_in path' 
    with e ->
        raise (Errors.InterpretationError (Printf.sprintf "error: file \"%s\" doesn't exists" path')) in

  object(self)
    inherit AlgebraTypes.feed_interface
    val name = name
    val path = path'
    val file = file'
    val mutable csv = Csv.of_channel ?has_header:(Some true) file'
    
    method next = 
      try 
        Some (Csv.next csv |> Array.of_list)
      with End_of_file ->
        None

    method reset = 
      let _ = Pervasives.seek_in file 0 in
      csv <- Csv.of_channel ?has_header:(Some true) file

    method headers =
      get_headers path'
  end
