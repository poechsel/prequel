type t = { name : string; csv: Csv.in_channel ref; file: Pervasives.in_channel }
type o = string

let open_feed s =
  let file = open_in @@ s ^ ".csv" in
  { name = s ^ ".csv"; file = file;  csv = ref (Csv.of_channel ?has_header:(Some true) file) }

let close_feed t = 
  ()


let next c = 
  try
    Some (Csv.next !(c.csv))
  with End_of_file ->
    None

let reset c = 
  let _ = Pervasives.seek_in c.file 0 in
  c.csv := Csv.of_channel ?has_header:(Some true) c.file 

let headers t = 
  Csv.Rows.header !(t.csv)
