let (<|) = (@@)

let array_concat s a = 
  String.concat s (Array.to_list a)

let array_find el ar =
  let rec aux i = 
    if i = Array.length ar then
      failwith "element not found"
    else
    if ar.(i) = el then
      i
    else aux (i+1)
  in aux 0

let merge_list fct l = 
  if List.length l = 1 then 
    List.hd l
  else 
    List.fold_left (fun a b ->
        fct a b
      ) (List.hd l) (List.tl l)

let option_map fct x = 
  match x with
  | None -> None
  | Some x -> Some (fct x)
