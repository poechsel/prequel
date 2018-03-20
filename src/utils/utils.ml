
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

let concat_list ?(sep=" ") l =
  List.fold_left (fun acc e -> acc^sep^e) (List.hd l) (List.tl l)
