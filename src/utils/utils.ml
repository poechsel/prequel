
let merge_list fct l = 
  if List.length l = 1 then 
    List.hd l
  else 
    List.fold_left (fun a b ->
        fct a b
      ) (List.hd l) (List.tl l)

