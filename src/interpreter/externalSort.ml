let rec csv_of_list channel l = 
  (* returns unit. Write a list of string to a channel
     in the form of a csv *)
  match l with 
  | [] -> ()
  | h::t -> 
    let _ = String.concat ", " h
            |> output_string channel
    in let _ = output_string channel "\n"
    in csv_of_list channel t

let csv_to_file headers file l =
  let channel = open_out file in
  let _ = output_string channel (String.concat "," (List.map snd headers)) in
  let _ = output_string channel "\n" in 
  let () = csv_of_list channel l in
  close_out channel


let tail_map f l = 
  let rec aux l acc =
    match l with
    | [] -> List.rev acc
    | x::tl -> aux tl (f x :: acc)
  in aux l []


let evaluate_row headers keys row = 
  let tbl = Arithmetics.Env.make headers row in
  Faster_map.faster_map (fun x -> Arithmetics.execute_value x tbl) keys

let rec initialize_sort ?(size_chunk=(1 lsl 22)) headers keys feed = 
  let sort headers keys l = 
    (* sort a list in memory efficiently *)
    (* here, tail recursivity is needed. If we take chunks of 10mo, 
       without tail recursivity, ie with a normal map, ocaml crash with
       a stackoverfow *)
    let l = Faster_map.faster_map (fun y -> 
        evaluate_row headers keys y, y
      ) l 
    in let l = List.sort Pervasives.compare l
    in Faster_map.faster_map snd l

  in 
  let rec aux file_name current_size acc filelist = 
    (* split the file into basic 'blocks' who are sorted in memory, then saved.
        Returns a list of file names representing these chunks
    *)
    match feed#next with
    | None when acc <> [] ->
      let _ = csv_to_file headers file_name (sort headers keys acc) in
      file_name::filelist
    | Some x ->
      let current_size = current_size + List.fold_left (fun a b -> a + String.length b + 1) 0 x in
      if current_size > size_chunk then
        (* if the current chunk is larger than the authorized size, create a new file *)
        let _ = csv_to_file headers file_name (sort headers keys (x::acc)) in
        aux (Utils.get_next_temp_file ()) 0 [] (file_name::filelist)
      else 
        aux file_name current_size (x::acc) filelist
    | _ -> filelist
  in aux (Utils.get_next_temp_file ()) 0 [] []


let rec kway_merge channel headers keys t = 
  while PriorityQueue.get_size t > 0 do
    let _, min_csv = PriorityQueue.pop t in
    let () = output_string channel (String.concat ", " @@ Csv.current_record min_csv); output_string channel "\n" in
    begin try
        let next = Csv.next min_csv in
        let append = (let tbl = Arithmetics.Env.make headers next in
                      List.map (fun x -> Arithmetics.execute_value x tbl) keys,
                      min_csv)
        in PriorityQueue.insert t append
      with _ -> ()
    end
  done 

let rec submerges ?(sub_groups_size=256) headers keys csvs =
  let split_groups n csvs = 
    let rec aux i l groups current = 
      match l with
      | [] -> 
        if i > 0 then 
          current :: groups 
        else 
          groups
      | x :: tl ->
        if i = n then
          aux 0 tl ((x::current)::groups) []
        else 
          aux (i+1) tl groups (x::current)
    in aux 0 csvs [] []
  in 
  let subgroups = split_groups sub_groups_size csvs in
  let all_files = List.map (fun group ->
      let _ = Printf.printf "switching to next group\n" in let _ = flush stdout in 
      let t = PriorityQueue.make (List.length group) (fun a b -> Pervasives.compare (fst a) (fst b)) in
      let _ = List.iter (fun file ->
          let csv = Csv.of_channel ~has_header:true (open_in file) in
          let row = Csv.next csv in
          (evaluate_row headers keys row, csv)
          |> PriorityQueue.insert t 
        ) group in
      let file = Utils.get_next_temp_file () in
      let output = open_out file in
      let _ = output_string output (String.concat "," (List.map snd headers)); output_string output "\n" in
      let _ = kway_merge output headers keys t in
      let _ = close_out output in
      let _ = List.iter Sys.remove group in
      file
    ) subgroups
  in if List.length all_files = 1 then
    List.hd all_files
  else submerges headers keys all_files



let external_sort feed headers keys =
  let start = Unix.gettimeofday() in
  let _ = Printf.printf "FIRST STEP\n" in let _ = flush stdout in
  let csvs = initialize_sort headers keys feed in
 let stop = Unix.gettimeofday ()
    in let () = Printf.printf "Execution time: %fs\n%!" (stop -. start) in
  let _ = Printf.printf "SECOND STEP\n" in let _ = flush stdout in
  let start = Unix.gettimeofday() in
  let file = submerges headers keys csvs in
 let stop = Unix.gettimeofday ()
    in let () = Printf.printf "Execution time: %fs\n%!" (stop -. start) in
  new InputCachedFile.inputCachedFile file




class sort (sub: AlgebraTypes.feed_interface) (keys : AlgebraTypes.expression list) =
  object(self)
    inherit AlgebraTypes.feed_interface

    val mutable initialized = false
    val mutable cache = []
    val mutable sub = sub
    
    method next = 
      let _ = if initialized = false then
        let _ = initialized <- true in
        let headers = self#headers in
        let _ = sub <- external_sort sub headers keys in ()
      in 
      sub#next

    method reset = 
      sub#reset

    method headers = 
      sub#headers
  end
