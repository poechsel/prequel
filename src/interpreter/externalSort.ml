let compare headers keys a b =
  let tbl_a = Arithmetics.Env.make headers a in
  let tbl_b = Arithmetics.Env.make headers b in
  let rec aux keys =
       match keys with
       | [] -> 0
       | x::tl ->
         let temp = Pervasives.compare 
             (Arithmetics.execute_value x tbl_a) 
             (Arithmetics.execute_value x tbl_b)
         in if temp = 0 then aux tl
         else temp
  in aux keys


let rec csv_of_list channel l = 
  (* returns unit. Write a list of string to a channel
     in the form of a csv *)
  match l with 
  | [] -> ()
  | h::t -> 
    let _ = String.concat ", " h
            |> Printf.fprintf channel "%s\n" 
    in csv_of_list channel t

let csv_to_file file l =
  let channel = open_out file in
  let () = csv_of_list channel l in
  close_out channel


let tail_map f l = 
  let rec aux l acc =
    match l with
    | [] -> List.rev acc
    | x::tl -> aux tl (f x :: acc)
  in aux l []


let rec initialize_sort ?(size_chunk=65335(*10000000*)) headers keys feed = 
  let sort headers keys l = 
    (* sort a list in memory efficiently *)
    (* here, tail recursivity is needed. If we take chunks of 10mo, 
       without tail recursivity, ie with a normal map, ocaml crash with
       a stackoverfow *)
    let l = tail_map (fun y -> 
        let tbl = Arithmetics.Env.make headers y in
        tail_map (fun x -> Arithmetics.execute_value x tbl) keys,
        y
      ) l 
    in let l = List.sort Pervasives.compare l
    in tail_map snd l

  in 
  let rec aux file_name current_size acc filelist = 
    (* split the file into basic 'blocks' who are sorted in memory, then saved.
        Returns a list of file names representing these chunks
    *)
    match feed#next with
    | None ->
      let _ = if acc <> [] then 
          csv_to_file file_name (sort headers keys acc) in
      let _ = List.iter (fun x -> Printf.printf "%s\n" x) (file_name::filelist) in
      file_name::filelist
    | Some x ->
      let current_size = current_size + List.fold_left (fun a b -> a + String.length b + 1) 0 x in
      if current_size > size_chunk then
        let _ = Printf.printf "creating new chunk\n" in let _ = flush stdout in
        (* if the current chunk is larger than the authorized size, create a new file *)
        let _ = csv_to_file file_name (sort headers keys (x::acc)) in
        aux (Utils.get_next_temp_file ()) 0 [] (file_name::filelist)
      else 
        aux file_name current_size (x::acc) filelist
  in aux (Utils.get_next_temp_file ()) 0 [] []


let rec kway_merge channel headers keys csvs = 
  let find_min csvs = 
    (* find the minimum element *)
    let rec aux i csvs (previous_min, previous_key) = 
      match csvs with 
      | [] -> 
        previous_min
      | (key, x)::tl when Pervasives.compare key previous_key <= 0 ->
        aux (i+1) tl (i, key)
      | _::tl ->
        aux (i+1) tl (previous_min, previous_key)
    in aux 1 (List.tl csvs) (0, fst @@ List.hd csvs)

  in let update_min min_i csvs headers keys =
       (* update the minimum value so that it points toward its next entry.
          If their is no next entries, remove it *)
       let rec aux i csvs = 
         match csvs with
         | (_, x)::tl when i = min_i ->
           begin try
               let next = Csv.next x in
               (let tbl = Arithmetics.Env.make headers next in
                List.map (fun x -> Arithmetics.execute_value x tbl) keys,
                x) :: tl
             with _ ->
               tl
           end 
         | x::tl ->
           x::(aux (i+1) tl)
         | [] -> []
       in aux 0 csvs

  in let rec merge csvs = 
       (* merge the db contained into the csv of csvs. Save it to channel*)
       match csvs with
       | [] -> 
         ()
       | _ ->
         let min_i = find_min csvs in
         let min_csv = snd @@ List.nth csvs min_i in
         let () = Printf.fprintf channel "%s\n" (String.concat ", " @@ Csv.current_record min_csv) in
         let csvs = update_min min_i csvs headers keys in
         merge csvs
  in merge csvs


let rec submerges ?(sub_groups_size=10) headers keys csvs =
  let split_groups n csvs = 
    let rec aux i l groups current = 
      match l with
      | [] -> current :: groups
      | x :: tl ->
        if i = n then
          aux 0 tl ((x::current)::groups) []
        else 
          aux (i+1) tl groups (x::current)
    in aux 0 csvs [] []

  in 
  let subgroups = split_groups sub_groups_size csvs in
  let _ = List.iter (fun group ->
                     let _ = Printf.printf "subdvizing into: " in            
                     let _ = List.iter (fun x -> Printf.printf "%s " x) group in
                     print_string "\n"
                    ) subgroups
  in let _ = flush stdout in 
  let all_files = List.map (fun group ->
      let _ = Printf.printf "switching to next group\n" in let _ = flush stdout in 
      let csvs = List.map (fun x -> Csv.of_channel @@ open_in x) group in
      let csvs = List.map (fun x -> 
          let y = Csv.next x in
          let tbl = Arithmetics.Env.make headers y in
          List.map (fun x -> Arithmetics.execute_value x tbl) keys,
          x
        ) csvs in
      let file = Utils.get_next_temp_file () in
      let output = open_out file in
      let _ = kway_merge output headers keys csvs in
      let _ = close_out output in
      let _ = List.iter (fun x -> let _ = Printf.printf "removing %s\n" x in Sys.remove x) group in
      let _ = flush stdout in
      file
    ) subgroups
  in if List.length all_files = 1 then
    List.hd all_files
  else submerges headers keys all_files



let external_sort feed headers keys =
  let _ = Printf.printf "FIRST STEP\n" in let _ = flush stdout in
  let csvs = initialize_sort headers keys feed in
  let _ = Printf.printf "SECOND STEP\n" in let _ = flush stdout in
  let file = submerges headers keys csvs in
  new InputCachedFile.inputCachedFile file
  (*
  let csvs = List.map (fun x -> Csv.of_channel @@ open_in x) csvs in
  let csvs = List.map (fun x -> 
      let y = Csv.next x in
      let tbl = Arithmetics.Env.make headers y in
      List.map (fun x -> Arithmetics.execute_value x tbl) keys,
      x
    ) csvs in
  let file = Utils.get_next_temp_file () in
  let output = open_out file in
  
  let _ = kway_merge output headers keys csvs in
  let _ = close_out output in
  new InputCachedFile.inputCachedFile file

    *)



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
      (*
      let _ = 
        if initialized = false then
          let _ = initialized <- true in
          let rec aux acc = 
            match sub#next with
            | None -> acc
            | Some x -> aux (x::acc)
          in 
          let headers = self#headers in

          let _ = cache <- aux [] in
          let _ = Printf.printf "size %d \n" @@ List.length cache in
          let _ = cache <- List.sort (compare headers keys) cache in
          Printf.printf "size %d \n" @@ List.length cache
      in 
      match cache with
      | [] -> None
      | x::tl -> let _ = cache <- tl in Some x

    method reset = 
      let _ = initialized <- false in 
      sub#reset

    method headers =
      sub#headers
    *)
  end
