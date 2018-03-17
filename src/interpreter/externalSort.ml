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

module PriorityQueue = struct
    type d = Ast.atom list * Csv.in_channel
    type t = { mutable size : int; data : d array; comp : d -> d -> int}

    let make n comp =
      let rec next_pow_two acc = 
        if acc < n then
          next_pow_two (2 * acc)
        else acc
      in 
      { size = 0; data = Array.make (next_pow_two n) (Obj.magic ()); comp = comp }

    let parent i = (i-1) / 2

    let left i = 2 * i + 1

    let right i = 2 * i + 2

    let swap heap i j =
        let temp = heap.data.(i) in
        let _ = heap.data.(i) <- heap.data.(j) in
        let _ = heap.data.(j) <- temp in
        ()

    let rec down_heap heap i = 
      let smallest = i in
      let smallest = if left i < heap.size && heap.comp (heap.data.(left i)) (heap.data.(i)) < 0 then
          left i
        else 
          smallest
      in 
      let smallest = if right i < heap.size && heap.comp (heap.data.(right i)) (heap.data.(i)) < 0 then
          right i
        else 
          smallest
      in if smallest != i then
        let _ = swap heap i smallest in
        down_heap heap smallest

    let rec up_heap heap i = 
      if i != 0 then
        let f = parent i in
        if heap.comp (heap.data.(f)) (heap.data.(i)) >= 0 then
          let _ = swap heap i f in
        up_heap heap f

    let insert heap element = 
      let _ = heap.data.(heap.size) <- element in
      let _ = heap.size <- heap.size + 1 in 
      let _ = up_heap heap (heap.size-1) in
      ()

    let get_size t = t.size


    let pop heap = 
      let e = heap.data.(0) in
      let _ = heap.size <- heap.size - 1 in
      let _ = heap.data.(0) <- heap.data.(heap.size) in
      let _ = down_heap heap 0 in
      e

    let dump heap = 
      for i = 0 to heap.size - 1 do
        Printf.printf "%d  " (let Ast.Number(x) :: _, _ = heap.data.(i) in x)
      done ;
      Printf.printf "\n"
end 


let rec initialize_sort ?(size_chunk=(1 lsl 22)) headers keys feed = 
  let sort headers keys l = 
    (* sort a list in memory efficiently *)
    (* here, tail recursivity is needed. If we take chunks of 10mo, 
       without tail recursivity, ie with a normal map, ocaml crash with
       a stackoverfow *)
    let l = Faster_map.faster_map (fun y -> 
        let tbl = Arithmetics.Env.make headers y in
        Faster_map.faster_map (fun x -> Arithmetics.execute_value x tbl) keys,
        y
      ) l 
    in let l = List.sort Pervasives.compare l
    in Faster_map.faster_map snd l

  in 
  let rec aux file_name current_size acc filelist = 
    (* split the file into basic 'blocks' who are sorted in memory, then saved.
        Returns a list of file names representing these chunks
    *)
    match feed#next with
    | None ->
      let _ = if acc <> [] then 
          csv_to_file headers file_name (sort headers keys acc) in
      let _ = List.iter (fun x -> Printf.printf "%s\n" x) (file_name::filelist) in
      file_name::filelist
    | Some x ->
      let current_size = current_size + List.fold_left (fun a b -> a + String.length b + 1) 0 x in
      if current_size > size_chunk then
        let _ = Printf.printf "creating new chunk\n" in let _ = flush stdout in
        (* if the current chunk is larger than the authorized size, create a new file *)
        let _ = csv_to_file headers file_name (sort headers keys (x::acc)) in
        aux (Utils.get_next_temp_file ()) 0 [] (file_name::filelist)
      else 
        aux file_name current_size (x::acc) filelist
  in aux (Utils.get_next_temp_file ()) 0 [] []


let rec kway_merge channel headers keys t = 
  let rec merge t = 
    (* merge the db contained into the csv of csvs. Save it to channel*)
    match PriorityQueue.get_size t with
    | 0 -> 
      ()
    | _ ->
      let _, min_csv = PriorityQueue.pop t in
      let () = output_string channel (String.concat ", " @@ Csv.current_record min_csv); output_string channel "\n" in
      let _ = begin try
          let next = Csv.next min_csv in
          let append = (let tbl = Arithmetics.Env.make headers next in
                        List.map (fun x -> Arithmetics.execute_value x tbl) keys,
                        min_csv)
          in PriorityQueue.insert t append
        with _ -> ()
      end
      in merge t
           (*
       match csvs with
       | [] -> 
         ()
       | _ ->
         let min_i = find_min csvs in
         let min_csv = snd @@ List.nth csvs min_i in
         let () = Printf.fprintf channel "%s\n" (String.concat ", " @@ Csv.current_record min_csv) in
         let csvs = update_min min_i csvs headers keys in
         merge csvs
           *)
  in merge t

let rec submerges ?(sub_groups_size=256) headers keys csvs =
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
      let csvs = List.map (fun x -> Csv.of_channel ~has_header:true (open_in x)) group in
      let csvs = List.map (fun x -> 
          let y = Csv.next x in
          let tbl = Arithmetics.Env.make headers y in
          List.map (fun x -> Arithmetics.execute_value x tbl) keys,
          x
        ) csvs in
      let t = PriorityQueue.make (List.length group) (fun a b -> Pervasives.compare (fst a) (fst b)) in
      let _ = List.iter (fun x ->
        PriorityQueue.insert t x
        ) csvs in
      let file = Utils.get_next_temp_file () in
 let start = Unix.gettimeofday () in
      let output = open_out file in
      let _ = output_string output (String.concat "," (List.map snd headers)) in
      let _ = output_string output "\n" in
      let _ = kway_merge output headers keys t in
      let _ = close_out output in
 let stop = Unix.gettimeofday ()
    in let () = Printf.printf "Execution time: %fs\n%!" (stop -. start) in
      let _ = List.iter (fun x -> let _ = Printf.printf "removing %s\n" x in Sys.remove x) group in
      let _ = flush stdout in
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
