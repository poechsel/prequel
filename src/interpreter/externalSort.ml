let tail_map f l = 
  let rec aux l acc =
    match l with
    | [] -> List.rev acc
    | x::tl -> aux tl (f x :: acc)
  in aux l []


let evaluate_row headers keys row = 
  let tbl = Arithmetics.Env.make headers row in
  Faster_map.faster_map (fun x -> Arithmetics.execute_value x tbl) keys


(*
let serialize channel (comp, line) = 
  output_binary_int channel @@ List.length comp;
  let rec aux l = 
    match l with
    | [] -> ()
    | Ast.Number x :: tl -> begin
        output_byte channel 0;
        output_binary_int channel x;
        aux tl
      end 
    | Ast.String x :: tl -> begin
        output_byte channel 1;
        output_binary_int channel (String.length x);
        output_string channel x;
        aux tl
      end 
    | _ -> failwith ""
  in aux comp;
  output_binary_int channel @@ List.length line;
  let rec aux l =
    match l with
    | [] -> ()
    | x::tl ->
    output_binary_int channel @@ String.length x;
    output_string channel x; aux tl
  in aux line

(*)
  for i = 0 to Array.length line - 1 do
    output_binary_int channel @@ String.length line.(i);
    output_string channel line.(i)
  done; ()
*)

let deserialize channel = 
  let length = input_binary_int channel in
  let rec aux i = 
    if i = length then
      []
    else begin
      match input_byte channel with
      | 0 ->
        Ast.Number (input_binary_int channel) :: aux (i+1)
      | 1 ->
        let l = input_binary_int channel in
        Ast.String (really_input_string channel l) :: aux (i+1)
      | _ -> failwith ""
    end
  in let comp = aux 0 in
  let length = input_binary_int channel in
  let rec aux i = 
    if i = length then
      []
    else
      let l = input_binary_int channel in
      really_input_string channel l :: aux (i+1)
in comp, aux length
  (*
  let line = Array.make length "" in
  let _ = for i = 0 to length - 1 do
      let l = input_binary_int channel in
      line.(i) <- really_input_string channel l
    done
  in comp, line
*)
   *)


let to_file headers file offset buffer=
  let channel = open_out_bin file in
  let _ = 
    for i = offset to Array.length buffer - 1 do
      Marshal.to_channel channel buffer.(i) [Marshal.No_sharing]
    done in
  close_out channel

let rec initialize_sort ?(size_chunk=(1 lsl 18)) headers keys feed = 
  (* to initialize our exeternal sort, we sort some small parts of the file
     Because Array.sort is far more efficient than List.sort, we use an array buffer
     to store everything we need *)
  let buffer = Array.make size_chunk (Obj.magic ()) in
  let sort headers keys = 
    Array.fast_sort (fun a b -> Pervasives.compare (fst a) (fst b)) buffer 

  in 
  let rec aux current_size filelist = 
    (* split the file into basic 'blocks' who are sorted in memory, then saved.
        Returns a list of file names representing these chunks
    *)
    match feed#next with
    | None when current_size > 0 ->
      let _ = 
        for i = current_size to size_chunk - 1 do
          (* only the start of the buffer is filled there.
             Because ocaml's sort can't sort between two indexes, we fill the
             buffer with values that are small: they will be placed on the 
             start of the buffer *)
          buffer.(i) <- ([], [])
        done in
      let _ = sort headers keys in
      let file_name = TempManager.new_temp () in
      let _ = to_file headers file_name (size_chunk - current_size) buffer in
      file_name::filelist
    | Some x ->
      let () = buffer.(current_size) <- evaluate_row headers keys x, x in
      if current_size + 1 = size_chunk then
        (* if the current chunk is larger than the authorized size, create a new file *)
        let _ = sort headers keys in
        let file_name = TempManager.new_temp () in
        let _ = to_file headers file_name 0 buffer in
        aux 0 (file_name::filelist)
      else 
        aux (current_size + 1) filelist
    | _ -> filelist
  in aux 0 []


let rec kway_merge ?(write_to_csv=false) channel headers keys t = 
  (* merge k streams using a priority queue *)
  while PriorityQueue.get_size t > 0 do
    let el, min_csv = PriorityQueue.pop t in
    let () = 
      if write_to_csv then (output_string channel (String.concat ", " @@ snd el); output_string channel "\n") 
      else Marshal.to_channel channel el [Marshal.No_sharing]
    in
    begin try
        let next = Marshal.from_channel min_csv in
        let append = (next, min_csv)
        in PriorityQueue.insert t append
      with _ -> ()
    end
  done 

let rec submerges ?(sub_groups_size=256) headers keys csvs =
  (* will merge all the subfolders by batch of size at most sub_groups_size *)
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
      let t = PriorityQueue.make (List.length group) (fun a b -> Pervasives.compare (fst a) (fst b)) in
      let _ = List.iter (fun file ->
          let channel = open_in_bin file in
          let el = Marshal.from_channel channel in
          (el, channel)
          |> PriorityQueue.insert t 
        ) group in
      let write_to_csv = List.length subgroups = 1 in
      let file = TempManager.new_temp () in
      let output = 
        if write_to_csv then begin
          let output = open_out file in
          output_string output (String.concat "," (List.map snd headers));
          output_string output "\n";
          output;
        end else 
          open_out_bin file in
      let _ = kway_merge output headers keys t ~write_to_csv:write_to_csv in
      let _ = close_out output in
      let _ = List.iter TempManager.remove_temp group in
      file
    ) subgroups
  in if List.length all_files = 1 then
    List.hd all_files
  else submerges headers keys all_files



let external_sort feed headers keys =
  let csvs = initialize_sort headers keys feed in
  let file = submerges headers keys csvs in
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
