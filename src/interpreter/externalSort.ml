(*
Quick overview on optimisation for speed:
   - we are using arrays when we can instead of lists. After testing, it is
     far more efficient, especially when sorting
   - keys are computed only once, and are storred inside temporary files. Even 
     though this yield larger files, recomputing them decreases performances by 10%
     -> This is no more true since we use arrays instead of list to represent
        rows. If the expressions on which the keys are computed are simple enough, 
        not caching the keys on the hdd might yield the same performances (if not 
        slightly better performances)
   - Marshalling is the quickest way we found to write temporary files. Converting to 
     csv is less flexible, and doing its own serialisation is less performant.
     On top of that, Marshalling is independant from the data we want to write !
   - transitionning to using Arrays to represent rows lead to a tremendous decrease
     in ram usage. Probably because arrays are mutable and not lists.
     ~ the mistery of the gc
*)
let evaluate_row keys row = 
  Array.map (fun (f, _) -> f row) keys

let serialize a b = Marshal.to_channel a b [Marshal.No_sharing]
let deserialize = Marshal.from_channel


(** Compares arrays a and b using the ordering from keys. *)
let ordered_compare
  (keys : ('b * Ast.ordering) array)
  (a : 'a array)
  (b : 'a array) : int =

  let la = Array.length a in
  let lb = Array.length b in

  if la = 0 && lb = 0 then
    0
  else if lb = 0 then
    1
  else if la = 0 then
    -1
  else begin
    let cmp x y = function
      | Ast.Asc  -> (Pervasives.compare x y)
      | Ast.Desc -> (Pervasives.compare x y) * (-1) in

    let rec aux i =
      if i >= la then
        0
      else
        let c = cmp a.(i) b.(i) (snd keys.(i)) in
        if c <> 0 then
          c
        else
          aux (i + 1)
    in aux 0
  end

let to_file headers file offset buffer=
  let channel = open_out_bin file in
  let _ = 
    for i = offset to Array.length buffer - 1 do
        serialize channel (snd buffer.(i))
    done in
  close_out channel

type init_results = 
    InRam of string array array * int
  | InHdd of string list

let rec initialize_sort ?(size_chunk=(1 lsl 18)) headers keys feed = 
  (* to initialize our exeternal sort, we sort some small parts of the file
     Because Array.sort is far more efficient than List.sort, we use an array buffer
     to store everything we need *)
  let buffer = Array.make size_chunk (Obj.magic ()) in
  let sort () = 
    Array.fast_sort (fun a b -> ordered_compare keys (fst a) (fst b)) buffer 

  in 
  let rec aux current_size filelist = 
    (* split the file into basic 'blocks' who are sorted in memory, then saved.
        Returns a list of file names representing these chunks
    *)
    match feed#next with
    | None ->
      let _ = 
        for i = current_size to size_chunk - 1 do
          (* only the start of the buffer is filled there.
             Because ocaml's sort can't sort between two indexes, we fill the
             buffer with values that are small: they will be placed on the 
             start of the buffer *)
          buffer.(i) <- ([||], [||])
        done in
      let _ = sort () in
      (* if the result fits in one chunk, then we keep it in ram without writing
         it to disk*)
      if filelist = [] then
        InRam (Array.map snd buffer, (size_chunk - current_size))
      else 
        let file_name = TempManager.new_temp () in
        let _ = to_file headers file_name (size_chunk - current_size) buffer in
        if current_size = 0 then
          InHdd(filelist)
        else
          InHdd(file_name::filelist)
    | Some x ->
      let () = buffer.(current_size) <- evaluate_row keys x, x in
      if current_size + 1 = size_chunk then
        (* if the current chunk is larger than the authorized size, create a new file *)
        let _ = sort () in
        let file_name = TempManager.new_temp () in
        let _ = to_file headers file_name 0 buffer in
        aux 0 (file_name::filelist)
      else 
        aux (current_size + 1) filelist
  in aux 0 []


let rec kway_merge ?(write_to_csv=false) channel headers keys t = 
  (* merge k streams using a priority queue *)
  while PriorityQueue.get_size t > 0 do
    let el, min_csv = PriorityQueue.pop t in
    let () = 
      if write_to_csv then (
        let a = snd el in
        for i = 0 to Array.length a - 1 do
          if i > 0 then output_string channel ",";
          output_string channel a.(i)
        done;
        output_string channel "\n") 
      else serialize channel (snd el)
    in
    begin try
        let next = deserialize min_csv in
        let append = ((evaluate_row keys next, next), min_csv)
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
      let t = PriorityQueue.make (List.length group) 
          (fun a b -> ordered_compare keys (fst (fst a)) (fst (fst b))) 
      in
      let _ = List.iter (fun file ->
          let channel = open_in_bin file in
          let el = deserialize channel in
          let row = ((evaluate_row keys el, el), channel) in
          PriorityQueue.insert t row
        ) group in
      let write_to_csv = List.length subgroups = 1 in
      let file = TempManager.new_temp () in
      let output = 
        if write_to_csv then begin
          let output = open_out file in
          output_string output (Utils.array_concat "," (Array.map snd headers));
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
  match csvs with
  | InRam(datas, offset) ->
    new InputArray.inputArray headers datas offset
  | InHdd(csvs) ->
    let file = submerges headers keys csvs in
    new InputCachedFile.inputCachedFile file



class sort
  (sub: AlgebraTypes.feed_interface)
  (keys : (AlgebraTypes.expression * Ast.ordering) array) =
  object(self)
    inherit AlgebraTypes.feed_interface

    val mutable initialized = false
    val mutable cache = []
    val keys = 
      let h = sub#headers in 
      Array.map (fun (v, ord) -> (Arithmetics.compile_value h v, ord)) keys
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
