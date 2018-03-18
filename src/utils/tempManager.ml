let temp_file_uid = ref 0

let temp_files_list = ref []

let new_temp () = 
  let _ = incr temp_file_uid in
  let name = Printf.sprintf "tmp/tmp_%d" !temp_file_uid in
  let () = temp_files_list := name :: (!temp_files_list) in
  name

let remove_temp name = 
  if List.exists ((=) name) (!temp_files_list) then
    Sys.remove name;
    temp_files_list := List.filter ((<>) name) (!temp_files_list)

let remove_all_temp () =
  List.iter remove_temp (!temp_files_list);
  temp_files_list := []
