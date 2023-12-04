open Base
open Stdio

type maxes = {
  mutable max_blue : int;
  mutable max_green : int;
  mutable max_red : int;
}

let day2 () =
  let item_valid s ms =
    let (num_str, color) = String.lsplit2_exn ~on:' ' @@ String.strip s in
    let num = Int.of_string num_str in
    match color with
    | "red" -> ms.max_red <- max ms.max_red num
    | "green" -> ms.max_green <- max ms.max_green num
    | "blue" -> ms.max_blue <- max ms.max_blue num
    | _ -> failwith "bad color" in
  
  let subset_valid s ms =
    let items = String.split ~on:',' s in
    List.iter ~f:(fun s -> item_valid s ms) items in

  let process_game line =
    let ms = {max_red = 0; max_green = 0; max_blue = 0} in
    let (_game_id_str, rest) = String.chop_prefix_exn ~prefix:"Game " line |> String.lsplit2_exn ~on:':' in
    (* let game_id =  Int.of_string game_id_str in *)
    let subsets = String.strip rest |> String.split ~on:';' |> List.map ~f:String.strip in
    let _ = List.iter ~f:(fun s -> subset_valid s ms) subsets in
    ms.max_red * ms.max_green * ms.max_blue in
    
  let lines = In_channel.read_lines "input.txt" in
  (* I don't really get why List.sum wants to map first *)
  List.sum (module Int) ~f:process_game lines

let () = day2 () |> Int.to_string |> print_endline