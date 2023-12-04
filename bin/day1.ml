open Base
open Stdio

let day1 () =
  let check_at s pos =
    let num_strs = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"] in
    let s_res = List.findi num_strs ~f:(fun _ num_str -> String.is_substring_at s ~pos ~substring:num_str) in
    let s_num = Option.map s_res ~f:(fun res -> 1 + (fst res)) in
    let c_num = Char.get_digit @@ String.get s pos in
    Option.first_some s_num c_num in

  let rec get_nums' s nums pos =
    let nums = match check_at s pos with
      | Some x -> x :: nums
      | None -> nums in
    if pos = 0 then nums else get_nums' s nums (pos - 1) in
  let get_nums s = get_nums' s [] (String.length s - 1) in

  let value line =
    let nums = get_nums line in
    let x = List.nth_exn nums 0 in
    let y = List.last_exn nums in
    x * 10 + y in

  let lines = In_channel.read_lines "input.txt" in
  List.sum (module Int) ~f:value lines 

let () = day1 () |> Int.to_string |> print_endline
