open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct

  (* Vir funkcije: drugi odvogor v https://stackoverflow.com/questions/10068713/string-to-list-of-char *)
  let explode s =
    List.init (String.length s) (String.get s)

  let get_value ch =
    let v = Char.code ch in
    if v < 95 then
      v - 65 + 27
    else
      v - 97 + 1

  let rec print_list = function
    | [] -> print_char '\n'
    | prvi :: ostalo ->
      let () = print_char prvi in
      print_list ostalo

  let rec find_matching_chars other = function
    | [] ->
      0
    | prvi :: ostalo ->
      let el = List.find_opt (fun ch -> ch = prvi) other in
      match el with
      | None -> find_matching_chars other ostalo
      | Some ch ->
        get_value ch

  let rec find_matching_3 o1 o2 = function
    | [] -> 0
    | prvi :: ostalo ->
      let el1 = List.find_opt (fun ch -> ch = prvi) o1 in
      let el2 = List.find_opt (fun ch -> ch = prvi) o2 in

      match (el1, el2) with
      | (None, _) -> find_matching_3 o1 o2 ostalo
      | (_, None) -> find_matching_3 o1 o2 ostalo
      | (Some a, Some b) -> get_value prvi

  let rec split_into_triplets = function
    | [] -> []
    | "" :: [] -> []
    | a :: b :: c :: ostalo -> (a, b, c) :: split_into_triplets ostalo
    | _ -> failwith "ne_triplets"

  let naloga1 data =
    let lines = List.lines data in
    let rec aux = function
      | [] -> 0
      | "" :: ostalo -> aux ostalo
      | s :: ostalo ->
        let polovici = List.split_on_n (explode s) ((String.length s) / 2) in
        (find_matching_chars (snd polovici) (fst polovici)) + aux ostalo
    in
    string_of_int (aux lines)

  let naloga2 data _part1 =
    let lines = List.lines data in
    let triplets = split_into_triplets lines in
    let rec aux = function
      | [] -> 0
      | (a, b, c) :: ostalo -> (find_matching_3 (explode a) (explode b) (explode c)) + aux ostalo
      | _ -> failwith "ne"
    in
    string_of_int (aux triplets)
end
