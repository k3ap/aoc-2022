open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let naloga1 data =
    let lines = List.map (String.split_on_char ' ') (List.lines data) in
    let rec sum_scores = function
      | [] -> 0
      | [a;b] :: ostalo ->
        begin
        match (a, b) with
        | ("A", "X") -> 1 + 3 + sum_scores ostalo
        | ("A", "Y") -> 2 + 6 + sum_scores ostalo
        | ("A", "Z") -> 3 + 0 + sum_scores ostalo
        | ("B", "X") -> 1 + 0 + sum_scores ostalo
        | ("B", "Y") -> 2 + 3 + sum_scores ostalo
        | ("B", "Z") -> 3 + 6 + sum_scores ostalo
        | ("C", "X") -> 1 + 6 + sum_scores ostalo
        | ("C", "Y") -> 2 + 0 + sum_scores ostalo
        | ("C", "Z") -> 3 + 3 + sum_scores ostalo
        | _ -> failwith "Ne1"
      end
      | prvi :: ostalo -> sum_scores ostalo
      | _ -> failwith "Ne2"
    in
    string_of_int (sum_scores lines)

  let naloga2 data _part1 =
    let lines = List.map (String.split_on_char ' ') (List.lines data) in
    let rec sum_scores = function
      | [] -> 0
      | [a;b] :: ostalo ->
        begin
          match (a, b) with
          | ("A", "X") -> 0 + 3 + sum_scores ostalo
          | ("A", "Y") -> 3 + 1 + sum_scores ostalo
          | ("A", "Z") -> 6 + 2 + sum_scores ostalo
          | ("B", "X") -> 0 + 1 + sum_scores ostalo
          | ("B", "Y") -> 3 + 2 + sum_scores ostalo
          | ("B", "Z") -> 6 + 3 + sum_scores ostalo
          | ("C", "X") -> 0 + 2 + sum_scores ostalo
          | ("C", "Y") -> 3 + 3 + sum_scores ostalo
          | ("C", "Z") -> 6 + 1 + sum_scores ostalo
          | _ -> failwith "Ne1"
        end
      | prvi :: ostalo -> sum_scores ostalo
      | _ -> failwith "Ne2"
    in
    string_of_int (sum_scores lines)
end
