open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct

  let ranges lines =
    let rec aux = function
      | [] -> []
      | prvi :: ostalo ->
        let deli = String.split_on_char ',' prvi in
        match deli with
          | [] -> aux ostalo
          | [""] -> aux ostalo
          | s1 :: s2 :: [] -> (List.map int_of_string (String.split_on_char '-' s1), List.map int_of_string (String.split_on_char '-' s2)) :: aux ostalo
          | _ -> failwith "ne ranges"
    in
    aux lines

  let naloga1 data =
    let lines = List.lines data in
    let rng = ranges lines in

    let rec check = function
      | [] -> 0
      | ([a; b], [c; d]) :: ostalo ->
        if c <= a && b <= d || a <= c && d <= b then
          1 + check ostalo
        else
          check ostalo
      | _ -> failwith "ne naloga1"
    in
    string_of_int (check rng)

  let naloga2 data _part1 =
    let lines = List.lines data in
    let rng = ranges lines in

    let rec check = function
      | [] -> 0
      | ([a; b], [c; d]) :: ostalo ->
        if c <= b && b <= d || c <= a && a <= d || a <= c && d <= b then
          1 + check ostalo
        else
          check ostalo
      | _ -> failwith "ne naloga2"
    in
    string_of_int (check rng)
end
