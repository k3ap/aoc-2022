open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let rec elfsums acc partsum = function
    | [] -> acc
    | prvi :: ostalo ->
      if (0 = String.length prvi) then
        elfsums (partsum :: acc) 0 ostalo
      else
        elfsums acc (partsum + int_of_string prvi) ostalo

  let naloga1 data =
    let lines = List.lines data in
    string_of_int (List.fold_left max min_int (elfsums [] 0 lines))

  let naloga2 data _part1 =
    let lines = List.lines data in
    let zajci = List.sort (fun a b -> - compare a b) (elfsums [] 0 lines) in
    match zajci with
    | a :: b :: c :: ostalo -> string_of_int (a + b + c)
    | _ -> failwith "ne"
end
