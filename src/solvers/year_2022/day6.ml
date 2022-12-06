open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct

  let rec is_distinct s idx len =
    if len = 1 then true
    else
      let rec check i =
        if i = idx then true
        else if s.[i] = s.[idx] then false
        else check (i-1)
      in
      check (idx+len-1) && is_distinct s (idx+1) (len-1)

  let rec find_marker s idx len =
    if not (is_distinct s idx len) then
      find_marker s (idx+1) len
    else
      idx+len

  let naloga1 data =
    string_of_int (find_marker data 0 4)

  let naloga2 data _part1 =
    string_of_int (find_marker data 0 14)
end
