type matrix =
  | Square of int * (int list) list
  | Rectangular of int * int * (int list) list
  | Vector of int * (int list)
  | Diagonal of int * (int list)
  | Identity of int * (int list)
  | Zero of int * int * (int list) list


(* Axioms *)
(* Note the performance is O(nˆ2) due to List.nth which is O(n) inside List.mapi which is also O(n). Given the O(n * n) -> O(n^2) complexity, this is not efficient for large matrices. *)
let get_diagonal m =
  List.mapi (fun i row -> List.nth row i) m

let is_zero m =
  List.for_all (fun row -> List.for_all (fun x -> x = 0) row) m
