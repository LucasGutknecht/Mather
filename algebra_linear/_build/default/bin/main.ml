type matrix =
  | Square of int * (int list) list
  | Rectangular of int * int * (int list) list
  | Vector of int * (int list)
  | Diagonal of int * (int list)
  | Identity of int * (int list)
  | Zero of int * int * (int list) list


(* Axioms *)
let get_diagonal m =
  List.mapi (fun i row -> List.nth row i) m

