type matrix =
  | Square of int * (int list) list
  | Rectangular of int * int * (int list) list
  | Vector of int * (int list)
  | Diagonal of int * (int list)
  | Identity of int * (int list)
  | Zero of int * int * (int list) list

(* Helpers *)
let add m1 m2 =
  match (m1, m2) with
  | (Zero _, any) -> any  (* Axiom: 0 + A = A *)
  | (any, Zero _) -> any  (* Axiom: A + 0 = A *)
  | (Identity (n1, _), Identity (n2, _)) when n1 = n2 ->
      (* Addition of two identities is just a diagonal matrix of 2s *)
      Diagonal (n1, List.init n1 (fun _ -> 2))
  | _ ->
      (* Fallback: Standard element-wise addition *)
      (* You'll implement the list-walking logic here *)
      Zero (0, 0, [])

(* Structure Axioms *)

let square_or_rectangular m =
  if List.length m = List.length (List.hd m) then
    Square (List.length m, m)
  else
    Rectangular (List.length m, List.length (List.hd m), m)

let identify_vector m =
  let row_count = List.length m in
  let col_count = match m with [] -> 0 | h :: _ -> List.length h in

  if row_count = 1 then
    (* It's a row vector: grab the only row in the list *)
    Vector (col_count, List.hd m)
  else if col_count = 1 then
    (* It's a column vector: flatten the nested 1-element lists *)
    Vector (row_count, List.flatten m)
  else
    (* Fallback if it's not a vector *)
    if row_count = col_count then Square (row_count, m)
    else Rectangular (row_count, col_count, m)

let identify_zero m =
  let rows = List.length m in
  let cols = match m with [] -> 0 | h :: _ -> List.length h in

  (* The Zero Axiom Check *)
  let all_zero = List.for_all (fun row -> List.for_all (fun x -> x = 0) row) m in

  if all_zero then
    Zero (rows, cols, m)
  else
    (* Fallback to Square or Rectangular if not all zeros *)
    if rows = cols then Square (rows, m)
    else Rectangular (rows, cols, m)

(* Note the performance is O(nˆ2) due to List.nth which is O(n) inside List.mapi which is also O(n). Given the O(n * n) -> O(n^2) complexity, this is not efficient for large matrices. *)
let identify_diagonal m =
  let n = List.length m in
  let cols = List.length (List.hd m) in

  (* Axiom Check: Is it square and are off-diagonals zero? *)
  let is_diag_structure =
    n = cols &&
    List.mapi (fun i row ->
      List.mapi (fun j value -> i = j || value = 0) row
    ) m |> List.for_all (List.for_all (fun x -> x))
  in

  if is_diag_structure then
    let diag_list = List.mapi (fun i row -> List.nth row i) m in
    Diagonal (n, diag_list)
  else
    (* If it fails the axiom, return it as a standard Square or Rectangular *)
    if n = cols then Square (n, m) else Rectangular (n, cols, m)

(* Note of performance: The is_identity function has a complexity of O(n^2) due to the nested List.mapi calls. *)
let identify_identity m =
  match square_or_rectangular m with
  | Square (n, matrix_data) ->
      (* Check if it satisfies the Identity Axiom *)
      let is_id =
        List.mapi (fun i row ->
          List.mapi (fun j value ->
            if i = j then value = 1 else value = 0
          ) row
        ) matrix_data
        |> List.for_all (List.for_all (fun x -> x))
      in
      if is_id then
        Identity (n, List.mapi (fun i row -> List.nth row i) matrix_data)
      else
        Square (n, matrix_data)

  | other -> other (* Returns Rectangular or whatever it was *)

let identify m =
  let (rows, cols) = (List.length m, match m with [] -> 0 | h::_ -> List.length h) in

  (* 1. Check for Zero first (it can be any shape) *)
  match identify_zero m with
  | Zero (r, c, data) -> Zero (r, c, data)
  | _ ->
      (* 2. Check for Vector *)
      match identify_vector m with
      | Vector (n, data) -> Vector (n, data)
      | _ ->
          (* 3. Check for Square-specific types *)
          if rows = cols then
            match identify_identity m with
            | Identity (n, d) -> Identity (n, d)
            | _ ->
                match identify_diagonal m with
                | Diagonal (n, d) -> Diagonal (n, d)
                | _ -> Square (rows, m)

          (* 4. Final Catch-all *)
          else
            Rectangular (rows, cols, m)

(* Operations axioms *)
let commutative_addition m1 m2 =
  (* This is a placeholder for the actual implementation of matrix addition *)
  (* You would need to implement the logic to add two matrices and check if m1 + m2 = m2 + m1 *)
  add m1 m2 = add m2 m1

let associative_addition m1 m2 m3 =
  (* This is a placeholder for the actual implementation of matrix addition *)
  (* You would need to implement the logic to add three matrices and check if (m1 + m2) + m3 = m1 + (m2 + m3) *)
  true

let non_commutative_multiplication m1 m2 =
  (* This is a placeholder for the actual implementation of matrix multiplication *)
  (* You would need to implement the logic to multiply two matrices and check if m1 * m2 != m2 * m1 *)
  true

let distributive_multiplication_over_addition m1 m2 m3 =
  (* This is a placeholder for the actual implementation of matrix multiplication and addition *)
  (* You would need to implement the logic to check if m1 * (m2 + m3) = m1 * m2 + m1 * m3 *)
  true
