type espaco =
  | Metrico of (float * float) list
  | Topologico of (float * float) list

type set = 
  | Finite of (float * float) list
  | Infinite of (float * float) list
  | Empty
  | Equal of (float * float) list
  | Equivalent of (float * float) list
  | Subset of (float * float) list
  | Superset of (float * float) list
  | Universal of (float * float) list
  | PowerSet of (float * float) list
  | Disjoint of (float * float) list
  | Overlap of (float * float) list
  | Open of (float * float) list
  | Closed of (float * float) list

let dist_fn (x1, y1) (x2, y2) =
  sqrt (((x2 -. x1) ** 2. +. (y2 -. y1) ** 2.) : float)

(* Axioms for metric spaces *)
(* Use nested List.for_all to compare every p1 to every p2 *)
let is_zero_or_positive dist_fn points =
  List.for_all (fun p1 ->
    List.for_all (fun p2 -> dist_fn p1 p2 >= 0.) points
  ) points

let epsilon = 1e-9
let non_degeneracy dist_fn points =
  List.for_all (fun p1 ->
    List.for_all (fun p2 -> (abs_float (dist_fn p1 p2) < epsilon) = (p1 = p2)) points
  ) points

let symmetry dist_fn points =
  List.for_all (fun p1 ->
    List.for_all (fun p2 -> dist_fn p1 p2 = dist_fn p2 p1) points
  ) points

(** Epsilon for floating-point comparison / Error tolerance *)
let epsilon = 1e-9

(* Your triangle_inequality already uses three nested loops,
   but make sure the first one is just 'p1' not '(p1, p2)' *)
let triangle_inequality dist_fn points =
  List.for_all (fun p1 ->
    List.for_all (fun p2 ->
      List.for_all (fun p3 ->
        dist_fn p1 p3 <= (dist_fn p1 p2 +. dist_fn p2 p3) +. epsilon
      ) points
    ) points
  ) points

(** Check if a set is open in a topological space *)
let is_open_set points topoloy = function
  | Open pts -> List.for_all (fun p -> List.mem p points) pts
  | _ -> false

(* Each axiom function must take the data and pass it to the next *)
let is_metrico  = function
  | Metrico points ->
    is_zero_or_positive dist_fn points &&
    non_degeneracy dist_fn points &&
    symmetry dist_fn points &&
    triangle_inequality dist_fn points
  | _ -> false

let is_topologico  = function
  | Topologico points ->
    is_zero_or_positive dist_fn points &&
    non_degeneracy dist_fn points &&
    symmetry dist_fn points &&
    triangle_inequality dist_fn points
  | _ -> false

let is_hausdorff = function
  | Open points -> is_open_set points (Open points)
  | _ -> false

(* Example usage *)
(* 1. Define a list of points (a triangle in 2D space) *)
let my_points = [(0.0, 0.0); (4.5, 8.0); (0.0, 4.0)]

(* 2. Wrap them in the Metrico constructor *)
let my_space = Metrico my_points

(* 3. Run the check *)
let () =
  if is_metrico my_space then
    print_endline "The space is a valid Metric Space!"
else if is_topologico my_space then
    print_endline "The space is a valid Topological Space!"
  else
    print_endline "Axiom violation detected."
