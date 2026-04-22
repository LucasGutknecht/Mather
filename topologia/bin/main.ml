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

let are_disjoint_set set1 set2 =
  match (set1, set2) with
  | ((Finite pts1 | Open pts1 | Closed pts1 | Disjoint pts1), (Finite pts2 | Open pts2 | Closed pts2 | Disjoint  pts2)) ->
    List.for_all (fun p1 -> not (List.exists (fun p2 -> p1 = p2) pts2)) pts1
  | (Empty, _) | (_, Empty) -> true
  | _ -> failwith "Unsupported set types for disjoint check" 

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

let cointain_point p = function
  | (Finite pts | Open pts | Closed pts) -> List.exists (fun pt -> dist_fn pt p < epsilon) pts
  | Empty -> false
  | _ -> failwith "Unsupported set type for point containment check"

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

let check_t2_property point topology = 
  let open_points = match topology with
    | Open pts -> pts
    | _ -> failwith "Unsupported topology type for T2 check"
  in
  List.exists (fun p -> p <> point && List.mem p open_points) open_points

(** Check if two points can be separated by disjoint open sets.
   'topology' should be a list of 'set' variants (the open sets of the space).
**)
let can_be_separated_t2 p1 p2 topology =
  List.exists (fun u ->
    List.exists (fun v ->
      cointain_point p1 u && 
      cointain_point p2 v && 
      are_disjoint_set u v
    ) topology
  ) topology

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

(** Check if the entire space satisfies the Hausdorff (T2) axiom.
    points: all points in the space
    topology: the list of all sets defined as 'Open'
**)
let is_space_hausdorff points topology =
  List.for_all (fun p1 ->
    List.for_all (fun p2 ->
      if p1 = p2 then true 
      else can_be_separated_t2 p1 p2 topology
    ) points
  ) points

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
