(**
    This Ocaml code defines the simplest framework for checking if a given 
      set of points is a valid metric space or topological space, based on axioms.

    Some heuristic decisions made in this implementation:
      - We are treating the space as a collection of points in 2D, which is a common way to visualize metric and topological spaces.
      - The distance function is defined as the Euclidean distance between points, which is a standard choice for metric spaces.
      - The axioms for metric spaces (non-negativity, identity of indiscernibles, symmetry, triangle inequality) are implemented as separate functions that check each axiom against the list of points.
      - For topological spaces, we are only checking the T2 (Hausdorff) separation axiom, which states that any two distinct points can be separated by disjoint open sets. This is a fundamental property of many topological spaces.
      - The code is structured to allow for easy extension to more complex types of spaces and additional axioms if needed.
*)

(**
  Here we define the types of spaces. Each type is a list of points(Type float): (x, y).

  Why would nees a pair of points?
    - To check the axioms of metric spaces, we need to compute distances between pairs of points.
    - For topological spaces, we also need to check properties that involve pairs of points (like separation axioms).
    Are all spaces defined as pairs of points? 
      - No, but for the sake of this example, we are treating the space as a collection of points in 2D.
      - A space doesnt have to be a grid of coordinates. It can be a space of functions, shapes, sequences... even symbols.
      - What will be different is how we define the axioms in the face of the content of the space. 
      - For example, in a space of functions, the distance might be defined as the maximum difference between function values, 
        rather than Euclidean distance between points.
      Example of spaces with not pairs of points:
        - A topological space could be defined by a set of open sets, which are subsets of the points, rather than just pairs of points.

  Restrictions of types:
    - This is a very simplified model. Real metric and topological spaces can be much more complex, and the axioms can involve more than just pairs of points. 
      - To achieve this we must genericize the types(Polimorphism) to allow for more complex structures, such as sets of functions, shapes, or even abstract elements.
    - The distance function is fixed as Euclidean distance, which may not be appropriate for all metric spaces.
    - The topological space is not fully defined;
*)
type espaco =
  | Metrico of (float * float) list
  | Topologico of (float * float) list

(** 
  Here we define the types of sets. Each set is a list of points (float * float) representing coordinates in 2D space. 
  We have different types of sets to represent various properties and operations on sets, such as finite, infinite, empty, equal, equivalent, subset, superset, universal, power set, disjoint, overlap, open, and closed sets.

  Why do we need different types of sets?
    - To represent different properties and operations on sets that are relevant in metric and topological spaces.
    - For example, we might want to check if two sets are disjoint or if one set is a subset of another. 
      - These operations require us to have a way to represent and manipulate different types of sets.

  Restrictions of types:
    - This is a very simplified model. Real metric and topological spaces can involve much more complex sets and operations.
    - The types defined here are not exhaustive; there are many other types of sets and operations that could be relevant in different contexts.
*)  
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

(* Epsilon for floating-point comparison / Error tolerance *)
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
