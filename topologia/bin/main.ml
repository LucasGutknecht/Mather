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

  (**
    Disjoint set are collections of elements that have no common members, meaning that their intersection is an empty set.

    To achieve this in Ocaml, we define a function which takes two sets as input. Mathching them, first, we take the
    list of points from each set and match them with pertinent stablished types as input '|'/'or' operator.

    Then, we use List.for_all to check if every point in the first set does not exist in the second set.
      - The function List.for_all takes an anonymous function that checks if a point p1 from the first set is not found in the second set (using List.exists).
      - If all points in the first set satisfy this condition, then the function returns true, indicating that the sets are disjoint. If any point in the first set is found in the second set, the function returns false, indicating that the sets are not disjoint.
    If either set is empty, we can immediately conclude that they are disjoint, since an empty set has no elements to share with any other set.
     - In this case, we check if either set is Empty and return true if so.
     - If the sets are of unsupported types for disjoint check, we raise an exception to indicate that the operation cannot be performed.

     Why do we need to check if sets are disjoint?
      - In topology, the concept of disjoint sets is important for defining separation axioms, such as the T2 (Hausdorff) axiom, which states that any two distinct points can be separated by disjoint open sets.
      - In metric spaces, disjoint sets can also be relevant for understanding the structure of the space and how points relate to each other.
  *)
let are_disjoint_set set1 set2 =
  match (set1, set2) with
  | ((Finite pts1 | Open pts1 | Closed pts1 | Disjoint pts1), (Finite pts2 | Open pts2 | Closed pts2 | Disjoint  pts2)) ->
    List.for_all (fun p1 -> not (List.exists (fun p2 -> p1 = p2) pts2)) pts1
  | (Empty, _) | (_, Empty) -> true
  | _ -> failwith "Unsupported set types for disjoint check"

(**
    This is a distance function that calculates the Euclidean distance between two points in 2D space.

    Euclidean distance means the straight-line distance between two points in a 2D plane. It is calculated using the formula:
      distance = sqrt((x2 - x1)^2 + (y2 - y1)^2). The name of the formula is the Pythagorean theorem, which states that in a right triangle, the square of the length of the hypotenuse (the side opposite the right angle) is equal to the sum of the squares of the lengths of the other two sides.

      Why do we need a distance function?
        - In metric spaces, the distance function is a fundamental component that defines the structure of the space. It allows us to measure how far apart points are from each other, which is essential for checking the axioms of metric spaces (like non-negativity, identity of indiscernibles, symmetry, and triangle inequality).
        - In topological spaces, while we do not necessarily need a distance function to define the topology, having one can help us understand the properties of the space and how points relate to each other in terms of proximity.
*)
let dist_fn (x1, y1) (x2, y2) =
  sqrt (((x2 -. x1) ** 2. +. (y2 -. y1) ** 2.) : float)

(* Axioms for metric spaces *)

(**
  Here we define the axioms for metric spaces. Each axiom is implemented as a function that checks a specific property of the distance function and the points in the space.

  Why do we need to check these axioms?
    - The axioms of metric spaces are fundamental properties that must hold for a space to be considered a metric space. They ensure that the distance function behaves in a way that is consistent with our intuitive understanding of distance.
    - By checking these axioms, we can determine whether a given set of points and a distance function define a valid metric space.

  Restrictions of these axioms:
    - These axioms are specific to metric spaces and may not apply to other types of spaces (like topological spaces). For example, in a topological space, we do not require a distance function, and the properties we check would be different (like separation axioms).
*)

(**
  Here we define the axiom of is zero or positive, which states that the distance between any two points must be greater than or equal to zero.

  As parameters it receives the distance function and the list of points in the space. It uses List.for_all to check if every pair of points (p1, p2) satisfies the condition that the distance between them is greater than or equal to zero.

  Why do we need to check this axiom?
    - This is a fundamental property of distance functions in metric spaces. It ensures that the concept of distance is meaningful and consistent with our intuitive understanding of how distance should behave.
    - If this axiom is violated, it would imply that there are points in the space that are "negatively distant" from each other, which does not make sense in the context of a metric space.

  Restrictions of this axiom:
    - This axiom is specific to metric spaces and may not apply to other types of spaces (like topological spaces). In a topological space, we do not require a distance function, so this axiom would not be relevant.
*)
let is_zero_or_positive dist_fn points =
  List.for_all (fun p1 ->
    List.for_all (fun p2 -> dist_fn p1 p2 >= 0.) points
  ) points

(**
  Here we define the axiom of non-degeneracy, which states that the distance between two points is zero if and only if the points are the same.

  As parameters it receives the distance function and the list of points in the space. It uses List.for_all to check if every pair of points (p1, p2) satisfies the condition that the distance between them is less than a small epsilon value (indicating they are effectively the same point) if and only if p1 and p2 are actually the same point.

  The epsilon value is used to account for floating-point precision issues, allowing us to treat points that are very close together as the same point.

  Why do we need to check this axiom?
    - This axiom ensures that the distance function can distinguish between different points in the space. If this axiom is violated, it would imply that there are distinct points in the space that are considered to be at zero distance from each other, which would undermine the structure of a metric space.

  Restrictions of this axiom:
    - This axiom is specific to metric spaces and may not apply to other types of spaces (like topological spaces). In a topological space, we do not require a distance function, so this axiom would not be relevant.
*)
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
(* 1. Define a list of points (a triangle in 2D space); add invalid points. example characters *)
let my_points = [(0.0, 0.0); (4.5, 8.0); (0.0, 4.0); (1.0, nan)] (* This point is added to potentially violate the axioms *)
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
