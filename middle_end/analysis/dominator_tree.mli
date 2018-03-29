type t

(* CR lmaurer: Generalize from ilambda. *)
val of_function_declaration : Ilambda.function_declaration -> t

val root_node : t -> Continuation.t
val of_successor_map
  :  root_node:Continuation.t
  -> Continuation.Set.t Continuation.Map.t -> t

val parent : t -> Continuation.t -> Continuation.t option
val children : t -> Continuation.t -> Continuation.Set.t
val ancestors : t -> Continuation.t -> Continuation.Set.t
val descendants : t -> Continuation.t -> Continuation.Set.t
val predecessors : t -> Continuation.t -> Continuation.Set.t
val successors : t -> Continuation.t -> Continuation.Set.t

val iter : (Continuation.t -> Continuation.Set.t -> unit) -> t -> unit

val dominance_frontier : t -> Continuation.t -> Continuation.Set.t
(* Too slow for real use *)
val iterated_dominance_frontier : t -> Continuation.Set.t -> Continuation.Set.t

val print : Format.formatter -> t -> unit
val print_dominance_frontiers : Format.formatter -> t -> unit
