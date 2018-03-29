module type S = sig
  module Key : Identifiable.S
  module Value : Identifiable.S

  type t = Value.Set.t Key.Map.t

  val mem : Key.t -> Value.t -> t -> bool
  val add : Key.t -> Value.t -> t -> t
  val add_set : Key.t -> Value.Set.t -> t -> t
  val find : Key.t -> t -> Value.Set.t

  exception Duplicate_key of Key.t
  val union_disjoint_exn : t -> t -> t

  val inverse : t -> Key.Set.t Value.Map.t

  val print : Format.formatter -> t -> unit
  val to_string : t -> string

  module Invertible : sig
    type t = private { forward : Value.Set.t Key.Map.t; backward : Key.Set.t Value.Map.t }

    val of_relation : Value.Set.t Key.Map.t -> t
    val forward : t -> Value.Set.t Key.Map.t
    val backward : t -> Key.Set.t Value.Map.t

    val print : Format.formatter -> t -> unit
    val to_string : t -> string
  end
end

module Make (Key : Identifiable.S) (Value : Identifiable.S)
  : S
    with module Key := Key
    with module Value := Value
