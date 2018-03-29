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
= struct
  type t = Value.Set.t Key.Map.t

  let mem x y t =
    match Key.Map.find_opt x t with
    | Some elts -> Value.Set.mem y elts
    | None -> false

  let add x y t =
    Key.Map.update x (function
      | Some ys -> Some (Value.Set.add y ys)
      | None -> Some (Value.Set.singleton y)
    ) t

  let add_set x ys map =
    Key.Map.update x (function
      | Some elts -> Some (Value.Set.union ys elts)
      | None -> Some ys
    ) map

  let find x t =
    match Key.Map.find_opt x t with
    | Some ys -> ys
    | None -> Value.Set.empty

  exception Duplicate_key of Key.t

  let union_disjoint_exn t1 t2 =
    Key.Map.union (fun key _ _ -> raise (Duplicate_key key)) t1 t2

  let print fmt t = Key.Map.print Value.Set.print fmt t
  let to_string t = Format.asprintf "%a" print t

  module Invertible = struct
    type nonrec t = { forward : t; backward : Key.Set.t Value.Map.t }

    let of_relation forward =
      let backward =
        Key.Map.fold (fun x ys acc ->
          Value.Set.fold (fun y acc ->
            Value.Map.update y (function
              | Some xs -> Some (Key.Set.add x xs)
              | None -> Some (Key.Set.singleton x)
            ) acc
          ) ys acc
        ) forward Value.Map.empty
      in
      { forward; backward }

    let forward { forward; _ } = forward
    let backward { backward; _ } = backward

    let print ppf t =
      Format.fprintf ppf "@[<v>forward:@,%a@,backward:@,%a@]"
        (Key.Map.print Value.Set.print) t.forward
        (Value.Map.print Key.Set.print) t.backward
    let to_string t = Format.asprintf "%a" print t
  end

  let inverse t = Invertible.backward (Invertible.of_relation t)
end
