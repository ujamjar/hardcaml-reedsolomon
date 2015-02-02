open HardCaml

module Make(B : Comb.S)(P : Reedsolomon.Galois.Table.Params) : sig

  module G : Reedsolomon.Galois.Table.Ops with type t = int

  type t

  val n_elems : int
  val bits : int
  val alpha : t
  val zero : t
  val one : t

  val log : t -> t
  val antilog : t -> t
  
  val inv : t -> t

  val (+:) : t -> t -> t
  val (-:) : t -> t -> t

  val ( *: ) : t -> t -> t
  val ( /: ) : t -> t -> t

  val ( **: ) : t -> t -> t

  val to_string : t -> string

  val rom : (int -> G.t) -> B.t -> t

  val cpow : t -> int -> t
  val cmul : ?rom:bool -> G.t -> t -> t

end
  with type t = B.t
