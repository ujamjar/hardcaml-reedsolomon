(** hardware reed-solomon codec *)
open HardCaml

module type S = sig
  open Signal.Comb

  module Encoder : sig
    val encoder : enable:t -> ctrl:t -> d_in:t -> t
    module I : interface enable ctrl d end
    module O : interface q end
    val f : t I.t -> t O.t 
  end

  module type N = sig val n : int end

  module Decoder(N : N) : sig

    (** sequential calculation of syndromes *)
    val syndrome : root:int -> enable:t -> first:t -> x:t -> t
    val syndromes : enable:t -> first:t -> x:t -> t array

    (** parallel calculation of syndromes.
        
        Calculates [x{n-1}.r{n-1}^(n-1) + ... + x{0}.r{0}^0] where [x{i}] is an
        input symbol and [r{i}] is a root of the generator polynomial.  The symbols [x{i}]
        should be provided from the highest index to the lowest.
      
        The [Rp.n] symbols which comprise the received codeword are passed into the circuit
        via the [x] array at a rate of [N.n] symbols per cycle.

        The [first] and [last] cycles of data must be strobed (active high, in extremis they
        can be on the same cycle).
      
        In most cases the parallel inputs will not evenly divide the codeword size [Rp.n].
        For example with [Rp.n=15] and [N.n=2] there will be 1 extra symbol.  If we
        logically pack 0's at the start of the codeword (corresponding to the highest power)
        then all is OK.  

        Alternatively we could add 0's at the end of the codeword (this might be easier
        for integration with a system and is required by the correction logic in the
        provided decoder architecture).  The output syndrome values become scaled by
        [(root i)^(extra-zeros)].  The [scale] parameter can be used to remove this 
        scale factor by setting it to [extra-zeros].

     *)
    val psyndromes : scale:int -> clear:t -> enable:t -> first:t -> last:t -> x:t array -> 
      t * t array

    module PSyndromes : sig
      module I : interface clear enable first last x{| |} end
      module O : interface valid syndromes{| |} end
      val f : scale:int -> t I.t -> t O.t
    end

    (* berlekamp massey *)
    val iBM : clear:t -> enable:t -> start:t -> syndromes:t list -> t list
    val riBM : clear:t -> enable:t -> start:t -> syndromes:t list -> t list
    val rriBM : clear:t -> enable:t -> first:t -> last:t -> syndromes:t list -> t list * t list

    module RiBM : sig
      module I : interface clear enable first last syndromes{| |} end
      module O : interface w{| |} l{| |} end
      val f : t I.t -> t O.t
    end

    (* chien search *)
    val chien : clear:t -> enable:t -> lambda:t list -> t
    val pchien : p:int -> clear:t -> enable:t -> start:t -> lambda:t array -> 
      t array * t array * t array * t array

    module PChien : sig
      module I : interface clear enable start lambda{| |}  end
      module O : interface eval{| |} eloc{| |} evld{| |} eerr{| |} end
      val f : t I.t -> t O.t
    end

    (* forney *)
    val forney_serial : clear:t -> enable:t -> start:t -> store:t -> ctrl:t -> tap:t -> x:t -> t

    module Forney_serial : sig
      module I : interface clear enable start store ctrl tap x end
      module O : interface e end
      val f : t I.t -> t O.t
    end

    val forney : clear:t -> enable:t -> vld:t -> err:t -> 
      v:t array -> l:t array -> x:t -> t * t * t
    module Forney : sig
      module I : interface clear enable vld err v{| |} l{| |} x end
      module O : interface emag frdy ferr end
      val f : t I.t -> t O.t
    end

    module PForney : sig
      module I : interface clear enable vld{| |} err{| |} v{| |} l{| |} x{| |} end
      module O : interface emag{| |} frdy{| |} ferr{| |} end
      val f : t I.t -> t O.t
    end

    module Decode : sig
      module I : interface clear enable load first last x{| |} end
      module O : interface 
        (syn : PSyndromes.O)
        (bm : RiBM.O)
        (ch : PChien.O)
        (fy : PForney.O)
        corrected{| |}
        ordy
      end
      val f : t I.t -> t O.t
    end

  end
end


module Make
  (Gp : Reedsolomon.Galois.Table.Params)
  (Rp : Reedsolomon.Codec.RsParams) : S

