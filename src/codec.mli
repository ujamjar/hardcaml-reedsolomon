(** hardware reed-solomon codec *)
open HardCaml

module type S = sig
  open Signal.Comb

  module Encoder : sig
    val encoder : enable:t -> ctrl:t -> d_in:t -> t
    module I : sig
      type 'a t = { enable : 'a; ctrl : 'a; d : 'a; }[@@deriving hardcaml]
    end
    module O : sig
      type 'a t = { q : 'a; }[@@deriving hardcaml]
    end
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
      module I : sig
        type 'a t = { clear : 'a; enable : 'a; first : 'a; last : 'a; x : 'a array;  }[@@deriving hardcaml]
      end
      module O : sig
        type 'a t = { valid : 'a; syndromes : 'a array;  }[@@deriving hardcaml]
      end
      val f : scale:int -> t I.t -> t O.t
    end

    (* berlekamp massey *)
    val iBM : clear:t -> enable:t -> start:t -> syndromes:t list -> t list
    val riBM : clear:t -> enable:t -> start:t -> syndromes:t list -> t list
    val rriBM : clear:t -> enable:t -> first:t -> last:t -> syndromes:t list -> t list * t list

    module RiBM : sig
      module I : sig
        type 'a t = { clear : 'a; enable : 'a; first : 'a; last : 'a; syndromes : 'a array;  }[@@deriving hardcaml]
      end
      module O : sig
        type 'a t = { w : 'a array;  l : 'a array;  }[@@deriving hardcaml]
      end
      val f : t I.t -> t O.t
    end

    (* chien search *)
    val chien : clear:t -> enable:t -> lambda:t list -> t
    val pchien : p:int -> clear:t -> enable:t -> start:t -> lambda:t array -> 
      t array * t array * t array * t array

    module PChien : sig
      module I : sig
        type 'a t = { clear : 'a; enable : 'a; start : 'a; lambda : 'a array;   }[@@deriving hardcaml]
      end
      module O : sig
        type 'a t = { eval : 'a array;  eloc : 'a array;  evld : 'a array;  eerr : 'a array;  }[@@deriving hardcaml]
      end
      val f : t I.t -> t O.t
    end

    (* forney *)
    val forney_serial : clear:t -> enable:t -> start:t -> store:t -> ctrl:t -> tap:t -> x:t -> t

    module Forney_serial : sig
      module I : sig
        type 'a t = { clear : 'a; enable : 'a; start : 'a; store : 'a; ctrl : 'a; tap : 'a; x : 'a; }[@@deriving hardcaml]
      end
      module O : sig
        type 'a t = { e : 'a; }[@@deriving hardcaml]
      end
      val f : t I.t -> t O.t
    end

    val forney : clear:t -> enable:t -> vld:t -> err:t -> 
      v:t array -> l:t array -> x:t -> t * t * t
    module Forney : sig
      module I : sig
        type 'a t = { clear : 'a; enable : 'a; vld : 'a; err : 'a; v : 'a array;  l : 'a array;  x : 'a; }[@@deriving hardcaml]
      end
      module O : sig
        type 'a t = { emag : 'a; frdy : 'a; ferr : 'a; }[@@deriving hardcaml]
      end
      val f : t I.t -> t O.t
    end

    module PForney : sig
      module I : sig
        type 'a t = { clear : 'a; enable : 'a; vld : 'a array;  err : 'a array;  v : 'a array;  l : 'a array;  x : 'a array;  }[@@deriving hardcaml]
      end
      module O : sig
        type 'a t = { emag : 'a array;  frdy : 'a array;  ferr : 'a array;  }[@@deriving hardcaml]
      end
      val f : t I.t -> t O.t
    end

    module Decode : sig
      module I : sig
        type 'a t = { clear : 'a; enable : 'a; load : 'a; first : 'a; last : 'a; x : 'a array;  }[@@deriving hardcaml]
      end
      module O : sig
type 'a t = { 
        (*(syn : PSyndromes.O)
        (bm : RiBM.O)
        (ch : PChien.O)
        (fy : PForney.O)*)
        corrected : 'a array; 
        ordy : 'a;
        error_count : 'a;
}[@@deriving hardcaml]
      end
      val f : t I.t -> t O.t
    end

  end
end


module Make
  (Gp : Reedsolomon.Galois.Table.Params)
  (Rp : Reedsolomon.Codec.RsParams) : S

