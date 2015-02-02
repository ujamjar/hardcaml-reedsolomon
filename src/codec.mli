(** hardware reed-solomon codec *)
open HardCaml

module type S = sig
    open Signal.Comb

    module Encoder : sig
        val encoder : enable:t -> ctrl:t -> d_in:t -> t
        module I : interface enable ctrl d end
        module O : interface q end
        val encoder_if : t I.t -> t O.t 

    end

    module Decoder : sig
        module type N = sig val n : int end
        (* sequential calculation of syndromes *)
        val syndrome : root:int -> enable:t -> first:t -> x:t -> t
        val syndromes : enable:t -> first:t -> x:t -> t array

        (* parallel calculation of syndromes *)
        val psyndromes : clear:t -> enable:t -> first:t -> last:t -> x:t array -> t * t array

        module PSyndromes(N : N) : sig
          module I : interface clear enable first last x{| |} end
          module O : interface valid syndromes{| |} end
          val f : t I.t -> t O.t
        end

        (* berlekamp massey *)
        val iBM : clear:t -> enable:t -> start:t -> syndromes:t list -> t list
        val riBM : clear:t -> enable:t -> start:t -> syndromes:t list -> t list
        val rriBM : clear:t -> enable:t -> first:t -> last:t -> syndromes:t list -> t list * t list

        module RiBM : sig
          module I : interface clear enable first last syndromes{ } end
          module O : interface w{ } l{ } end
          val f : t I.t -> t O.t
        end

        (* chien search *)
        val chien : clear:t -> enable:t -> lambda:t list -> t
        val pchien : p:int -> clear:t -> enable:t -> start:t -> lambda:t array -> t array

        module PChien(N:N) : sig
          module I : interface clear enable start lambda{| |}  end
          module O : interface error_locs{| |} end
          val f : t I.t -> t O.t
        end

        (* forney *)
        val forney : clear:t -> enable:t -> start:t -> store:t -> ctrl:t -> tap:t -> x:t -> t
        
        module Forney : sig
          module I : interface clear enable start store ctrl tap x end
          module O : interface e end
          val f : t I.t -> t O.t
        end
    end
end


module Make(Gp : Reedsolomon.Galois.Table.Params)
           (Rp : Reedsolomon.Codec.RsParams) : S

