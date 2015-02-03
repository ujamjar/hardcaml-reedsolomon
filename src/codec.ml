(* hardware implementation of reed-solomon codec *)

open HardCaml

(* register specification *)
module Seq = Signal.Make_seq(struct
    let reg_spec = Signal.Seq.r_sync
    let ram_spec = Signal.Seq.r_none
end)

module type S = sig
    open Signal.Comb

    module Encoder : sig
        val encoder : enable:t -> ctrl:t -> d_in:t -> t
        module I : interface enable ctrl d end
        module O : interface q end
        val f : t I.t -> t O.t 

    end

    module Decoder : sig
        module type N = sig val n : int end
        val syndrome : root:int -> enable:t -> first:t -> x:t -> t
        val syndromes : enable:t -> first:t -> x:t -> t array
        val psyndromes : clear:t -> enable:t -> first:t -> last:t -> x:t array -> t * t array
        module PSyndromes(N : N) : sig
          module I : interface clear enable first last x{| |} end
          module O : interface valid syndromes{| |} end
          val f : t I.t -> t O.t
        end

        val iBM : clear:t -> enable:t -> start:t -> syndromes:t list -> t list
        val riBM : clear:t -> enable:t -> start:t -> syndromes:t list -> t list
        val rriBM : clear:t -> enable:t -> first:t -> last:t -> syndromes:t list -> t list * t list
        module RiBM : sig
          module I : interface clear enable first last syndromes{ } end
          module O : interface w{ } l{ } end
          val f : t I.t -> t O.t
        end


        val chien : clear:t -> enable:t -> lambda:t list -> t
        val pchien : p:int -> clear:t -> enable:t -> start:t -> lambda:t array -> t array
        module PChien(N:N) : sig
          module I : interface clear enable start lambda{| |}  end
          module O : interface error_locs{| |} end
          val f : t I.t -> t O.t
        end

        val forney : clear:t -> enable:t -> start:t -> store:t -> ctrl:t -> tap:t -> x:t -> t
        module Forney : sig
          module I : interface clear enable start store ctrl tap x end
          module O : interface e end
          val f : t I.t -> t O.t
        end

    end
end

module Make(Gp : Reedsolomon.Galois.Table.Params)
           (Rp : Reedsolomon.Codec.RsParams) = struct

    module B = Signal.Comb
    module S = Signal.Seq

    module Gfh = Galois.Make(B)(Gp)
    module Gfs = Gfh.G
    module Rs = Reedsolomon.Codec.MakePoly(Gfs)(Rp)

    (* RS encoder *)

    module Encoder = struct

        let encoder ~enable ~ctrl ~d_in =
            let open Utils in
            let g = List.(Rs.generator |> Array.to_list |> rev |> tl |> rev) in
            let b = Gfh.bits in

            let reg r e d = B.(S.reg r e d -- "enc_state_reg") in

            let rec f d_in d_prev = function
                | [] -> []
                | cof::t -> 
                    let r = reg S.r_sync enable Gfh.( (cmul cof d_in) +: d_prev ) in
                    r :: f d_in r t
            in

            let w = B.wire b in
            let qs = f w (B.zero b) g in
            let q = List.(qs |> rev |> hd)  in
            let () = B.(w <== Gfh.(mux2 ctrl zero (d_in +: q))) in
            q

        (* with interface *)
        module I = interface enable[1] ctrl[1] d[Gfh.bits] end
        module O = interface q[Gfh.bits] end

        let f i = O.({ q = I.(encoder i.enable i.ctrl i.d) })

    end

    module Decoder = struct

        open B

        module type N = sig
            val n : int
        end
        module type F = sig
            module I : Interface.S
            module O : Interface.S
            val f : t I.t -> t O.t
        end

        let rec tree_depth n x =
            if x<=1 then 0 (* XXX I think *)
            else if x<=n then 1
            else 1 + tree_depth n ((x+n-1)/n) 

        (***********************************************************)
        (* syndrome calculation *)

        let syndrome ~root ~enable ~first ~x = 
            S.reg_fb S.r_sync enable Gfh.bits 
                (fun d -> 
                    let d = mux2 first Gfh.zero d in
                    Gfh.(cmul root d +: x))
        
        let syndromes ~enable ~first ~x = 
            Array.init (2*Rp.t) 
                (fun i -> syndrome (Rs.root i) enable first x)

        let psyndromes ~clear ~enable ~first ~last ~x = 
            let n = Array.length x in
            let n_tree = 2 in
            let eval c x = 
                let len = Array.length x in
                let cmul c i x = Gfh.cmul (Gfs.(c **: i)) x in
                let a = Array.mapi (fun i x -> cmul c (len-i-1) x) x in
                let a = Array.map (fun x -> Seq.reg ~c:clear ~e:enable x) a in
                let add_p a = 
                    Seq.reg ~c:clear ~e:enable (reduce Gfh.(+:) a)
                in
                tree n_tree add_p (Array.to_list a) 
            in
            let horner clear enable first c x = 
                Seq.reg_fb ~c:clear ~e:enable ~w:Gfh.bits
                    (fun d -> Gfh.(x +: cmul c (mux2 first zero d)))
            in
            let syndrome clear enable first root_n root x = 
                horner clear enable first root_n (eval root x)
            in
            let first = Seq.pipeline ~c:clear ~e:enable ~n:(1+tree_depth n_tree n) first in
            let last = Seq.pipeline ~c:clear ~e:enable ~n:(2+tree_depth n_tree n) last in
            let syndromes = 
                Array.init (2*Rp.t) 
                    (fun i -> 
                        let root = Rs.root i in
                        syndrome clear enable first Gfs.(root **: n) root x)
            in
            Seq.reg ~c:clear ~e:enable last, 
            Array.map (fun d -> Seq.reg ~c:clear ~e:last d) syndromes 

        module PSyndromes(N : N) = struct
            module I = interface clear[1] enable[1] first[1] last[1] x{|N.n|}[Gfh.bits] end
            module O = interface valid[1] syndromes{|2*Rp.t|}[Gfh.bits] end
            let f i = 
                let valid, syndromes = I.(psyndromes ~clear:i.clear ~enable:i.enable 
                                                     ~first:i.first ~last:i.last ~x:i.x) 
                in
                O.({ valid; syndromes })
        end

        (***********************************************************)
        (* berlekamp massey *)

        (* discrepancy unit (DC) *)
        let rDC clear enable load syndromes lambda =
            (* order syndromes s[0],s[2t-1],...,s[2],s[1] *)
            let s = List.(hd syndromes :: rev (tl syndromes)) in

            (* connect up registers *)
            let rec f d s = 
                match s with
                | [] -> []
                | s::s' ->
                    let d = Seq.reg ~c:clear ~e:enable (mux2 load s d) in
                    d :: f d s'
            in
            let s0 = wire Gfh.bits in
            let s = f s0 s in
            let s_fb = List.(hd (rev s)) in
            let () = s0 <== s_fb in
            let s = Utils.lselect s 0 Rp.t in

            (* generate multipliers *)
            let m = List.map2 Gfh.( *: ) s lambda in

            (* adder tree *)
            tree 2 (reduce Gfh.(+:)) m 

        (* PE0 unit *)
        let pe0 n clear enable init delta gamma mc b =
            let (--) s name = s -- (name ^ string_of_int n) in
            (* lambda update *) 
            let l = Seq.reg_fb ~c:clear ~cv:init ~e:enable ~w:Gfh.bits 
                (fun l -> Gfh.( (delta *: b) +: (l *: gamma) )) 
            in
            (* b update *)
            let b = Seq.reg ~c:clear ~cv:init ~e:enable (mux2 mc l b) in
            b -- "b", l

        (* ELU unit *)
        let pe0s clear enable delta gamma mc = 
            let rec f b n = 
                if n > Rp.t then []
                else
                    let init = if n=0 then one else zero in
                    let b,l = pe0 n clear enable (init Gfh.bits) delta gamma mc b in
                    l :: f b (n+1)
            in
            f (zero Gfh.bits) 0

        (* PE1 unit *)
        let pe1 n clear enable gamma syndrome delta delta' mc =
            let theta = Seq.reg_fb ~c:clear ~cv:syndrome ~e:enable ~w:Gfh.bits
                (fun theta -> mux2 mc delta' theta)
            in
            let delta' = Seq.reg ~c:clear ~cv:syndrome ~e:enable 
                Gfh.( (delta *: theta) +: (delta' *: gamma) ) 
            in
            delta'

        (* systolic array of pe1's *)
        let pe1s clear enable gamma syndromes delta mc = 
            let rec f delta' n = function
                | [] -> []
                | s::s' ->
                    let delta' = pe1 n clear enable gamma s delta delta' mc in
                    delta' :: f delta' (n+1) s'
            in
            let delta' = List.(rev (f (zero Gfh.bits) 0 (rev syndromes))) in
            delta'

        (* *)
        let pe1_2 n clear enable first last gamma syndrome delta delta' mc =
            let theta = Seq.reg_fb ~c:clear ~e:enable ~w:Gfh.bits
                (fun theta -> mux2 first syndrome (mux2 mc delta' theta))
            in
            let delta'' = Gfh.( (delta *: theta) +: (delta' *: gamma) ) in
            let delta' = Seq.reg ~c:clear ~e:enable 
                                 (mux2 first syndrome delta'') in
            delta', Seq.reg ~c:clear ~e:last delta''

        (* *)
        let pe1s_2 clear enable first last gamma syndromes delta mc = 
            let rec f delta' n = function
                | [] -> []
                | s::s' ->
                    let delta' = pe1_2 n clear enable first last 
                                         gamma s delta delta' mc in
                    delta' :: f (fst delta') (n+1) s'
            in
            let delta' = List.(rev (f (zero Gfh.bits) 0 (rev syndromes))) in
            delta'

        (* control unit *)
        let ctrl clear enable delta =
            let k_bits = (Utils.nbits (2*Rp.t)) + 1 in
            let mc = wire 1 in 
            let k = Seq.reg_fb ~c:clear ~e:enable ~w:k_bits 
                (fun k -> mux2 mc (~: k) (k +:. 1)) -- "K" 
            in
            let () = mc <== ((delta <>:. 0) &: (~: (msb k))) in
            let gamma = Seq.reg_fb ~c:clear ~cv:(one Gfh.bits) 
                ~e:enable ~w:Gfh.bits (fun gamma -> mux2 mc delta gamma) 
            in
            mc -- "mc", gamma -- "gamma"

        (* iBM hardware architecture *)
        let iBM ~clear ~enable ~start ~syndromes = 
            let delta = wire Gfh.bits -- "delta" in
            let clear' = clear |: start in
            let mc, gamma = ctrl clear' enable delta in
            let lambda = pe0s clear' enable delta gamma mc in
            let delta' = rDC clear enable start syndromes lambda in
            let () = delta <== delta' in
            lambda

        (* riBM hardware architecture *)
        let riBM ~clear ~enable ~start ~syndromes = 
            let delta0 = wire Gfh.bits in
            let clear = clear |: start in
            let mc, gamma = ctrl clear enable delta0 in
            let lambda = pe0s clear enable delta0 gamma mc in
            let delta' = pe1s clear enable gamma syndromes delta0 mc in
            let () = delta0 <== List.hd delta' in
            lambda

        (* RiBM hardware architecture *)
        let rriBM_old ~clear ~enable ~start ~syndromes = 
            let syndromes = 
                List.concat [ syndromes; 
                              Array.(to_list (make Rp.t (zero Gfh.bits))); 
                              [one Gfh.bits] 
                            ] 
            in
            let delta0 = wire Gfh.bits in
            let clear = clear |: start in
            let mc, gamma = ctrl clear enable delta0 in
            let delta' = pe1s clear enable gamma syndromes delta0 mc in
            let () = delta0 <== List.hd delta' in
            Utils.lselect delta' 0 (Rp.t-1),
            Utils.lselect delta' Rp.t (2*Rp.t)

        let rriBM ~clear ~enable ~first ~last ~syndromes = 
            let syndromes = 
                List.concat [ syndromes; 
                              Array.(to_list (make Rp.t (zero Gfh.bits))); 
                              [one Gfh.bits] 
                            ] 
            in
            let delta0 = wire Gfh.bits in
            let mc, gamma = ctrl (clear |: first) enable delta0 in
            let delta' = pe1s_2 clear enable first last gamma syndromes delta0 mc in
            let () = delta0 <== fst (List.hd delta') in
            List.map snd (Utils.lselect delta' 0 (Rp.t-1)),
            List.map snd (Utils.lselect delta' Rp.t (2*Rp.t))

        module RiBM = struct
            module I = interface clear[1] enable[1] first[1] last[1] syndromes{2*Rp.t}[Gfh.bits] end
            module O = interface w{Rp.t}[Gfh.bits] l{Rp.t+1}[Gfh.bits] end
            let f i = 
                let w, l = I.(rriBM ~clear:i.clear ~enable:i.enable 
                                    ~first:i.first ~last:i.last
                                    ~syndromes:i.syndromes)
                in
                O.({ w; l })
        end

        (***********************************************************)
        (* chien search *)

        let chien ~clear ~enable ~lambda = 
            let f i l = 
                Seq.reg_fb ~c:clear ~cv:l ~e:enable ~w:Gfh.bits
                    (fun l -> Gfh.cmul (Gfs.antilog i) l)
            in
            let l = Utils.mapi f lambda in
            tree 2 (reduce Gfh.(+:)) l

        let pchien ~p ~clear ~enable ~start ~lambda = 
            let lambda' = Array.map (fun _ -> wire Gfh.bits) lambda in
            let f i j l  = Gfh.cmul (Gfs.antilog (i*j)) l in
            let c = Array.init p (fun i -> Array.mapi (f (i+1)) lambda') in
            let fb = c.(p-1) in
            let () = 
                Array.iteri 
                    (fun i l ->
                        lambda'.(i) <== 
                            (mux2 start l (Seq.reg ~c:clear ~e:enable fb.(i)))) 
                    lambda
            in
            Array.map (fun c -> tree 2 (reduce Gfh.(+:)) (Array.to_list c)) c

        module PChien(N:N) = struct
            module I = interface clear[1] enable[1] start[1] lambda{|Rp.t+1|}[Gfh.bits] end
            module O = interface error_locs{|N.n|}[Gfh.bits] end
            let f i = 
                let error_locs = I.(pchien ~p:N.n ~clear:i.clear ~enable:i.enable
                                           ~start:i.start ~lambda:i.lambda)
                in
                O.({ error_locs })
        end

        (***********************************************************)
        (* forney *)

        let forney ~clear ~enable ~start ~store ~ctrl ~tap ~x = 
            let ghorner ~clear ~enable ~tap ~x = 
                Seq.reg_fb ~c:clear ~cv:tap ~e:enable ~w:Gfh.bits
                    Gfh.(fun d -> tap +: (x *: d))
            in
            let regce = Seq.reg ~c:clear ~e:enable in

            (* 2 stages, compute various values from x *)
            let x'  = regce Gfh.(antilog x) -- "x1" in
            let x'2 = regce (Gfh.rom (fun i -> Gfs.(i **: 2)) x') -- "x2" in
            let x   = regce Gfh.( cpow x' (Rp.b+(2*Rp.t)-1) ) -- "x3" in
            let x_m = regce (mux2 ctrl x'2 x') -- "x_m" in

            (* evaluate polys, latching results *)
            let h   = ghorner ~clear:start ~enable ~tap ~x:x_m -- "h" in
            let v   = Seq.reg ~c:clear ~e:(enable &: ctrl &: start) h -- "v" in

            (* 2 stages, compute error *)
            let e   = Seq.reg ~c:clear ~e:enable Gfh.(v /: h) -- "v_div_l" in 
            let e   = Seq.reg ~c:clear ~e:(enable &: store) Gfh.(x *: e) -- "x_mul_e" in 
            e

        module Forney = struct
            module I = interface clear[1] enable[1] start[1] store[1]
                                 ctrl[1] tap[Gfh.bits] x[Gfh.bits] end 
            module O = interface e[Gfh.bits] end 
            let f i = 
                let e = I.(forney ~clear:i.clear ~enable:i.enable ~start:i.start
                                  ~store:i.store ~ctrl:i.ctrl ~tap:i.tap ~x:i.x)
                in
                O.({ e })
        end

        (***********************************************************)
        (* error correction *)

        (***********************************************************)
        (* decoder *)

        let decoder clear enable load rx = 
            let t_bm = Rp.t*2 + 1 in
            let t_forney = Rp.t + (Rp.t/2) + 4 in
            let t_max = max t_bm t_forney in
            ()

    end

end




