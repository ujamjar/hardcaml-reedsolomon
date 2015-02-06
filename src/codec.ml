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

  module type N = sig val n : int end

  module Decoder(N : N) : sig

    val syndrome : root:int -> enable:t -> first:t -> x:t -> t
    val syndromes : enable:t -> first:t -> x:t -> t array
    val psyndromes : scale:int -> clear:t -> enable:t -> first:t -> last:t -> x:t array -> 
      t * t array
    module PSyndromes : sig
      module I : interface clear enable first last x{| |} end
      module O : interface valid syndromes{| |} end
      val f : scale:int -> t I.t -> t O.t
    end

    val iBM : clear:t -> enable:t -> start:t -> syndromes:t list -> t list
    val riBM : clear:t -> enable:t -> start:t -> syndromes:t list -> t list
    val rriBM : clear:t -> enable:t -> first:t -> last:t -> syndromes:t list -> t list * t list
    module RiBM : sig
      module I : interface clear enable first last syndromes{| |} end
      module O : interface w{| |} l{| |} end
      val f : t I.t -> t O.t
    end

    val chien : clear:t -> enable:t -> lambda:t list -> t
    val pchien : p:int -> clear:t -> enable:t -> start:t -> lambda:t array -> 
      t array * t array * t array * t array
    module PChien : sig
      module I : interface clear enable start lambda{| |}  end
      module O : interface eval{| |} eloc{| |} evld{| |} eerr{| |} end
      val f : t I.t -> t O.t
    end

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

  module type N = sig
    val n : int
  end

  module Decoder(N : N) = struct

    open B

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

    let psyndromes ~scale ~clear ~enable ~first ~last ~x = 
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
      Array.init (Array.length syndromes) 
        (fun i ->
          let iroot = Gfs.(inv (Rs.root i **: scale)) in
          Seq.reg ~c:clear ~e:last 
            (if scale=0 then syndromes.(i) else (Gfh.cmul iroot syndromes.(i))))

    module PSyndromes = struct
      module I = interface clear[1] enable[1] first[1] last[1] x{|N.n|}[Gfh.bits] end
      module O = interface valid[1] syndromes{|2*Rp.t|}[Gfh.bits] end
      let f ~scale i = 
        let valid, syndromes = I.(psyndromes ~scale
          ~clear:i.clear ~enable:i.enable 
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
      module I = interface clear[1] enable[1] first[1] last[1] syndromes{|2*Rp.t|}[Gfh.bits] end
      module O = interface w{|Rp.t|}[Gfh.bits] l{|Rp.t+1|}[Gfh.bits] end
      let f i = 
        let w, l = I.(rriBM ~clear:i.clear ~enable:i.enable 
                      ~first:i.first ~last:i.last
                      ~syndromes:(Array.to_list i.syndromes))
        in
        O.({ w=Array.of_list w; l=Array.of_list l })
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

    type state = Start | Run

    let chien_ctrl ~clear ~enable ~start = 
      let open Signal.Guarded in
      let st, sm, next = Seq.statemachine ~c:clear ~e:enable [Start;Run] in
      let vld = g_wire B.gnd in
      let eloc = Seq.g_reg ~c:clear ~cv:(one Gfh.bits) ~e:enable Gfh.bits in
      let () = compile [
          sm [
            Start, [
              eloc $==. 1;
              g_when start [
                eloc $==. 2;
                vld $==. 1;
                next Run;
              ];
            ];
            Run, [
              vld $==. 1;
              eloc $== (eloc#q +:. 1);
              g_when (eloc#q ==:. (Gfh.n_elems-2)) [
                eloc $==. 0;
              ];
              g_when (eloc#q ==:. 0) [
                next Start;
              ];
            ];
          ];
        ] in
      vld#q, eloc#q

    (* produces error location results in reverse order ie from [n_elem-2 ... 0] *)
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
      let eval = Array.map (fun c -> tree 2 (reduce Gfh.(+:)) (Array.to_list c)) c in
      let vld, eloc = chien_ctrl ~clear ~enable ~start in (* XXX FIX ME for parallel execution *)
      let eloc = Array.init p (fun _ -> eloc) in
      let evld = Array.init p (fun _ -> vld) in
      let eerr = Array.init p (fun j -> (eval.(j) ==:. 0) &: evld.(j)) in
      eval, eloc, evld, eerr

    module PChien = struct
      module I = interface clear[1] enable[1] start[1] lambda{|Rp.t+1|}[Gfh.bits] end
      module O = interface eval{|N.n|}[Gfh.bits] eloc{|N.n|}[Gfh.bits] 
                           evld{|N.n|}[1] eerr{|N.n|}[1] end
      let f i = 
        let eval, eloc, evld, eerr = 
          I.(pchien ~p:N.n ~clear:i.clear ~enable:i.enable
                    ~start:i.start ~lambda:i.lambda)
        in
        O.({ eval; eloc; evld; eerr })
    end

    (***********************************************************)
    (* forney *)

    let forney_serial ~clear ~enable ~start ~store ~ctrl ~tap ~x = 
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

    module Forney_serial = struct
      module I = interface clear[1] enable[1] start[1] store[1]
                          ctrl[1] tap[Gfh.bits] x[Gfh.bits] end 
      module O = interface e[Gfh.bits] end 
      let f i = 
        let e = I.(forney_serial ~clear:i.clear ~enable:i.enable ~start:i.start
                                ~store:i.store ~ctrl:i.ctrl ~tap:i.tap ~x:i.x)
        in
        O.({ e })
    end

    let forney ~clear ~enable ~vld ~err ~v ~l ~x = 
      let n_tree = 4 in
      let reg d = Seq.reg ~c:clear ~e:enable d in
      let pipe n d = Seq.pipeline ~c:clear ~e:enable ~n:n d in

      (* parallel polynomial evaluation *)
      let eval clear enable poly x = 
        let a = Array.mapi Gfh.(fun pow coef -> coef *: (cpow x pow)) poly in
        let a = Array.map (fun d -> reg d) a in
        let add_p a = reg (reduce Gfh.(+:) a) in
        tree n_tree add_p (Array.to_list a) 
      in

      let v_depth = tree_depth n_tree (Array.length v) in
      let l_depth = tree_depth n_tree (Array.length l) in

      assert (v_depth >= l_depth);

      let xv = reg Gfh.(antilog x) in
      let xl = (reg Gfh.(rom (fun i -> Gfs.(i **: 2)) xv)) -- "xl" in
      let xv = (reg xv) -- "xv" in

      let v = (eval clear enable v xv) -- "ev" in
      let l = (eval clear enable l xl) -- "el_" in
      let l = (pipe (v_depth-l_depth) l) -- "el" in

      let x = (pipe (1+v_depth) Gfh.( cpow xv (Rp.b+(2*Rp.t)-1) )) -- "xp" in
      reg Gfh.( x *: (v /: l) ),
      pipe (4+v_depth) vld,
      pipe (4+v_depth) err

    module Forney = struct
      module I = interface clear[1] enable[1] vld[1] err[1]
                           v{|Rp.t|}[Gfh.bits] l{|(Rp.t+1)/2|}[Gfh.bits] x[Gfh.bits] end 
      module O = interface emag[Gfh.bits] frdy[1] ferr[1] end 
      let f i = 
        let emag, frdy, ferr = I.(forney ~clear:i.clear ~enable:i.enable 
                                         ~vld:i.vld ~err:i.err
                                         ~v:i.v ~l:i.l ~x:i.x) in
        O.({ emag; frdy; ferr })
    end

    module PForney = struct
      module I = interface clear[1] enable[1] vld{|N.n|}[1] err{|N.n|}[1]
                          v{|Rp.t|}[Gfh.bits] l{|(Rp.t+1)/2|}[Gfh.bits] 
                          x{|N.n|}[Gfh.bits] end 
      module O = interface emag{|N.n|}[Gfh.bits] frdy{|N.n|}[1] ferr{|N.n|}[1] end 
      let f i = 
        let o = 
          Array.init N.n (fun j ->
            I.(forney ~clear:i.clear ~enable:i.enable ~vld:i.vld.(j) ~err:i.err.(j)
                      ~v:i.v ~l:i.l ~x:i.x.(j))) 
        in 
        O.({ emag=Array.map (fun (x,_,_) -> x) o; 
            frdy=Array.map (fun (_,x,_) -> x) o; 
            ferr=Array.map (fun (_,_,x) -> x) o}) 
    end

    (***********************************************************)
    (* input codeword store *)

    module Fifo = struct
      module I = interface clear[1] wr[1] d{|N.n|}[Gfh.bits] rd[1] end
      module O = interface q{|N.n|}[Gfh.bits] end
      let f i =
        let open I in
        let fbits = 5 in (* XXX get actual size *)
        let felems = 1 lsl fbits in
        let wa = Seq.reg_fb ~c:i.clear ~e:i.wr ~w:fbits (fun d -> d +:. 1) -- "fifo_wa" in
        let ra = Seq.reg_fb ~c:i.clear ~e:i.rd ~w:fbits (fun d -> d +:. 1) -- "fifo_ra" in
        let d = concat (List.rev (Array.to_list i.d)) in
        let q = Seq.ram_rbw felems ~we:i.wr ~wa ~d ~re:i.rd ~ra in
        let q = Array.init N.n (fun i -> select q (((i+1)*Gfh.bits)-1) (i*Gfh.bits)) in
        O.{ q }
    end

    (***********************************************************)
    (* decoder *)

    module Decode = struct
      module I = interface clear[1] enable[1] load[1] first[1] last[1] x{|N.n|}[Gfh.bits] end
      module O = interface 
        (syn : PSyndromes.O)
        (bm : RiBM.O)
        (ch : PChien.O)
        (fy : PForney.O)
        corrected{|N.n|}[Gfh.bits]
        ordy[1]
      end

      let f i = 
        let clear, enable = I.(i.clear, i.enable) in
        let reg d = Seq.reg ~c:clear ~e:enable d in
        let pipe ~n d = Seq.pipeline ~c:clear ~e:enable ~n d in

        (* syndromes *)
        let syn = PSyndromes.f ~scale:0 
            { PSyndromes.I.clear; enable; 
              first=i.I.first; last=i.I.last; x=i.I.x } 
        in

        let fifo_re = wire 1 in
        let fifo = Fifo.f { Fifo.I.clear; wr=i.I.load; d=i.I.x; rd=fifo_re } in

        (* berlekamp-massey *)
        let first = syn.PSyndromes.O.valid in
        let last = pipe ~n:(2*Rp.t) syn.PSyndromes.O.valid in
        let bm = RiBM.f 
            { RiBM.I.clear; enable; first; last; 
              syndromes=syn.PSyndromes.O.syndromes } 
        in

        (* chien search *)
        let start = reg last in
        let ch = PChien.f { PChien.I.clear; enable; start; lambda=bm.RiBM.O.l } in

        (* forney *)
        let l = Array.init ((Rp.t+1)/2) (fun i -> bm.RiBM.O.l.(i*2+1)) in
        let fy = PForney.f 
            { PForney.I.clear; enable; vld=ch.PChien.O.evld; err=ch.PChien.O.eerr;
              v=bm.RiBM.O.w; l; x=ch.PChien.O.eloc }
        in

        (* correction *)
        let () = fifo_re <== fy.PForney.O.frdy.(0) in (* dont need array? *)
        let corrected = Array.init N.n 
            (fun j ->
              mux2 (reg fy.PForney.O.ferr.(j)) 
                (fifo.Fifo.O.q.(j) ^: (reg fy.PForney.O.emag.(j)))
                fifo.Fifo.O.q.(j))
        in
        let ordy = reg fy.PForney.O.frdy.(0) in

        (* for now all submodule outputs *)
        O.{ syn; bm; ch; fy; corrected; ordy }

    end

  end

end

