open HardCaml.Api
open HardCamlWaveLTerm.Api

open Rsutil

module Hw = HardCamlReedsolomon.Codec.Make(Sw.Gp)(Sw.Rp)
module Bm = Hw.Decoder.RiBM
module G = Interface.Gen(Bm.I)(Bm.O)

let cfg = 
  let open Waveterm_waves in
  let open Bm in
  ["clock",B] @
  I.(to_list (map (fun (n,_) -> n,B) t)) @
  O.(to_list (map (fun (n,_) -> n,U) t)) 

let test () = 
  let open Bm.I in
  let open Bm.O in

  let circ, sim, i, o = G.make "berlekamp" Bm.f in
  let () = 
    if false then begin
      let f = open_out "test/bm.v" in
      HardCaml.Rtl.Verilog.write (output_string f) circ;
      close_out f
    end
  in
  let sim, waves = Waveterm_sim.wrap ~cfg sim in

  (* err'd message *)
  let codeword = codeword (message ()) in
  let error = error 2 in
  let received = codeword ^. error in
  let syndromes = syndromes received in
  let bm_sw_w, bm_sw_l = berlekamp syndromes in

  dump bm_sw_w;
  dump bm_sw_l;

  Cs.reset sim;
  i.enable := B.vdd;
  i.clear := B.vdd;
  Cs.cycle sim;
  i.clear := B.gnd;

  i.first := B.vdd;
  for j=0 to 2*Rsutil.t-1 do
    i.syndromes.(j) := B.consti sbits syndromes.(j);
  done;
  for j=0 to 2*Rsutil.t do
    if j=(2*Rsutil.t) then begin
      i.last := B.vdd;
    end;
    Cs.cycle sim;
    i.first := B.gnd;
    i.last := B.gnd;
  done;

  for i=0 to 2 do
    Cs.cycle sim;
  done;

  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

let () = test ()

