(* reed-solomon decoder syndrome calculation testbench 
 *
 * Compares sw to hw.
 *
 * The syndrome hardware supports parallel inputs, which isn't 
 * tested here (yet) 
 *)

open HardCaml.Api
open HardCamlWaveLTerm.Api

open Rsutil

module Hw = HardCamlReedsolomon.Codec.Make(Sw.Gp)(Sw.Rp)
module Decoder = Hw.Decoder(struct let n = 1 end)
module PSyndromes = Decoder.PSyndromes
module G = Interface.Gen(PSyndromes.I)(PSyndromes.O)

(* configure waveform display *)
let cfg = 
  let open Waveterm_waves in
  let open PSyndromes in
  (List.map  (fun n -> n, B) ["clock" ]) @
  (I.(to_list (map2 (fun (n,_) b -> n, b) t {clear=B; enable=B; first=B; last=B; x=[|U|]}))) @
  (O.(to_list (map (fun (n,_) -> n, (if n="valid" then B else U)) t))) 

let test () = 
  let open PSyndromes.I in
  let open PSyndromes.O in

  let circ, sim, i, o = G.make "syndromes" PSyndromes.f in
  let () = if false then HardCaml.Rtl.Verilog.write print_string circ in
  let sim, waves = Waveterm_sim.wrap ~cfg sim in
  
  (* err'd message *)
  let codeword = codeword (message ()) in
  let error = error 2 in
  let received = codeword ^. error in

  (* reset, clear, enable *)
  Cs.reset sim;
  i.enable := B.vdd;
  i.clear := B.vdd;
  Cs.cycle sim;
  i.clear := B.gnd;

  let recv = rev received in

  (* load received data *)
  i.first := B.vdd;
  for j=0 to n-1 do
    i.x.(0) := B.consti sbits recv.(j);
    if j=(n-1) then begin
      i.last := B.vdd;
    end;
    Cs.cycle sim;
    i.first := B.gnd;
    i.last := B.gnd;
  done;

  (* wait for valid *)
  while B.to_int !(o.valid) = 0 do
    Cs.cycle sim;
  done;

  (* compare *)
  let syndromes_tb = Array.map (fun x -> B.to_int !x) o.syndromes in
  let syndromes_sw = syndromes received in
  dump syndromes_sw;
  dump syndromes_tb;

  Cs.cycle sim;

  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

let () = test ()

