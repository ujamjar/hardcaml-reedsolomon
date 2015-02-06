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

module N = struct let n = 2 end (* set the parallelism of the syndrome calculations *)
module Hw = HardCamlReedsolomon.Codec.Make(Sw.Gp)(Sw.Rp)
module Decoder = Hw.Decoder(N)
module PSyndromes = Decoder.PSyndromes
module G = Interface.Gen(PSyndromes.I)(PSyndromes.O)

(* configure waveform display *)
let cfg = 
  let open Waveterm_waves in
  let open PSyndromes in
  (List.map  (fun n -> n, B) ["clock" ]) @
  I.(to_list (map (fun (n,b) -> n, if b=1 then B else U) t)) @ 
  O.(to_list (map (fun (n,b) -> n, if b=1 then B else U) t))  

let test () = 
  let open PSyndromes.I in
  let open PSyndromes.O in

  let cycles_per_codeword = (n + N.n - 1) / N.n in
  let offset = cycles_per_codeword * N.n - n in
  Printf.printf "sym=%i off=%i\n%!"
    cycles_per_codeword offset;

  let circ, sim, i, o = G.make "syndromes" (PSyndromes.f ~scale:offset) in
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

  dump received;
  (*let recv = Array.concat [ Array.init offset (fun _ -> 0); rev received ] in*)
  let recv = Array.concat [ rev received; Array.init offset (fun _ -> 0) ] in

  (* load received data *)
  i.first := B.vdd;
  for j=0 to cycles_per_codeword-1 do
    for k=0 to N.n-1 do
      i.x.(k) := B.consti sbits recv.(j*N.n+k);
    done;
    if j=cycles_per_codeword-1 then begin
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
  let syndromes_sw = syndromes received in
  let syndromes_tb = Array.map (fun x -> B.to_int !x) o.syndromes in
  (*let iroots = Array.init (2*Sw.Rp.t) (fun i -> Sw.G.(inv ((Sw.R.root i) **: offset))) in
  let syndromes_tb = Array.init (2*Sw.Rp.t) (fun i -> Sw.G.( iroots.(i) *: syndromes_tb.(i) )) in*)
  dump syndromes_sw;
  dump syndromes_tb;

  Cs.cycle sim;

  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

let () = test ()

