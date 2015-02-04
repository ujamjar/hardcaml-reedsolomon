open HardCaml.Api
open HardCamlWaveLTerm.Api

open Rsutil

module Hw = HardCamlReedsolomon.Codec.Make(Sw.Gp)(Sw.Rp)
module Chien = Hw.Decoder.PChien(struct let n=1 end)
module G = Interface.Gen(Chien.I)(Chien.O)

let cfg = 
  let open Waveterm_waves in
  let open Chien in
  O.(to_list (map (fun (n,_) -> n,U) t)) 

let test () = 
  let open Chien.I in
  let open Chien.O in

  let circ, sim, i, o = G.make "chien" Chien.f in
  let sim, waves = Waveterm_sim.wrap ~cfg sim in

  let codeword = codeword (message ()) in
  let _ = error 2 in
  let _ = error 2 in
  let error = error 2 in
  let received = codeword ^. error in
  let syndromes = syndromes received in
  let w, l = berlekamp syndromes in
  let ch = chien l in
  let el = List.map err_loc ch in

  dump codeword;
  dump error;
  dump received;
  dump syndromes;
  dump w;
  dump l;
  dump (Array.of_list ch);
  dump (Array.of_list el);

  (* val ch : int list = [12; 13; 5; 8; 12; 5; 0; 9; 4; 8; 4; 0; 1; 1; 9] *)

  dump (Array.init n_elems (fun i -> Sw.R.horner l (Sw.G.antilog i)));
  dump (Array.init n_elems (fun i -> (Sw.G.(log @@ inv @@ antilog i))));

  Cs.reset sim;
  i.enable := B.vdd;
  i.clear := B.vdd;
  Cs.cycle sim;
  i.clear := B.gnd;

  i.start := B.vdd;
  for j=0 to Array.length l - 1 do
    i.lambda.(j) := B.consti sbits l.(j)
  done;
  for j=0 to n_elems-2 do
    Cs.cycle sim;
    i.start := B.gnd;
  done;
  for j=0 to 3 do
    Cs.cycle sim;
  done;

  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

let () = test ()


