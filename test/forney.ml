open HardCaml.Api
open HardCamlWaveLTerm.Api

open Rsutil

module Hw = HardCamlReedsolomon.Codec.Make(Sw.Gp)(Sw.Rp)
module Decoder = Hw.Decoder(struct let n = 1 end)
module Forney = Decoder.Forney
module G = Interface.Gen(Forney.I)(Forney.O)

let cfg = 
  let open Waveterm_waves in
  let open Forney in
  ["clock",B] @
  I.(to_list (map (fun (n,b) -> if b=1 then n,B else n,U) t)) @
  O.(to_list (map (fun (n,b) -> if b=1 then n,B else n,U) t)) @
  ["xl",U; "xv",U; "el",U; "ev",U]

let test () = 
  let open Forney.I in
  let open Forney.O in

  let circ, sim, i, o, n = G.make "forney" Forney.f in
  let sim, waves = Waveterm_sim.wrap ~cfg sim in

  let codeword = codeword (message ()) in
  let _ = error 2 in
  let _ = error 2 in
  let error = error 2 in
  let received = codeword ^. error in
  let syndromes = syndromes received in
  let v, l = berlekamp syndromes in
  let l' = Array.init ((Rsutil.t+1)/2) (fun i -> l.(i*2+1)) in (* derivative of l, if evaluated at x^2 *)
  let ch = Array.of_list @@ chien l in
  let fy = Array.map (Sw.R.Sarwate.forney v l) ch in

  Printf.printf "c: "; dump codeword;
  Printf.printf "e: "; dump error;
  Printf.printf "r: "; dump received;
  Printf.printf "s: "; dump syndromes;
  Printf.printf "w: "; dump v;
  Printf.printf "l: "; dump l;
  Printf.printf "': "; dump l';
  Printf.printf "n: "; dump ch;
  Printf.printf "f: "; dump fy;

  (* forney debug gubbins *)
  Printf.printf "_: %i %i %i %i\n"
    Sw.G.(antilog 3) Sw.G.( (antilog 3) **: 2)
    Sw.G.(antilog 9) Sw.G.( (antilog 9) **: 2);
  Printf.printf "_: [v=%i l'=%i] [v=%i l'=%i] \n" 
    (Sw.R.horner v Sw.G.(antilog 3)) (Sw.R.horner l' Sw.G.((antilog 3) **: 2))
    (Sw.R.horner v Sw.G.(antilog 9)) (Sw.R.horner l' Sw.G.((antilog 9) **: 2));

  Cs.reset sim;
  i.enable := B.vdd;
  i.clear := B.vdd;
  Cs.cycle sim;
  i.clear := B.gnd;
  
  for j=0 to Array.length v - 1 do
    i.v.(j) := B.consti sbits v.(j)
  done;
  for j=0 to Array.length l' - 1 do
    i.l.(j) := B.consti sbits l'.(j)
  done;

  for j=0 to Array.length ch - 1 do
    i.x := B.consti sbits ch.(j);
    Cs.cycle sim;
  done;
  (* some flush cycles *)
  for j=0 to 4 do
    Cs.cycle sim;
  done;

  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

let () = test ()

