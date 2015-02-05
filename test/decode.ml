open HardCaml.Api
open HardCamlWaveLTerm.Api

open Rsutil

module Hw = HardCamlReedsolomon.Codec.Make(Sw.Gp)(Sw.Rp)
module Decoder = Hw.Decoder(struct let n = 1 end)
module Decode = Decoder.Decode
module G = Interface.Gen(Decode.I)(Decode.O)

let cfg = 
  let open Waveterm_waves in
  let open Decode in
  ["clock",B] @
  I.(to_list (map (fun (n,b) -> if b=1 then n,B else n,U) t)) @
  O.(to_list (map (fun (n,b) -> if b=1 then n,B else n,U) t))

let test () = 
  let open Decode.I in
  let open Decode.O in

  let circ, sim, i, o = G.make "decode" Decode.f in
  let sim, waves = Waveterm_sim.wrap ~cfg sim in

  let codeword = codeword (message ()) in
  let _ = error 2 in
  let _ = error 2 in
  let error = error 2 in
  let received = codeword ^. error in
(*  let syndromes = syndromes received in
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
*)

  Cs.reset sim;
  i.enable := B.vdd;
  i.clear := B.vdd;
  Cs.cycle sim;
  i.clear := B.gnd;

  let received = rev received in (* load data from highest power first *)

  (* load received data *)
  i.first := B.vdd;
  i.load := B.vdd;
  for j=0 to n-1 do
    i.x.(0) := B.consti sbits received.(j);
    if j=(n-1) then begin
      i.last := B.vdd;
    end;
    Cs.cycle sim;
    i.first := B.gnd;
    i.last := B.gnd;
  done;
  i.load := B.gnd;

  let ocnt = ref 0 in
  let corrected = Array.init n (fun _ -> 0) in
  while !ocnt < n do
    Cs.cycle sim;
    if B.to_int !(o.ordy) <> 0 then begin
      corrected.(!ocnt) <- B.to_int !(o.corrected.(0));
      incr ocnt;
    end
  done;

  Cs.cycle sim; (* show last cycle *)
  Printf.printf "EXPECTED : "; dump (rev codeword);
  Printf.printf "CORRECTED: "; dump corrected;

  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

let () = test ()

