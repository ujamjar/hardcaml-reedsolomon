open HardCaml.Api
open HardCamlWaveLTerm.Api

open Rsutil

module Hw = HardCamlReedsolomon.Codec.Make(Sw.Gp)(Sw.Rp)
module Forney = Hw.Decoder.Forney
module G = Interface.Gen(Forney.I)(Forney.O)

let cfg = []

let test () = 
  let open Forney.I in
  let open Forney.O in

  let circ, sim, i, o = G.make "forney" Forney.f in
  let sim, waves = Waveterm_sim.wrap ~cfg sim in

  Cs.reset sim;
  i.enable := B.vdd;
  i.clear := B.vdd;
  Cs.cycle sim;
  i.clear := B.gnd;

  i.start := B.vdd;

  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

let () = test ()

