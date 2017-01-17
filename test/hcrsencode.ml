open HardCamlFramework

module Design = struct

  open HardCaml
  open Framework
  open Param

  let name = "rsencode"
  let desc = {|**HardCaml Reed-Solomon Encoder**

Simulation and netlist generation of a Reed-Solomon encoder
with configurable symbol size, error correction performance
and Galois field.

The testbench encodes a file with the given RS code and
writes to a special format that can be decoded with the
corresponding RS decoder testbench.

**RS code configuration**

*m* number of bits per symbol
*t* error correction capability

Two further parameters are derived from *m* and *t*

*n* = 2^m - 1, number symbols per code word
*k* = n - 2t, number of symbols per message

*note; it is not currently possible to specify code shortening*

*default: m=8, t=8, n=255, k=239*

**Galois field configuration**

*pp* primitive polynomial specified as an integer
*pe* primitive element specified as an integer

*default: pp=285, pe=2*

**Output file format**

First the input file is prepending with a 32 bit integer representing
the number of bits in the input file.

Then multiple *n.m* sized blocks of rs encoded data are generated.
The first *k.m* bits of each block are the actual data from the file.
The rest are computed parity symbols.

The final block will usually be padded with some zeros to reach a 
block boundary.
|}

  module Hw_config = struct
    include struct
      type 'a t = { m : 'a; t : 'a; pp : 'a; pe : 'a; }[@@deriving hardcaml]
    end
    let params = {
      m = Int 8, "Bits per symbol";
      t = Int 8, "Error correction capability";
      pp = Int 285, "Primitive polynomial";
      pe = Int 2, "Primitive element";
    }
  end

  module Tb_config = struct
    include struct
      type 'a t = { i : 'a; o : 'a; }[@@deriving hardcaml]
    end
    let params = {
      i = String "", "Input file";
      o = String "", "Output file";
    }
  end

  let validate hw tb = Ok

  module Make
    (B : Comb.S)
    (H : Params with type 'a t = 'a Hw_config.t)
    (T : Params with type 'a t = 'a Tb_config.t) = struct

    open Hw_config
    open Tb_config

    let m = get_int H.params.m
    let t = get_int H.params.t
    let pp = get_int H.params.pp
    let pe = get_int H.params.pe
    let k = (1 lsl m) - 1 - (2 * t)

    let in_file = get_string T.params.i
    let out_file = get_string T.params.o

    module Rp = struct
      let k = k
      let t = t
      let b = 0
    end
    module Gp = struct
      let pp = pp
      let pe = pe
    end
    module Rs = HardCamlReedsolomon.Codec.Make(Gp)(Rp)
    module I = Rs.Encoder.I
    module O = Rs.Encoder.O

    let hw = Rs.Encoder.f

    let wave_cfg = 
      let disp (n,b) = if b=1 then Display.bin (n,b) else Display.hex (n,b) in
      Some(["clock",Display.B; "clear",Display.B; "enable",Display.B] @
           I.(to_list (map disp t)) @ 
           O.(to_list (map disp t)))

    let n = k + (2 * t)
    let bits_per_data_block = m * k
    let bits_per_code_word = m * n
    let t' = t

    module Buf = Buf.Make(B)

    let tb sim i o _ = 
      let open Rs.Encoder.I in
      let open Rs.Encoder.O in
      let module S = Cyclesim.Api in

      let in_file = open_in in_file in
      let get_block = Buf.get_blocks true in_file (m * k) in

      let () = 
        let open Printf in
        printf "m=%i k=%i t=%i n=%i\n" m k t' n
      in

      let file_out = open_out out_file in
      let enc_buffer = ref B.empty in
      let add_enc_buffer d = 
        enc_buffer := B.concat_e [ d; !enc_buffer ];
        while B.width !enc_buffer >= 8 do
          output_byte file_out B.(to_int (select_e !enc_buffer 7 0));
          enc_buffer := B.(select_e !enc_buffer (width !enc_buffer - 1) 8);
        done
      in

      S.reset sim;
      i.enable := B.vdd;
      (S.in_port sim "clear") := B.vdd;
      S.cycle sim;
      (S.in_port sim "clear") := B.gnd;

      let rec encode_blocks () = 
        let block = get_block () in
        if block = B.empty then ()
        else begin

          (* load data *)
          i.ctrl := B.gnd;
          for j=0 to k-1 do
            let data = B.select block ((j+1)*m-1) (j*m) in
            add_enc_buffer data;
            i.d := data;
            S.cycle sim;
          done;

          (* read parity *)
          i.ctrl := B.vdd;
          for j=0 to (t' * 2) - 1 do
            S.cycle sim; 
            add_enc_buffer !(o.q);
          done;

          encode_blocks ()
        end
      in
      
      encode_blocks ();

      (* flush and close file *)
      if B.width !enc_buffer > 0 then begin
        add_enc_buffer B.(zero (8 - (B.width !enc_buffer))) 
      end;
      close_out file_out

  end

end

module A = HardCamlFrameworkConsole.App.Make(Design)

