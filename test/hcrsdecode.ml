open HardCamlFramework

module Design = struct

  open HardCaml
  open Framework
  open Param

  let name = "rsdecode"
  let desc = "**HardCaml Reed-Solomon Decoder**
  
Simulation and netlist generation of a Reed-Solomon decoder
with configurable symbol size, error correction performance,
Galois field and parallelism.

The testbench decodes a file produced with the corresponding
RS encoder testbench.  Errors may be manually inserted into
the input file.  The testbench will write a file with the
corrected results.  If all went well it should match the
file input to the encoder.

The decoder can process multiple symbols per cycle thus 
increasing performance at the cost of much larger circuits.
This is controlled with the *parallelism* parameter.

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
"

  module Hw_config = struct
    include interface m t pp pe parallelism end
    let params = {
      m = Int 8, "Bits per symbol";
      t = Int 8, "Error correction capability";
      pp = Int 285, "Primitive polynomial";
      pe = Int 2, "Primitive element";
      parallelism = Int 1, "Codeword parallelism"
    }
  end

  module Tb_config = struct
    include interface i o maxcodes end
    let params = {
      i = String "", "Input file";
      o = String "", "Output file";
      maxcodes = Int 0, "limit number of codewords to decode";
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
    let parallelism = get_int H.params.parallelism

    let in_file = get_string T.params.i
    let out_file = get_string T.params.o
    let maxcodes = get_int T.params.maxcodes

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
    module Decoder = Rs.Decoder(struct let n = parallelism end)
    module I = Decoder.Decode.I
    module O = Decoder.Decode.O

    let hw = Decoder.Decode.f

    let wave_cfg = 
      let disp (n,b) = if b=1 then Display.bin (n,b) else Display.hex (n,b) in
      Some(["clock",Display.B; "clear",Display.B; "enable",Display.B] @
           I.(to_list (map disp t)) @ 
           O.(to_list (map disp t)))

    let n = k + (2 * t)
    let bits_per_data_block = m * k
    let bits_per_code_word = m * n
    let cycles_per_codeword = (n + parallelism - 1) / parallelism
    let t' = t (* alias *)
    let k' = k

    module Buf = Buf.Make(B)

    let tb sim i o _ = 
      let open Decoder.Decode.I in
      let open Decoder.Decode.O in
      let module S = Cyclesim.Api in
      
      let in_file = open_in in_file in
      let get_block = Buf.get_blocks false in_file bits_per_code_word in

      let () = 
        let open Printf in
        printf "m=%i k=%i t=%i n=%i\n" m k t' n;
        printf "parallelism=%i\n" parallelism;
        printf "bits_per_code_word=%i\n" bits_per_code_word;
        printf "cycles_per_code_word=%i\n" cycles_per_codeword;
      in

      (* output buffer/file handling *)
      let file_out = open_out out_file in
      let dec_buffer = ref B.empty in
      let file_size_bits = ref 0 in
      let decoded_bits = ref 0 in
      let flushed_bits = ref 0 in
      let flush_dec_buffer () = 
        while !flushed_bits < !file_size_bits && B.width !dec_buffer >= 8 do
          output_byte file_out B.(to_int (select_e !dec_buffer 7 0));
          dec_buffer := B.(select_e !dec_buffer (width !dec_buffer - 1) 8);
          flushed_bits := !flushed_bits + 8;
        done
      in
      let add_dec_buffer d = 
        dec_buffer := B.concat_e [ d; !dec_buffer ];
        flush_dec_buffer ()
      in

      S.reset sim;
      i.enable := B.vdd;

      let decode_block () = 
        let block = get_block () in

        i.clear := B.vdd;
        S.cycle sim;
        i.clear := B.gnd;
        
        (* load received data *)
        i.first := B.vdd;
        i.load := B.vdd;
        for j=0 to cycles_per_codeword-1 do
          for k=0 to parallelism-1 do
            let l = (j*parallelism) + k in
            let sym = B.select_e block ((l+1)*m-1) (l*m) in
            i.x.(k) := if sym = B.empty then B.zero m else sym;
          done;
          if j=(cycles_per_codeword-1) then begin
            i.last := B.vdd;
          end;
          S.cycle sim;
          i.first := B.gnd;
          i.last := B.gnd;
        done;
        i.load := B.gnd;

        let ccnt = ref 0 in (* count cycles per code word *)
        let scnt = ref 0 in (* count extracted data symbols *)
        while !ccnt < cycles_per_codeword do
          if B.to_int !(o.ordy) <> 0 then begin
            for k=0 to parallelism-1 do
              if !scnt < k' then begin
                add_dec_buffer !(o.corrected.(k));
                incr scnt;
              end
            done;
            incr ccnt;
          end;
          S.cycle sim
        done;
        B.to_int !(o.error_count)
      in

      let n_codewords = ref 0 in
      let maxcodes () = maxcodes = 0 || !n_codewords < maxcodes in 

      (* decode until we have read the header *)
      while maxcodes () && B.width !dec_buffer < 32 do
        let error_count = decode_block () in
        Printf.printf "decoded %i bits for header [%i errors]\n%!" (B.width !dec_buffer) error_count;
        incr n_codewords;
      done;
    
      (* extract the file size header - also starts flushing process *)
      file_size_bits := B.(to_int (select !dec_buffer 31 0));
      dec_buffer := B.(select_e !dec_buffer (width !dec_buffer - 1) 32);
      decoded_bits := B.(width !dec_buffer);

      (* decode rest of file *)
      while maxcodes () && !decoded_bits < !file_size_bits do
        let error_count = decode_block () in
        decoded_bits := !decoded_bits + bits_per_data_block;
        Printf.printf "decoded %i / %i bits [%i errors]\n%!" !decoded_bits !file_size_bits error_count;
        incr n_codewords;
      done;

      (* flush and close file *)
      add_dec_buffer B.(zero 8);
      close_out file_out

  end

end

module A = HardCamlFrameworkConsole.App.Make(Design)


