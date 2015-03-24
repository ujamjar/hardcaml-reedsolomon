(* binary bit blocks *)

module Make(B : HardCaml.Comb.S) = struct

  open B

  let get_blocks enc file block_size = 
    (* size of file in bits *)
    let size = Unix.( (fstat (descr_of_in_channel file)).st_size ) * 8 in
    (* bit buffer *)
    let buffer = if enc then ref (B.consti 32 size) else ref B.empty in

    (* re-fill buffer *)
    let rec fill () = 
      if width !buffer >= block_size then ()
      else
        match input_byte file with
        | x -> begin
          buffer := concat_e [ consti 8 x; !buffer ];
          fill ()
        end
        | exception End_of_file -> close_in file
        | exception Sys_error _ -> ()
    in

    let get () = 
      fill ();
      let width = width !buffer in
      if width = 0 then (* finished *)
        B.empty
      else if width < block_size then begin (* pad *)
        let r = concat_e [ zero (block_size - width); !buffer ] in
        buffer := B.empty;
        r
      end else (* return buffer *)
        let r = select_e !buffer (block_size-1) 0 in
        buffer := select_e !buffer (width-1) block_size;
        r
    in

    get

end


