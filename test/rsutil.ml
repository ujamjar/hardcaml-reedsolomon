module Sw = Reedsolomon.Codec.BBCTest
(*module Hw = HardCamlReedsolomon.Codec.Make(Sw.Gp)(Sw.Rp)*)

(* rs code params *)
let k = Sw.Rp.k
let t = Sw.Rp.t
let n = k + t + t

(* number of field elements *)
let n_elems = Sw.G.n_elems

(* number of bits per symbol *)
let sbits = HardCaml.Utils.clog2 n_elems

(* create a random message *)
let message () = Array.init k (fun _ -> Random.int n_elems)

let rev a = 
  let len = Array.length a in
  Array.init len (fun i -> a.(len - i - 1))

(* codeword from message *)
let codeword message = Sw.R.R.slice (Sw.R.encode message) (n-1) 

(* parity of message *)
let parity message = Sw.R.R.slice (Sw.R.parity message) (2*t-1)

let rand_sort a = 
  for i=(Array.length a - 1) downto 1 do
    let swap = Random.int (i+1) in
    let tmp = a.(i) in
    a.(i) <- a.(swap);
    a.(swap) <- tmp
  done

(* random error vector with 'e' errors *)
let error e = 
  let e = Array.init n (fun i -> if i < e then 1 + Random.int (n_elems-1) else 0) in
  rand_sort e;
  e

let (^.) a b = Array.init (Array.length a) (fun i -> a.(i) lxor b.(i))

let syndromes recv = Sw.R.R.slice (Sw.R.syndromes recv) (2*t-1)

let berlekamp syn = Sw.R.Sarwate.rriBM syn

let chien l = Sw.R.chien l

let err_loc l = Sw.R.error_location l

let dump a = Array.iter (Printf.printf "%2i ") a; Printf.printf "\n"


