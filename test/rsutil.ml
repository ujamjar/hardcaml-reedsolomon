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
let codeword message = rev @@ Sw.R.R.slice (Sw.R.encode (rev @@ message)) (n-1) 

(* parity of message *)
let parity message = rev @@ Sw.R.R.slice (Sw.R.parity (rev @@ message)) (2*t-1)

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

let syndromes recv = Sw.R.R.slice (Sw.R.syndromes (rev recv)) (2*t-1)

let dump a = Array.iter (Printf.printf "%i ") a; Printf.printf "\n"
