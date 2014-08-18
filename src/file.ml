module L = Log
module G = Gallery
module SJ = Serial_j
module BO = Bi_outbuf
module Y = Yojson.Safe
module LX = Lexing
module C = Cryptokit
module CC = Cryptokit.Cipher
module R = Cryptokit.Random
module Z = Cryptokit.Zlib
module H = Cryptokit.Hash
module P = Cryptokit.Padding
module B = Buffer

module type Representation =
  sig
    type in_channel
    type out_channel
    val open_in : string -> string -> in_channel
    val input : in_channel -> string -> int -> int -> int
    val close_in_noerr : in_channel -> unit
    val open_out : string -> string -> out_channel
    val output : out_channel -> string -> int -> int -> unit
    val close_out_noerr : out_channel -> unit
  end

module type Transform =
  sig
    val bsize : int
    val p_to_c : string -> string * C.transform
    val c_to_p : string -> int * (string -> C.transform)
  end

module type S =
  sig
    val open_file : string -> L.t
    val write_file : string -> L.t -> unit
  end

module Make(R: Representation) =
  struct
    let open_file secret filename = 
      if (Sys.file_exists filename) then
        let rin = (R.open_in secret filename) in
        try
          let input s = R.input rin s 0 in
          let lexing = LX.from_function input in
          let serial = SJ.read_log (Y.init_lexer ()) lexing in
          R.close_in_noerr rin ;
          L.from_serial serial
        with
        | e -> R.close_in_noerr rin ; raise e
      else
        L.empty ()
    
    let flush writer bo size =
      R.output writer bo.BO.o_s 0 bo.BO.o_len ;
      bo.BO.o_len <- 0 ;
      if size > bo.BO.o_max_len then
        BO.really_extend bo size
    
    let write_file secret filename log =
      let roc = (R.open_out secret filename) in
      let flusher = (flush roc) in
        try
          let boc = BO.create ~make_room:flusher 4096 in
          SJ.write_log boc (L.to_serial log) ;
          BO.flush_channel_writer boc ;
          R.close_out_noerr roc
        with
        | e -> R.close_out_noerr roc ; raise e
  end

module Plain =
  struct
    type in_channel = Pervasives.in_channel

    type out_channel = Pervasives.out_channel

    let open_in secret filename = Pervasives.open_in filename

    let input = Pervasives.input

    let close_in_noerr = Pervasives.close_in_noerr

    let open_out secret filename = Pervasives.open_out filename

    let output = Pervasives.output

    let close_out_noerr = Pervasives.close_out_noerr
  end

module Gziped =
  struct
    let bsize = 1
    let p_to_c _ = ("",Z.compress ())
    let c_to_p _ = ( 0 , (fun s -> Z.uncompress ()))
  end

module Aes_encrypted =
  struct
    let key secret =
      let hash = H.sha256 () in
      hash#add_string secret ;
      hash#result

    let bsize = 16

    let p_to_c secret =
      let iv = R.string R.secure_rng 16 in
      (iv,CC.aes ~iv:iv (key secret) ~pad:P.length CC.Encrypt)

    let c_to_p secret =
      (16, (fun iv -> CC.aes ~iv:iv (key secret) ~pad:P.length CC.Decrypt))
  end

module Cryptolib (R : Representation) (T : Transform) =
  struct
    type in_channel = R.in_channel * C.transform

    type out_channel = R.out_channel * C.transform

    let remain = ref 0

    let buf = B.create 4096
    let buf_pos = ref None

    let open_in secret filename =
      let (pre_len,f) = T.c_to_p secret in
      let ic = R.open_in secret filename in
      let pre = String.make pre_len ' ' in
      let pre_len = ref pre_len in
      let pos = ref 0 in
      while !pre_len > 0 do
        let read = R.input ic pre !pos !pre_len in
        pre_len := !pre_len - read ;
        pos := !pos + read
      done ;
      (ic, f pre)

    let rec input_block_multiple ic s pos n =
      let n = n - (n mod T.bsize) in
      let r_n = R.input ic s pos n in
      let m = r_n mod T.bsize in
      if m != 0 then
        r_n + input_block_multiple ic s (pos + r_n) m
      else
        r_n

    let rec input (ic,trans) s pos n =
      match !buf_pos with
      | Some p ->
          let l = B.length buf in
          let to_read = (min n (l - p)) in
          B.blit buf p s pos to_read ;
          let pos = p + to_read in
          buf_pos :=
            if pos == l then
              None
            else
              Some pos
          ;
          to_read
      | None ->
          B.clear buf ;
          let r_n = input_block_multiple ic s pos n in
          trans#put_substring s pos r_n ;
          trans#flush ;
          let (b_s,b_pos,b_n) = trans#get_substring in
          B.add_substring buf b_s b_pos b_n ;
          buf_pos := Some 0 ;
          input (ic,trans) s pos n

    let close_in_noerr (ic,trans) =
      R.close_in_noerr ic

    let open_out secret filename =
      let (pre,t) = T.p_to_c secret in
      let oc = R.open_out secret filename in
      R.output oc pre 0 (String.length pre) ;
      remain := 0;
      (oc,t)

    let output (oc,trans) (s : string) pos n = 
      let next_remain = (!remain + n) mod T.bsize in
      let to_put = (!remain + n) - next_remain in
      remain := next_remain ;
      trans#put_substring s pos to_put ;
      trans#flush ;
      while trans#available_output != 0 do
        let (s,pos,n) = trans#get_substring in
        R.output oc s pos n
      done ;
      trans#put_substring s (pos + to_put) !remain

    let close_out_noerr (oc,trans) =
      trans#finish ;
      while trans#available_output != 0 do
        let (s,pos,n) = trans#get_substring in
        R.output oc s pos n
      done ;
      R.close_out_noerr oc
      
  end

(* module Implementation = Make(Plain) *)
(* module Implementation = Make(Cryptolib(Plain)(Gziped)) *)
module Implementation = Make(Cryptolib(Plain)(Aes_encrypted))

let open_file = Implementation.open_file

let write_file = Implementation.write_file
