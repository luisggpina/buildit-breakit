module L = Log
module G = Gallery
module SJ = Serial_j
module BO = Bi_outbuf
module Y = Yojson.Safe
module LX = Lexing
module C = Cryptokit
module CC = Cryptokit.Cipher
module Z = Cryptokit.Zlib

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

module Gzipped (R : Representation) =
  struct
    type in_channel = R.in_channel * C.transform

    type out_channel = R.out_channel * C.transform

    let open_in secret filename =
      (R.open_in secret filename, Z.uncompress ())

    let input (ic,trans) s pos n =
      let r_n = R.input ic s pos n in
      trans#put_substring s pos r_n ;
      trans#flush ;
      let (b_s,b_pos,b_n) = trans#get_substring in
      let n = min n b_n in
      String.blit b_s b_pos s pos n ;
      n

    let close_in_noerr (ic,trans) =
      trans#finish ;
      R.close_in_noerr ic

    let open_out secret filename =
      (R.open_out secret filename, Z.compress ())

    let output (oc,trans) s pos n = 
      trans#put_substring s pos n ;
      trans#flush ;
      while trans#available_output != 0 do
        let (s,pos,n) = trans#get_substring in
        R.output oc s pos n
      done

    let close_out_noerr (oc,trans) =
      trans#finish ;
      while trans#available_output != 0 do
        let (s,pos,n) = trans#get_substring in
        R.output oc s pos n
      done ;
      R.close_out_noerr oc
      
  end

module AES_Encrypted =
  struct
  end

(* module Implementation = Make(Plain) *)
module Implementation = Make(Gzipped(Plain))
(* module Implementation = Make(AES_Encrypted) *)

let open_file = Implementation.open_file

let write_file = Implementation.write_file
