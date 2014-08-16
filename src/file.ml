module L = Log
module G = Gallery
module SJ = Serial_j
module BO = Bi_outbuf
module Y = Yojson.Safe
module LX = Lexing

module type Representation =
  sig
    type in_channel
    type out_channel
    val open_in : string -> in_channel
    val input : in_channel -> string -> int -> int -> int
    val close_in_noerr : in_channel -> unit
    val open_out : string -> out_channel
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
    let open_file filename = 
      if (Sys.file_exists filename) then
        let rin = (R.open_in filename) in
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
    
    let write_file filename log =
      let roc = (R.open_out filename) in
      let flusher = (flush roc) in
        try
          let boc = BO.create ~make_room:flusher 4096 in
          SJ.write_log boc (L.to_serial log) ;
          BO.flush_channel_writer boc ;
          R.close_out_noerr roc
        with
        | e -> R.close_out_noerr roc ; raise e
  end

module Bzipped2 =
  struct

    type in_channel = Bz2.in_channel * Pervasives.in_channel

    type out_channel = Bz2.out_channel * Pervasives.out_channel

    let open_in filename =
      let ic = Pervasives.open_in filename in
      (Bz2.open_in ic,ic)

    let input (bz2ic,ic) = Bz2.read bz2ic

    let close_in_noerr (bz2ic,ic) =
      Bz2.close_in bz2ic ;
      Pervasives.close_in_noerr ic

    let open_out filename =
      let oc = Pervasives.open_out filename in
      (Bz2.open_out oc,oc)

    let output (bz2oc,oc) = 
      Bz2.write bz2oc

    let close_out_noerr (bz2oc,oc) =
      Bz2.close_out bz2oc ;
      Pervasives.close_out_noerr oc
      
  end

(* module Implementation = Make(Pervasives) *)
module Implementation = Make(Bzipped2)

let open_file = Implementation.open_file

let write_file = Implementation.write_file
