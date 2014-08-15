module L = Log
module G = Gallery
module SJ = Serial_j
module BO = Bi_outbuf
module Y = Yojson.Safe
module LX = Lexing

let open_file_plain (filename: string) : L.t =
    if (Sys.file_exists filename) then
        let ic = open_in filename in
        try
          let serial = SJ.read_log (Y.init_lexer ()) (LX.from_channel ic) in
          let log = L.from_serial serial in
          close_in_noerr ic ;
          log
        with
          e -> close_in_noerr ic ; raise e
    else
        L.empty ()

let fill_from_bz2 bzic s size =
  Bz2.read bzic s 0 size

let open_file_bz2 (filename: string) : L.t =
    if (Sys.file_exists filename) then
        let ic = open_in filename in
        let bzic = Bz2.open_in ic in
        try
          let serial = SJ.read_log (Y.init_lexer ()) (LX.from_function (fill_from_bz2 bzic)) in
          let log = L.from_serial serial in
          Bz2.close_in bzic ;
          close_in_noerr ic ;
          log
        with
          e -> close_in_noerr ic ; raise e
    else
        L.empty ()

let open_file (filename: string) : L.t =
    open_file_bz2 filename

let write_file_plain (filename: string) (log: L.t) : unit =
    let oc = open_out filename in
    try
      let boc = BO.create_channel_writer oc in
      SJ.write_log boc (L.to_serial log) ;
      (BO.flush_channel_writer boc) ;
      flush oc ;
      close_out oc
    with
      e -> close_out_noerr oc ; raise e

let flush_bz2 bzoc bo size =
  Bz2.write bzoc bo.BO.o_s 0 bo.BO.o_len ;
  bo.BO.o_len <- 0 ;
  if size > bo.BO.o_max_len then
    BO.really_extend bo size

let write_file_gziped (filename: string) (log: L.t) : unit =
    let oc = open_out filename in
    let bzoc = Bz2.open_out oc in
    try
      let boc = BO.create ~make_room:(flush_bz2 bzoc) 4096 in
      SJ.write_log boc (L.to_serial log) ;
      BO.flush_channel_writer boc ;
      Bz2.close_out bzoc ;
      flush oc ;
      close_out oc
    with
      e -> close_out_noerr oc ; raise e

let write_file (filename: string) (log: L.t) : unit =
    write_file_gziped filename log
