module L = Log
module G = Gallery
module SJ = Serial_j
module BO = Bi_outbuf
module Y = Yojson.Safe
module LX = Lexing

let open_file (filename: string) : L.t =
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

let write_file (filename: string) (log: L.t) : unit =
    let oc = open_out filename in
    try
      let boc = BO.create_channel_writer oc in
      SJ.write_log boc (L.to_serial log) ;
      (BO.flush_channel_writer boc) ;
      flush oc ;
      close_out oc
    with
      e -> close_out_noerr oc ; raise e
