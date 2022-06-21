open Eio.Std

module Buf_read = Eio.Buf_read

let use_backtracing_in_old = true

let n = 10_000_000

let data = Cstruct.concat (List.init n (fun _ -> Cstruct.of_string {|"Hello \" world"|}))

module Old = struct
  open Reader

  (* This is the original parser from cohttp-eio, using backtracking. It crashes. *)
  module With_backtracking = struct
    let quoted_pair =
      char '\\'
      *> satisfy (function ' ' | '\t' | '\x21' .. '\x7E' -> true | _ -> false)

    (*-- qdtext = HTAB / SP /%x21 / %x23-5B / %x5D-7E / obs-text -- *)
    let qdtext =
      satisfy (function
          | '\t' | ' ' | '\x21' | '\x23' .. '\x5B' -> true
          | '\x5D' .. '\x7E' -> true
          | _ -> false)

    (*-- quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE --*)
    let quoted_string =
      let dquote = char '"' in
      let+ chars = dquote *> many_till (qdtext <|> quoted_pair) dquote <* dquote in
      String.of_seq @@ List.to_seq chars
  end

  (* This is the parser from the buf_read PR, but using Reader. *)
  module Without_backtracking = struct
    let quoted_char =
      let+ c = any_char in
      match c with
      | ' ' | '\t' | '\x21' .. '\x7E' -> c
      | c -> failwith (Printf.sprintf "Invalid escape \\%C" c)

    (*-- qdtext = HTAB / SP /%x21 / %x23-5B / %x5D-7E / obs-text -- *)
    let qdtext =
      let+ c = any_char in
      match c with
      | '\t' | ' ' | '\x21' | '\x23' .. '\x5B'
      | '\x5D' .. '\x7E' -> c
      | c -> failwith (Printf.sprintf "Invalid quoted character %C" c)

    (*-- quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE --*)
    let quoted_string r =
      char '"' r;
      let buf = Buffer.create 100 in
      let rec aux () =
        match any_char r with
        | '"' -> Buffer.contents buf
        | '\\' -> Buffer.add_char buf (quoted_char r); aux ()
        | c -> Buffer.add_char buf c; aux ()
      in
      aux ()
  end

  let p =
    if use_backtracing_in_old then With_backtracking.quoted_string
    else Without_backtracking.quoted_string

  (* Avoid memory leak! *)
  let p = p <* commit

  let test () =
    let r = Reader.create 1024 (Eio.Flow.cstruct_source [data]) in
    let i = ref 0 in
    print_endline (p r);    (* Do one test run *)
    try
      while true do
        ignore (p r : string);
        incr i;
      done; assert false
    with
    | End_of_file
    | Reader.Parse_failure _ -> !i
end

module New = struct
  open Buf_read
  open Buf_read.Syntax

  let quoted_char =
    let+ c = any_char in
    match c with
    | ' ' | '\t' | '\x21' .. '\x7E' -> c
    | c -> failwith (Printf.sprintf "Invalid escape \\%C" c)

  (*-- qdtext = HTAB / SP /%x21 / %x23-5B / %x5D-7E / obs-text -- *)
  let qdtext =
    let+ c = any_char in
    match c with
    | '\t' | ' ' | '\x21' | '\x23' .. '\x5B'
    | '\x5D' .. '\x7E' -> c
    | c -> failwith (Printf.sprintf "Invalid quoted character %C" c)

  (*-- quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE --*)
  let quoted_string r =
    char '"' r;
    let buf = Buffer.create 100 in
    let rec aux () =
      match any_char r with
      | '"' -> Buffer.contents buf
      | '\\' -> Buffer.add_char buf (quoted_char r); aux ()
      | c -> Buffer.add_char buf c; aux ()
    in
    aux ()

  let p = quoted_string

  let test () =
    let r = Buf_read.of_flow ~max_size:max_int ~initial_size:1024 (Eio.Flow.cstruct_source [data]) in
    let i = ref 0 in
    print_endline (p r);    (* Do one test run *)
    try
      while true do
        ignore (p r : string);
        incr i;
      done; assert false
    with End_of_file -> !i
end  

let time name fn =
  let t0 = Unix.gettimeofday () in
  let count = fn () in
  assert (count > 0);
  let t1 = Unix.gettimeofday () in
  traceln "%s: took %.2fs to parse %d items" name (t1 -. t0) count

let () =
  Eio_main.run @@ fun _ ->
  traceln "Comparing Reader (backtracking=%b) with Eio.Buf_read..." use_backtracing_in_old;
  time "buf_read" New.test;
  time "reader" Old.test;
  time "buf_read" New.test;
  time "reader" Old.test;
