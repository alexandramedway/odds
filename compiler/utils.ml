(*
 * COMS4115: Odds Utility File
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

(* Return a string representation of file 'file' *)
let str_of_file file =
  let ic = open_in file in
  let try_read () =
    try Some(input_line ic) with End_of_file -> None in
  let rec aux acc = match try_read () with
    | None -> close_in ic; String.concat "\n" (List.rev acc)
    | Some(s) -> aux (s :: acc) in
  aux []

let conclude_program () = "if PLOT:\n\tplt.show()"
