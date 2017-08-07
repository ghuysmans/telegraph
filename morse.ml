(* unfortunately, this is not part of the standard library... *)
let fold f init s =
  let l = String.length s in
  let rec g i acc =
    if i = l then
      acc
    else
      g (i+1) (f acc s.[i])
  in
  g 0 init

let decode tree word =
  let move (i, w) = function
    | '.' -> (* left *)
      2 * i, w
    | '-' -> (* right *)
      2 * i + 1, w
    | ' ' | '\n' as c -> (* complete char *)
      let i' = i - 2 (* 0-based, root not stored *) in
      1, (* back to the root *)
      w ^ String.sub tree i' 1 ^ (if c = '\n' then " " else "")
    | sym ->
      failwith ("unexpected character: " ^ String.make 1 sym)
  in
  fold move (1, "") word |> snd

let encode_symbol tree symbol =
  let rec bin s n =
    if n = 1 then (* root *)
      s
    else
      let lbl = if n land 1 = 0 then "." else "-" in
      bin (lbl ^ s) (n lsr 1)
  in
  bin "" (String.index tree symbol + 2 (* same reason as -2 in decode *))

let encode tree word =
  fold (fun acc l -> encode_symbol tree l :: acc) [] word |>
  List.rev |>
  String.concat " "


let basic = "ETIANMSURWDKGOHVF-L-PJBXCYZQ"
let extended = "ETIANMSURWDKGOHVF-L-PJBXCYZQ--54-3---2--+----16=/-----7---8-90"

let () =
  let usage code =
    prerr_endline ("usage: " ^ Sys.argv.(0) ^ " [--ext] [--decode] ...");
    exit code
  in
  let tree, enc, args =
    let parse (tree, enc, arg) = function
      | "--help" | "-h" -> usage 0
      | "--ext" -> extended, enc, arg
      | "--decode" -> tree, false, arg
      | _ as s -> tree, enc, s :: arg
    in
    let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
    let tree, enc, args = Array.fold_left parse (basic, true, []) args in
    tree, enc, List.rev args
  in
  let output =
    if enc then
      List.map (fun s -> encode tree (String.uppercase_ascii s)) args
    else
      List.map (fun s -> decode tree (s ^ " ")) args
  in
  List.iter print_endline output
