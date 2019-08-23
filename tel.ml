open Telegraph.Morse

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
