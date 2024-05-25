let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let rec f arr o acc = 
    match arr with
    | [ ] -> acc
    | x :: xs -> if o then f xs false acc else f xs true (x::acc);;

let () =
    let arr = read_lines() in
    let ans = List.rev ( f arr true [ ] ) in
    List.iter (fun x -> print_int x; print_newline ()) ans;;
