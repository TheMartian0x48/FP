let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let rec g arr n a = 
    match arr with
    | [] -> a
    | x :: xs -> if x < n then g xs n (x :: a) else g xs n a;;

let () = 
    let n::arr = read_lines() in
    let ans = List.rev ( g arr n [] ) in 
    List.iter (fun x -> print_int x; print_newline ()) ans;;


