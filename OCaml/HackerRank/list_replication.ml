let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []


let rec g x n = 
    match n with
     | 0 -> []
     | _ -> x ::  (g x (n - 1));;

let f n arr = List.concat ( List.map (fun x -> g x n)  arr ) ;;

let () =
    let n::arr = read_lines() in
    let ans = f n arr in
    List.iter (fun x -> print_int x; print_newline ()) ans;;
