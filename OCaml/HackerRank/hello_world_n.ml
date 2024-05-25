(* let rec iter n = if n != 0 then begin
        print_string "Hello World\n";
        iter (n - 1);
    end;;
 
iter (read_int()) *)

List.iter (fun _ -> print_string "Hello World\n") (List.init (read_int()) (fun x -> x));;
