(* returns an array of n elements *)
let make_array n = List.init n (fun x -> x)

let () =
    let n = int_of_string (read_line ()) in
    let arr = make_array n in
    List.iter ( Printf.printf "%d " ) arr
