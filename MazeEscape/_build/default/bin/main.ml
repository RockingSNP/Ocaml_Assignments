open Mazesolver

let n = let args = Sys.argv in 
if Array.length args <> 3 then failwith"Provide 2 Argument ,Coordiantes of  starting point X & Y" else (int_of_string(args.(1)),int_of_string(args.(2)));;

let () = (get_matrix "maze") |> start_from n |> print_stack2list ;;
