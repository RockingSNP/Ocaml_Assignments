let read_file file_name =
  let rec read_lines chan acc =
    match input_line chan with
    | exception End_of_file -> close_in chan; List.rev acc
    | line -> read_lines chan (line :: acc)
  in
  let chan = open_in file_name in
  read_lines chan []

let get_matrix (file_name:string) : int array array =
  let file_content = read_file ("bin/"^file_name^".txt") in
  let parse_row row = 
    row |> String.to_seq |> Array.of_seq |> Array.map (fun c -> int_of_char c - int_of_char '0')
  in
  let mat = 
    file_content |> List.filter (fun x -> x <> "") |> List.map parse_row |> Array.of_list 
  in
mat 

  (*Initiating a New Stack*)
let res : (int * int) list Stack.t = Stack.create()

let stack2list () = 
  let rec helper s acc = 
    match Stack.pop_opt s with 
    |Some v -> helper s (v::acc)
    |None -> acc in 
  helper res []

let start_from (a,b) (matrix:int array array)= 
let row = Array.length(matrix) in
let col = Array.length(matrix.(0)) in
(*saving the matrix in arr for more cases.. *)
let arr = matrix in 
(*Checks whether the given Coordinates resides in Boundary or not.*)
let outLine x y = if x <> a || y <> b then x = (row - 1)|| x = 0 || y = (col - 1) || y = 0 else false in 
if arr.(a).(b) <> 0 then failwith "Not a point of Entry" else
  let rec navi (x,y) acc =
    try
      if arr.(x).(y) <> 0 then () else 
        (*Checking If the Path is already nav'd , if does then placing a BreadCrumb(int:2) ...*)
        if (List.mem (x, y) acc) then (arr.(x).(y) <- 2) else if (outLine x y) then 
          (print_endline("Result Found");(Stack.push ((x,y) :: acc) res)) else
            (*Player Compass : North-East-South-West*)
          ((navi (x,(y + 1)) ((x,y):: acc));(*North*)
            (navi ((x+1),y) ((x,y):: acc));(*East*)
            (navi (x,(y - 1)) ((x,y):: acc));(*South*)
            (navi ((x-1),y) ((x,y):: acc));(*West*)) 
    with Invalid_argument(_) -> ()
  in navi (a,b) [] ;
  stack2list ()

let print_stack2list s2l=
let num = ref 0 in
List.iter (fun pair_list ->
  Printf.printf "Way no. %d -> " (!num +1);
  List.iter (fun (x, y) -> Printf.printf "(%d, %d) " x y) pair_list;
  Printf.printf "\n";
  incr num
) s2l;;