let rec cc a d acc = 
if a<0 then failwith "Invalid Input"
else if a=0 then acc
else match d with
| [] -> []
| h::t ->
if a>=h
then (cc (a-h) d (h :: acc)) @ (cc a t acc)
else (cc a t acc);;

let rec sumList lst = 
match lst with 
|[]->0
| h::t-> h + sumList t ;;

let rec breaklist amount lst acc = 
match lst with
|[]->[]
|h::t-> if (sumList (h:: acc) = amount)
  then (h::acc) :: breaklist amount t [] else if (sumList (h:: acc) < amount)
  then breaklist amount t (h::acc) 
else breaklist amount t [];;
let cch a d = breaklist a (cc a (List.rev(List.sort compare d)) []) [];;