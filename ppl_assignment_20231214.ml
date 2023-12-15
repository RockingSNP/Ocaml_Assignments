(* Assignment problem along with instructions appears towards the end
   of this file *)


(* Possible solutions to some problems given in previous classes. Just
   remember that these are only possible solutions for you to study
   and get some ideas from. There can be many other ways to solve
   these problems, possibly more efficient ones.*)

(* [cch amount denominations] returns all the possible ways in which
   [amount] can be produced using the [denominations] list *)
let cch (amount:int) (denominations: int list): int list list =
  let rec helper a d acc =
    if a<0 then failwith "Invalid Input"
    else if a=0 then [acc]
    else match d with
      | [] -> [[]]
      | h::t ->
        if a>=h
        then (helper (a-h) d (h::acc)) @ (helper a t acc)
        else helper a t acc
  in helper amount denominations [] |> List.filter (fun x -> x<>[]);;


let rec stack2list s = match Stack.pop_opt s with
  | None -> []
  | Some v -> v::stack2list s;;


(* A stack based implementation of cch *)
let cchstack (amount:int) (denominations:int list): int list list =
  let res = Stack.create () in
  let rec cch a d acc =
    if a<0 then failwith "Invalid Input"
    else if a=0 then (Stack.push acc res)
    else match d with
      | [] -> ()
      | h::t -> (if a>=h
                 then ((cch (a-h) d (h::acc));(cch a t acc))
                 else (cch a t acc)) in
  cch amount denominations []; stack2list res;;


(* A more compact version of cchstack *)
let cchstack2 amount denominations =
  let (res:(int*int) list Stack.t) = Stack.create () in
  let rec cch a d acc =
    if a<0 then failwith "Invalid Input"
    else if a=0 then Stack.push acc res (* we are done *)
    else match d with
      | [] -> () (* solution not possible *)
      | h::t -> let q=a/h in
        (for i=1 to q; do
           cch (a-(h*i)) t ((h,i)::acc)
         done); (cch a t acc)
  in cch amount denominations []; stack2list res;;


(* [scch amount denominations] returns the smallest coin changes
   needed to construct [amount].

   The result is a list of tuples of the form (denomination,
   number) *)
let scch amount denominations =
  let rec helper a d acc =
    if a<0
    then failwith "Invalid Input"
    else if a=0
    then acc
    else match d with
      | [] -> [] (* solution not possible *)
      | h::t ->
        (if a>=h
        then helper (a mod h) t ((h,a/h)::acc)
        else helper a t acc)
  in helper amount denominations [];;


(* [time f x y] returns (as float) the amount of time required to
   execute f with x and y as its parameters *)
let time f x y =
  let start = Sys.time() in
  let stop = ignore(f x y);Sys.time() in
  stop -. start;;

(* Note: stack_limit in utop can be changed with
   Gc.set { (Gc.get ()) with stack_limit=<value> }
*)


(* Now, see the difference in computation times for cch and cchstack/cchstack2 *)

(* Assignment:

   Make the time function into a writer monad.  Perhaps, go to lab if
   available and solve this problem on the computer (Monday and
   Tuesday, during lab classes) or just try to do this on your
   own.

   Submission:

   Submit a hand written copy (stapled, no need to use channel file,
   etc.) with a cover page with your name, roll, registration no.,
   subject name, subject code, year, and date of assignment, and the
   above problem statement in it. Your code (solution) should be from
   the second page onwards, properly formatted (even though hand
   written), with meaningful comments placed in appropriate places so
   that its easy to understand for others.

   Written solution to be submitted on Thursday (21/12/2023 right
   after the CA3 class test for lateral entry students), and the
   actual running solution should be demonstrated by you on the next
   available lab classes.

   A valid solution to this above assignment MUST include:

   1. Code for the Monad as mentioned in the problem statement

   2. Test cases where you use your monad module to time the coin
   change problem (both the list based and stack based versions as
   given at the beginning of this file), clearly showing the form of
   the outputs (along with the timing results for both versions).

   3. A conclusion indicating how big is the average running time
   difference between the two implementations.

*)
