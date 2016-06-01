let () = Random.self_init ()

let max_size = 1000000    (* max file size *)
let min_size = 10000      (* min file size *)
let ncalls = 3            (* number of calls to uniform distribution *)
let buffer_size = 8192    (* file read buffer size *)
let nchar = 4             (* number of letters in output file names *)

(* additional parameters used to define file name *)
let low = int_of_char 'a'
let high = int_of_char 'z'
let base = high-low+1
let prefix = "_"
let ints = Array.make nchar 0

let  carry_one = fun x n ->
  let v = x.(n)/base in
  if v>0  then
    (Array.set x n (x.(n) mod base);  Array.set x (n+1) (x.(n+1)+v))
  else ()
let rec carry_all = fun x n ->
  match n+1 < Array.length x with
  | false -> x
  | true -> carry_one x n; carry_all x (n+1)
let next = fun z -> carry_all (Array.set z 0 (z.(0)+1); z) 0
let overflow = fun z -> if z.(Array.length z - 1) > base-1 then
  (print_endline "Not enough characters in file name"; exit 1) else ()
let rec implode = fun int_array n str ->
  match n=(Array.length int_array) with
  | true -> str
  | false -> implode int_array (n+1)
    ((String.make 1 (char_of_int (low+int_array.(n)))) ^ str)
let newname = fun int_array -> prefix ^ (implode int_array 0 "")

let buffer = String.create buffer_size
let fd_in = Unix.stdin 
let open_output = fun fname -> Unix.openfile fname 
  [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666 

(* Approximate a Gaussian dist with multiple calls to uniform dist *)
let mins = Array.make ncalls (min_size/ncalls)
let () = Array.set mins 0 (mins.(0) + (min_size mod ncalls))
let mins = Array.to_list mins
let maxs = Array.make ncalls (max_size/ncalls)
let () = Array.set maxs 0 (maxs.(0) + (max_size mod ncalls))
let maxs = Array.to_list maxs
let one_call = fun mn mx -> mn + (if (mx>mn) then Random.int (mx-mn) else 0) 
let file_size = fun () -> 
  let s = Array.of_list (List.map2 one_call mins maxs) in
  Array.fold_left (+) 0 s

let rec split_back fp n m p =
  let nbytes = min buffer_size (m-p) in
  match nbytes>0 with
  | false -> Unix.close fp;
    let q = file_size () in
    let filename = newname (next ints) in
    split_back (open_output filename) (n+1) q 0 
  | true -> match Unix.read fd_in buffer 0 nbytes with
    | 0 -> ()
    | r -> ignore (Unix.write fp buffer 0 r);
       match p+r >= max_size with
       | true -> Unix.close fp;
         let q = file_size () in
         let filename = newname (next ints) in
         split_back (open_output filename) (n+1) q 0 
       | false -> split_back fp n m (p+r) 
let split = fun fp -> let fp = open_output (newname ints) in
  (split_back fp 1 (file_size ()) 0);
  Unix.close fp

let () = split ()

