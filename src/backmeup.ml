
(* The following function maps Unix standard output to a string *)
let syscall = fun cmd ->
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let _ = Unix.close_process (ic, oc) in
  (Buffer.contents buf)

(* Set backup target *)
let target = match Array.length Sys.argv with
  | 1 -> "/dev/sdc"
  | _ -> Sys.argv.(1)

(* Set backup mode: 
  0 - incremental backup to a single directory
  1 - backup to a directory named by the current date *)
let mode = match Array.length Sys.argv with
  | 1 -> 0
  | 2 -> 0
  | _ -> int_of_string Sys.argv.(2)

(* Dismount if already mounted *)
let dismount = "truecrypt -t -d " ^ target
let force_dismount = Unix.system (dismount ^ " 2> /dev/null")
let _ = force_dismount

(* Function to draw a message box before exiting terminal window *)
let terminate = fun termination_message ->
  let top = Tk.openTk () in
  Wm.title_set top "Done";
  let bkg = "#DBDBDA" in
  Wm.geometry_set top "180x60";
  Toplevel.configure 
    ~background:(`Color bkg) 
    top;
  Focus.set top;
  let msg = Label.create 
    ~text:termination_message
    ~font:"Sans 10"
    ~foreground:`Black 
    ~background:(`Color bkg) 
    top in
  Tk.pack [msg] ~side:`Top;
  let ok = Button.create 
    ~text:"Ok" 
    ~command: Tk.closeTk
    top in
  Tk.pack [ok] ~side:`Top; 
  Tk.mainLoop ()

(* Check that the prerequisite programs are installed *)
let check_version = fun program ->
  match (Unix.system (program ^ " --version  1> /dev/null")) with
  | (Unix.WEXITED 0) -> 
    ignore (Unix.system (program ^ " --version | head -1"))
  | _ -> terminate (program ^ " not found"); exit 1
let () = 
  check_version "truecrypt -t";
  check_version "rsync";
  check_version "gpg-zip"
let () = print_endline ("Executing shell: " ^ (Unix.getenv "SHELL"))

(* Reads a text file and outputs a list of strings *)
let read_text_file  = fun text_file_name ->
  let buffer_size = 8192 in
  let buffer = String.make buffer_size ' ' in
  let fd = Unix.openfile text_file_name [Unix.O_RDONLY] 0 in
  let rec loop str =
    match (Unix.read fd buffer 0 buffer_size) with
      | 0 -> (Unix.close fd); str
      | r -> let p = if buffer.[r-1] = '\n' then r-1 else r in
        loop (str ^ (String.sub buffer 0 p)) in
  Str.split (Str.regexp "\n") (loop "") 

(* Define initial system information and strings *)
let t = Unix.time ()
let ltm = Unix.localtime t
let host_name = Unix.gethostname ()
let log_name = Unix.getenv "LOGNAME"
let date_str = Printf.sprintf "%s:%s:%d.%02d.%02d"host_name log_name 
    (1900 + ltm.Unix.tm_year) (1 + ltm.Unix.tm_mon) ltm.Unix.tm_mday
let home_dir = Unix.getenv "HOME"
let local_dir = home_dir ^ "/.backup"
let exclusion_file = home_dir ^ "/.exclude_from_backup"
let inclusion_file = home_dir ^ "/.include_in_backup"
let param_file = home_dir ^ "/.backup_parameters"
let mnt_dir = "/media/truecrypt_x"
let backup_dir = mnt_dir ^ if mode=1 then "/" ^ date_str else "" 
let ws = " " 

(* Define the default alpabetical order *)  
let order = "0123456789abcdefghijklmnopqrstuvwxyz" 

(* Check that the mount directory doesn't already exist *)
(* This bombs under sid as of 18 Jan 2016. Commented out for now.
let _ = match Sys.file_exists mnt_dir with
  | false -> ()
  | true -> terminate ("Mount point exists"); exit 1
*)

(* If no parameter file exists then create a default one *)
(* file size *)          let kilobytes = 153781 
(* file name length *)   let name_length = 12   
let _ = if Sys.file_exists param_file then
  Unix.WEXITED 0
else
  let param_str = "\"# Modify input values as desired\n" 
    ^  "kilobytes\t" ^ (string_of_int kilobytes) ^ "\n"  
    ^ "name_length\t" ^ (string_of_int name_length) ^ "\"" in 
  Unix.system ("echo " ^ param_str ^ " > " ^ param_file)

(* read parameter file *)
let lines = read_text_file param_file

(* Strip comment lines*)
let lines = List.filter (fun x -> Str.first_chars x 1 <> "#") lines

(* Substitute space character for tab *)
let r = Str.regexp "\t"
let lines = List.map (Str.global_replace r " ") lines

(* Split words *)
let r = Str.regexp " "
let lines = List.map (Str.split r) lines

(* Eliminate blank characters from the lists *)
let lines = List.map (List.filter (fun x -> x <> "")) lines

(* Function to read an integer parameter from a list of strings *)
let get_int param_name default_value data_list =
let params_list = List.map 
  (fun x -> if (List.nth x 0) = param_name 
  then int_of_string (List.nth x 1) else -1) data_list in
let params_list = List.filter (fun x -> x <> -1) params_list in
match (List.length params_list) with
  | 0 -> default_value
  | 1 -> (List.nth params_list 0)
  | _ -> print_endline ("Error: " ^ param_name ^ 
    " multiply defined in " ^ param_file); 
    terminate "Format error in input file";
    exit 1

(* Read input parameters from list *)
let kilobytes = get_int "kilobytes" kilobytes lines
let name_length = get_int "name_length" name_length lines

(* If inclusion file does not exist, then create one *)
let line = home_dir
let _ = if not (Sys.file_exists inclusion_file) then 
  Unix.system ("echo " ^ line ^ " > " ^ inclusion_file)
else
  Unix.WEXITED 0

(* read inclusion file *)
let lines = read_text_file inclusion_file
let targ_dir = String.concat ws lines

(* make certain that every item in inclusion file exists *)
let lines = List.filter (fun x -> x <> "") lines
let () = match List.for_all (fun x -> Sys.file_exists x) lines with 
  | true -> ()
  | false -> terminate "Nonexistent backup item"; exit 1

(* If items owned by others are backed-up, must use sudo *)
let owners = List.map (fun x -> syscall ("stat --printf=%g " ^ x)) lines
let my_id = Unix.getuid ()
let sudo = match List.for_all (fun x -> int_of_string x = my_id) owners with 
  | true -> ""
  | false -> "sudo "

(* If exclusion file does not exist, then create one *)
let line = local_dir ^ "/*"
let _ = if not (Sys.file_exists exclusion_file) then 
  Unix.system ("echo " ^ line ^ " > " ^ exclusion_file)
else
  Unix.WEXITED 0

(* Make certain that the local directory path is excluded 
   from backup in order to prevent backing up backup files*)
let patterns_excluded = read_text_file exclusion_file
let lines = List.filter (fun x -> x = line) patterns_excluded
let _ = match (List.length lines) > 0 with 
  | true -> Unix.WEXITED 0
  | false -> print_endline ("Adding " ^ line ^ " to " ^ exclusion_file); 
    Unix.system ("echo " ^ line ^ " >> " ^ exclusion_file)

(* Function to generate random string of characters *)
let () = Random.self_init () 
let rec random_address sz prefix =
  let b = String.length order in
  match (sz = (String.length prefix)) with
  | true  -> prefix
  | false -> let n = (Random.int b) in
    random_address sz (prefix ^ (String.make 1 order.[n]))

(* Draw a Gtk GUI window to retrieve encryption password and other inputs *)
let p_ref = ref ""
let p_exec = fun z -> (p_ref := z)
let top = Tk.openTk ()
let bkg = "#DBDBDA"
let () = Wm.title_set top "Home Backup"
let () =  Wm.geometry_set top "300x100"
let () = Toplevel.configure 
  ~background:(`Color bkg) 
  top
let msg = Label.create 
  ~text:"Encryption Password" 
  ~font:"Sans 12"
  ~foreground:`Black 
  ~background:(`Color bkg) 
  top
let () = Tk.pack [msg] ~side:`Top
let v = Textvariable.create ()
let () = Textvariable.set v !p_ref
let passwd = Entry.create 
  ~width:30 
  ~relief:`Sunken 
  ~textvariable:v
  ~show:(char_of_int 42) 
  ~foreground:`Black top
let () = Tk.pack [passwd] ~side:`Top

(* toggle the online backup *)
let o_ref = ref "OFF"
let m_exec = fun z -> (o_ref := z)
let online = Textvariable.create()
let () = Textvariable.set online !o_ref
let online_button = Checkbutton.create
  ~background:(`Color bkg)
  ~foreground:`Black
  ~activebackground:(`Color bkg)
  ~activeforeground:`Black
  ~highlightbackground:(`Color bkg)
  ~text:"Create Files for Upload"
  ~font:"Sans 10"
  ~variable:online
  ~onvalue:"ON" 
  ~offvalue:"OFF"
  ~command: (fun () -> m_exec (Textvariable.get online))
  top
let () = Tk.pack [online_button] ~side:`Top

(* Draw the OK button *)
let ok = Button.create 
  ~text:"Ok" 
  ~command: (fun () -> p_exec (Entry.get passwd); Tk.closeTk ())
  top
let () = Tk.pack [ok] ~side:`Top
let () = Focus.set passwd
let () = Tk.mainLoop ()
let password = !p_ref
let p_ref = ref ""

(* Function to generate files for online backup *)
let gpg_backup = fun () -> 
  let file_suffix = "" in
  let bucket = random_address name_length "" in
  let local_backup_dir = local_dir ^ "/" ^ bucket in
  let gpgzip_cmd = "cd " ^ local_backup_dir ^ "\n"
    ^ sudo 
    ^ "gpg-zip  --gpg-args \"--batch --passphrase " ^ password 
    ^ " \" -c " ^ targ_dir ^ " -v -X " ^ exclusion_file 
    ^ " | /home/pluto/bin/random_split" in
(*
    ^ " | split -a4 -b" ^ string_of_int (kilobytes) ^ "K  - "
    ^ local_backup_dir ^ "/_" in
*)

  (* Make sure the .backup directory exists *)
  let () = if (Sys.file_exists local_dir) then 
    ()
  else 
    try
      (Unix.mkdir local_dir 0o755)
    with Unix.Unix_error(err, fun_name, arg) ->
      prerr_string Sys.argv.(0);
      prerr_string ": \"";
      prerr_string fun_name;
      prerr_string "\" failed";
      if String.length arg > 0 then begin
        prerr_string " on \"";
        prerr_string arg;
        prerr_string "\""
      end;
      prerr_string ": ";
      prerr_endline (Unix.error_message err );
      exit 2 in

  (* Execute gpgzip unix shell command *)
  let gpgzip_error = 
    try
      Unix.system ("mkdir " ^ local_backup_dir ^ "\n" ^ gpgzip_cmd)
    with Unix.Unix_error(err, fun_name, arg) ->
      prerr_string Sys.argv.(0);
      prerr_string ": \"";
      prerr_string fun_name;
      prerr_string "\" failed";
      if String.length arg > 0 then begin
        prerr_string " on \"";
        prerr_string arg;
        prerr_string "\""
      end;
      prerr_string ": ";
      prerr_endline (Unix.error_message err );
      exit 2 in

  (* generate a sorted list of random file names *)
  let files1 = Sys.readdir local_backup_dir in
  let nfiles = Array.length files1 in
  let random_names = List.map (random_address name_length)
    ((fun n -> Array.to_list (Array.make n "")) nfiles) in
  let sort lst = List.sort 
     (fun a b -> if a>b then 1 else if a=b then 0 else -1) lst in
  let random_names = sort random_names in
  let random_names = List.map 
    ((fun a c -> (a ^ "/" ^ c )) local_backup_dir) random_names in
  let file_suffix = Str.global_replace (Str.regexp "^[.]") "" file_suffix in
  let random_names = List.map 
    ((fun a b -> match a with "" -> b | _ -> b ^ "." ^ a)
    file_suffix) random_names in

  (* make certain that the randomly created file names are unique *)
  let rec unique_list z =
    match z with
    | [] -> []
    | h::t ->
      match (List.exists (fun a -> a = h) t) with  
        | true -> unique_list t
        | false -> h::(unique_list t) in 
  let () = match (unique_list random_names) = random_names with
    | true -> ()
    | false -> print_endline "\n**Error: Duplicate file names found.";
      print_endline "Try increasing the parameter name_length.";
      terminate "Duplicate file names"; 
      exit 1 in

  (* rename the original file list to the random names *)
  let file_list = Array.to_list files1 in
  let file_list = sort file_list in
  let file_list = List.map 
    ((fun a b ->  (a ^ "/" ^ b)) local_backup_dir) file_list in
  List.iter2 
    (fun a b -> Sys.rename a b) file_list random_names;
  gpgzip_error

(* Create a new summary file in the home directory *)
let summary_fp = (fun fname -> Unix.openfile fname 
  [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666) 
  (home_dir ^ "/.backup_summary")
let line1 = date_str ^ "\n"
let _ = Unix.single_write summary_fp line1 0 (String.length line1)


let mount = "truecrypt -t -k \"\" --protect-hidden=no " 
  ^ target ^ ws ^ mnt_dir ^ " --password=" ^ password

(* backup command on mounted truecrypt volume *)
let rsync = sudo ^ " rsync -avzR "

  (* uncomment the following line to delete all empty directories from backup *)
  (* ^ " -m --prune-empty-dirs " *)

  (* uncomment the following line to rely on checksum rather than date/time *)
  ^ " -c --checksum " 

  (* this omits excluded and extraneous files from target and omits the excluded
     files from backing up *)
  ^ " -e --delete --delete-excluded  --exclude-from '"
  ^ exclusion_file 

  ^"'" ^ ws ^ targ_dir ^ ws ^ backup_dir

let termination_msg = match (Unix.system mount) with
  | (Unix.WEXITED 0) -> begin match !o_ref with
    | "OFF" -> begin match (Unix.system rsync) with 
      | (Unix.WEXITED 0) ->  begin match (Unix.system dismount) with
        | (Unix.WEXITED 0) -> "Backup Complete"
        | _ -> "Dismount Error"
        end
      | _ -> "Rsync Failed" 
      end
    | _ -> begin match (Unix.system dismount) with 
      | (Unix.WEXITED 0) ->  begin match (gpg_backup ()) with
        | (Unix.WEXITED 0) -> "Upload Files Created"
        | _ -> "Error Writing Backup"
        end
      | _ -> "Dismount Failed"
      end
  end
  |  _  -> "Error Mounting Device"

(* Log statistics *)
let t_end = Unix.time ()
let elapsed_time = Printf.sprintf "%.1f Minutes Elapsed" ((t_end -. t) /. 60.)
let () = ignore (Unix.system ("echo " ^ elapsed_time))
let () = ignore (Unix.system ("echo " ^ date_str))
let () = Unix.close summary_fp
let () = terminate termination_msg
