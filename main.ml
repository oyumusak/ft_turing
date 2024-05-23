open Yojson.Basic.Util


type transition = {
	name : string;        (* Current state name *)
	read : string;        (* Character read from the tape *)
	to_state : string;    (* State to transition to *)
	write : string;       (* Character to write on the tape *)
	action : string;      (* Action to take: "LEFT" or "RIGHT" *)
}


type jsonHeaders = {
	name : string;               		(* Name of the Turing machine *)
	alphabet : string array;     		(* Alphabet of the Turing machine *)
	blank : string;              		(* Blank symbol *)
	states : string array;       		(* States of the Turing machine *)
	initial : string;            		(* Initial state *)
	finals : string array;       		(* Final (halting) states *)
	transitions : transition array;	(* Transition rules *)
}


(* Converts JSON representation of transitions into an array of transition records. 
	Each record contains the name of the current state, the character read from the tape, 
	the state to transition to, the character to write on the tape, and the action to take. 
*)

let initTransitions json =
	json |> to_assoc |> List.map (fun (state, trans_list) ->
		trans_list |> to_list |> List.map (fun trans ->
		{
			name = state;
			read = trans |> member "read" |> to_string;
			to_state = trans |> member "to_state" |> to_string;
			write = trans |> member "write" |> to_string;
			action = trans |> member "action" |> to_string;
		}
	)
	) |> List.flatten |> Array.of_list


(* Parses JSON content into a jsonHeaders record.*)

let makeRecord jsonContent =
	{
		name = jsonContent |> member "name" |> to_string;
		alphabet = jsonContent |> member "alphabet" |> to_list |> List.map to_string |> Array.of_list;
		blank = jsonContent |> member "blank" |> to_string;
		states = jsonContent |> member "states" |> to_list |> List.map to_string |> Array.of_list;
		initial = jsonContent |> member "initial" |> to_string;
		finals = jsonContent |> member "finals" |> to_list |> List.map to_string |> Array.of_list;
		transitions = initTransitions (jsonContent |> member "transitions");
	}


let readFile filePath =
	let inputChannel = open_in filePath in
	let rec read_lines acc =
	try
		let line = input_line inputChannel in
		read_lines (acc ^ line ^ "\n")
	with
	| End_of_file -> acc
	in
	let content = read_lines "" in
	close_in inputChannel;
	content


let checkArgs () =
	let args = Sys.argv in
	let argsLen = Array.length args in
	if argsLen != 3 then begin print_string "You need 3 arg!\n"; exit 0 end;
	args


let findTransition (transitions : transition array) name (read : char) =
	let rec loop i =
		if (transitions.(i).name = name) then
			if (transitions.(i).read = String.make 1 read) then
				transitions.(i)
			else
				loop (i + 1)
		else
			loop (i + 1)
	in
	loop 0

let isFinals finals toState =
	let rec loop i =
		if (finals.(i) = toState) then
			true
		else begin
			if (Array.length finals) > (i + 1) then
				loop (i + 1)
			else
				false
		end
	in
	loop 0


(* Initiates and runs the Turing machine algorithm. *)

let startAlgo jsonContent (input : char array) =
	let rec runAlgo jsonContent (input : char array) state index =
		let transitions = jsonContent.transitions in
		let currTransition = findTransition transitions state input.(index) in
		input.(index) <- currTransition.write.[0]; 
		if (isFinals jsonContent.finals currTransition.to_state) = true then
			input
		else
			runAlgo jsonContent input currTransition.to_state (if currTransition.action = "LEFT" then index - 1 else index + 1)
	in
	runAlgo jsonContent input jsonContent.initial 0




let checkDepends jsonContent inp =
	let blank = jsonContent.blank in
	let lastChCount = ref 0 in
	if (Array.length jsonContent.alphabet) < 3 then begin print_string "alphabet too short"; exit 0 end;
	Array.iter (fun x -> 
		if x = blank.[0] then begin
			print_string "No Blank Char In Input!!!"; exit 0
		end
		else if x = jsonContent.alphabet.((Array.length jsonContent.alphabet) - 1).[0] then begin
			incr lastChCount
		end
		else if (Array.exists (fun y -> y.[0] = x ) jsonContent.alphabet) = false then begin print_string "input characters must be alphabet characters!"; exit 0 end;
		) inp;
	if !lastChCount != 1 then begin print_string "alphabet last char count must be 1"; exit 0 end;
	if inp.((Array.length inp) - 1) != jsonContent.alphabet.((Array.length jsonContent.alphabet) - 1).[0] then begin
		print_string "Alphabet last character must be input last character!";
		exit 0
	end;
	()

let () =
	let args = checkArgs () in
	let fileContent = readFile args.(1) in
	let jsonContent = Yojson.Basic.from_string fileContent in
	let jsonContent = makeRecord jsonContent in
	let input = Array.init (String.length args.(2)) (fun i -> args.(2).[i]) in
	checkDepends jsonContent input;
	let result = startAlgo jsonContent input in
	Array.iter (fun x -> Printf.printf "%c" x) result;

