open Yojson.Basic.Util

type transition = {
	name : string;
	read : string;
	to_state : string;
	write : string;
	action : string;
}

type jsonHeaders = {
	name : string;
	alphabet : string array;
	blank : string;
	states : string array;
	initial : string;
	finals : string array;
	transitions : transition array;
}

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

let startAlgo jsonContent (input : char array) =
	let rec runAlgo jsonContent (input : char array) state index =
		let transitions = jsonContent.transitions in
		let currTransition = findTransition transitions state input.(index) in
		input.(index) <- currTransition.write.[0]; 
		if currTransition.to_state = "HALT" then
			input
		else
			runAlgo jsonContent input currTransition.to_state (if currTransition.action = "LEFT" then index - 1 else index + 1)
	in
	runAlgo jsonContent input jsonContent.initial 0

let () =
	let args = checkArgs () in
	let fileContent = readFile args.(1) in
	let jsonContent = Yojson.Basic.from_string fileContent in
	let jsonContent = makeRecord jsonContent in
	let input = Array.init (String.length args.(2)) (fun i -> args.(2).[i]) in
	let result = startAlgo jsonContent input in
	Array.iter (fun x -> Printf.printf "%c" x) result;

