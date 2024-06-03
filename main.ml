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
	if argsLen != 3 then begin print_string "You need 2 arg!\n"; exit 0 end;
	args

let checkFileExtension path extension =
	if String.length extension > String.length path then
		false
	else
		let sub = String.sub path (String.length path - String.length extension) (String.length extension) in
		if sub = extension then true else false


let () =
	let args = checkArgs () in
	let instPath = args.(1) in
	let fileContent = readFile instPath in
	if String.length fileContent = 0 then begin print_string "File cannot be empty!\n"; exit 0 end;
	let input = Array.init (String.length args.(2)) (fun i -> args.(2).[i]) in
	if checkFileExtension instPath ".json" then   
		Machineone.startMachine fileContent input
	else if checkFileExtension instPath ".txt" then
		Machinetwo.startMachine fileContent input
	else
		Printf.printf "Arg 1 must be json or txt!\n";



