
open Types

let stringSplit str del counter =
	let result = Array.make counter " " in
	let index = ref 0 in
	let strr = ref "" in
	String.iter (fun x ->
		if x = del then begin 
			result.(!index) <- !strr;
			incr index;
			strr := ""
		end
		else if x = '\n' then begin () end
		else begin
			strr := (!strr) ^ String.make 1 x
		end; 
	) str;
	result.(!index) <- !strr;
	result
	
let startAlgoo transitions input =
	let rec runAlgo (input : char array) state index =
		Printf.printf "[";
		let currTransition = Utils.findTransition transitions state input.(index) in
		let helper = ref 0 in
		Array.iter (fun x ->
			if !helper = index then
				Printf.printf "<%c>" x
			else begin
				Printf.printf " %c " x
			end;
			incr helper
		) input;
		Printf.printf "] [%s %c %s]\n" currTransition.to_state currTransition.write.[0] currTransition.action;
		input.(index) <- currTransition.write.[0]; 
		if "h" = currTransition.to_state = true then
			input
		else
			runAlgo input currTransition.to_state (if currTransition.action = "L" then index - 1 else index + 1)
	in
	runAlgo input transitions.(0).name 0

let startMachine fileContent input =
  let counter = ref 1 in
  String.iter (fun x -> if x = '&' then begin incr counter end;) fileContent;
  let myarray : string array = stringSplit fileContent '&' !counter in
  let transitions : transition array = Array.init !counter (fun x ->
    {
      name = String.make 1 myarray.(x).[0];
      to_state = String.make 1 myarray.(x).[2];
      action = String.make 1 myarray.(x).[String.length myarray.(x) - 1];
      write = String.make 1 myarray.(x).[6];
      read = String.make 1 myarray.(x).[4];
    }
  ) in
  let result = startAlgoo transitions input in
  Array.iter (fun x -> Printf.printf "%c" x) result