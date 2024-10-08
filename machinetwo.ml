open Types
(*
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
	
(* Initiates and runs the Turing machine algorithm. *)
let startAlgoo (transitions : transition array) (input : char array) =
  let inputHitCount = ref (Array.init (Array.length input)(fun _ -> 0)) in
	let rec runAlgo transitions (input : char array) state index =
    let _ = match Utils.findTransitionn transitions state input.(index) with
    | Some transition -> transition
    | None -> print_string "Transition not found!\n"; exit 0
    in
    let currTransition = Utils.findTransition transitions state input.(index) in
    Printf.printf "[";
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
		if ("h" = currTransition.to_state) then begin
      input
    end
		else begin
      if currTransition.action = "LEFT" && index - 1 < 0  then begin
        print_string "indexx LEFT out of bounds\n";
        exit 1
      end
      else if currTransition.action = "RIGHT" && index + 1 > Array.length input then begin
        print_string "indexx RIGHT out of bounds\n";
        exit 1
      end
      else begin
        !inputHitCount.(index) <- !inputHitCount.(index) + 1;
        Array.iteri (fun i x -> 
          if not(i >= index - 1 && i <= index + 1) && (!inputHitCount.(i) - 1 > 0)  then begin !inputHitCount.(i) <- x - 1 end;
        )!inputHitCount;
        if Array.exists (fun x -> x >= 100) !inputHitCount then begin print_string "Infinite loop dedected!!!\n"; exit 0 end;
			  runAlgo transitions input currTransition.to_state (if currTransition.action = "LEFT" then index - 1 else index + 1)
      end;
    end
	in
	runAlgo transitions input transitions.(0).name 0

let alphabetParse (fileContent : string) =
	let alphabet = ref "" in
	if fileContent.[0] != 'a' || fileContent.[1] != '=' then begin print_string "file content must start with \"a=\"\n"; exit 0 end;
	let rec loop (index : int) =
		if index + 1 > (String.length fileContent) then begin print_string "File format error\n"; exit 0 end
		else if index mod 2 = 1 then begin (*tek sayilar virgul olmali*)
			if fileContent.[index] = '\n' then begin !alphabet end
			else if fileContent.[index] = ',' then begin loop (index + 1) end
			else begin print_string "file format error!\n"; exit 0 end;
		end
		else begin
			if fileContent.[index] = 'a' || fileContent.[index] = 's' || fileContent.[index] = ':' || fileContent.[index] = '?' || fileContent.[index] = '#' || fileContent.[index] = 'R' || fileContent.[index] = 'L' then begin
				Printf.printf "alphabet character cannot be <%c>!\n" fileContent.[index];
				exit 0
			end
			else begin
				alphabet := !alphabet ^ String.make 1 fileContent.[index];
				loop (index + 1)
			end;
		end;
	in
	loop 2

let checkAlphabet alp input =
	let counter = ref 0 in
	String.iter (fun x ->
		String.iter (fun y ->
			if x = y then begin incr counter end;
		) alp;
		if !counter > 1 then begin print_string "alphabet characters must be unique\n"; exit 0 end;
		counter := 0
	) alp;
	if alp.[(String.length alp) - 1] != input.((Array.length input) - 1) then begin
		print_string "input last character must be alphabet last character!\n";
		exit 0
	end;
	Array.iter (fun x ->
		if alp.[(String.length alp) - 1] = x then begin incr counter end;
		if !counter > 1 then begin print_string "alphabet last character must be unique in input!\n"; exit 0 end;
	) input;
	()

let findInStr (fileContent : string) (c : char) =
	let rec loop (index : int) =
		if index >= (String.length fileContent) then begin Printf.printf "char %c not found in str\n" c; exit 0 end
		else if fileContent.[index] = c then begin index end
		else begin loop (index + 1) end;
	in
	loop 0

let removeLine (fileContent : string) =
	let newStr = ref "" in
	let index : int = (findInStr fileContent '\n') in
	String.iteri (fun i c -> if i > index then begin newStr := !newStr ^ (String.make 1 c) end;) fileContent;
	!newStr

let stateParse (fileContent : string) =
	let states = ref "" in
	if fileContent.[0] != 's' || fileContent.[1] != '=' then begin print_string "file contentin s= olarak baslaması lazim!\n"; exit 0 end;
	let rec loop (index : int) =
		if index + 1 > (String.length fileContent) then begin print_string "File Format Error\n"; exit 0 end
		else if index mod 2 = 1 then begin (*tek sayilar virgul olmali*)
			if fileContent.[index] = '\n' then begin !states end
			else if fileContent.[index] = ',' then begin loop (index + 1) end
			else begin print_string "file format error!\n"; exit 0 end;
		end
		else begin
			if fileContent.[index] = 'a' || fileContent.[index] = 's' || fileContent.[index] = ':' || fileContent.[index] = '?' || fileContent.[index] = '#' || fileContent.[index] = 'R' || fileContent.[index] = 'L' then begin
				Printf.printf "state karakteri %c olamaz!\n" fileContent.[index];
				exit 0
			end
			else begin
				states := !states ^ String.make 1 fileContent.[index];
				loop (index + 1)
			end;
		end;
	in
	loop 2

let checkStates (transitions : transition array) (states : string) =
	if (String.length states) < 3 then begin print_string "States too short!\n"; exit 0 end;
	if states.[(String.length states) - 1] != 'h' then begin print_string "Last state must be h for halt!\n"; exit 0 end;
	Printf.printf "%c\n" states.[(String.length states) - 1];
	String.iter (fun state ->
		if (Array.exists (fun (trans : transition) -> trans.name.[0] = state ) transitions) = false && state != states.[(String.length states) - 1] then begin
			print_string "state not found in transition\n";
			exit 0
		end;
	) states;
	()


let checkTransitions fileContent =
	let lineCount = ref 0 in
	let rec loop index =
		if index + 1 > (String.length fileContent) then begin
			if !lineCount = 0 then begin print_string "File Format Error!\n"; exit 0 end
			else begin () end;
		end
		else if !lineCount mod 2 = 0 then begin
			if fileContent.[index] != '&' && fileContent.[index + 1] != '\n' then begin
				Printf.printf "Excepted &\\n but has %c%c\n" fileContent.[index] fileContent.[index + 1];
				exit 0
			end;
			incr lineCount;
			loop (index + 2)
		end
		else begin
			if fileContent.[index + 1] != '-' || fileContent.[index + 3] != ':' || fileContent.[index + 5] != '?' || fileContent.[index + 7] != '#' || not(fileContent.[index + 8] = 'R' || fileContent.[index + 8] = 'L') || fileContent.[index + 9] != '\n' then begin
				Printf.printf "Syntax Error\n";
				exit 0
			end;
			incr lineCount;
			loop (index + 10)
		end;
	in
	loop 0

let startMachine fileContent input =
	let alphabet : string = alphabetParse fileContent in
	checkAlphabet alphabet input;
	let fileContent = removeLine fileContent in
	let states = stateParse fileContent in
	let fileContent = removeLine fileContent in
	checkTransitions fileContent;
  let counter = ref 0 in
  String.iter (fun x -> if x = '&' then begin incr counter end;) fileContent;
	let fileContent = removeLine fileContent in
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
	checkStates transitions states;
  let result = startAlgoo transitions input in
  Array.iter (fun x -> Printf.printf "%c" x) result
*)





(*
ABH:

e1.+

&
A1?.>B#R B1?1>B#R -+?1>B#R -e?e>H#L @
*)

(* Initiates and runs the Turing machine algorithm. *)
let startAlgoo (transitions : transition array) (input : char array) =
  let inputHitCount = ref (Array.init (Array.length input)(fun _ -> 0)) in
	let rec runAlgo transitions (input : char array) state index =
    let _ = match Utils.findTransitionn transitions state input.(index) with
    | Some transition -> transition
    | None -> print_string "Transition not found!\n"; exit 0
    in
    let currTransition = Utils.findTransition transitions state input.(index) in
    Printf.printf "[";
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
		if ("H" = currTransition.to_state) then begin
      input
    end
		else begin
      if currTransition.action = "L" && index - 1 < 0  then begin
        print_string "indexx LEFT out of bounds\n";
        exit 1
      end
      else if currTransition.action = "R" && index + 1 > Array.length input then begin
        print_string "indexx RIGHT out of bounds\n";
        exit 1
      end
      else begin
        !inputHitCount.(index) <- !inputHitCount.(index) + 1;
        Array.iteri (fun i x -> 
          if not(i >= index - 1 && i <= index + 1) && (!inputHitCount.(i) - 1 > 0)  then begin !inputHitCount.(i) <- x - 1 end;
        )!inputHitCount;
        if Array.exists (fun x -> x >= 100) !inputHitCount then begin print_string "Infinite loop dedected!!!\n"; exit 0 end;
			  runAlgo transitions input currTransition.to_state (if currTransition.action = "L" then index - 1 else index + 1)
      end;
    end
	in
	runAlgo transitions input transitions.(0).name 0

let parseStates fileContent =
  let states = ref "" in
  let rec loop index =
    if index >= (String.length fileContent) then begin
      print_string "State Parse Error!\n";
      exit 0
    end
    else if fileContent.[index] = ':' then begin
      ()
    end
    else begin
      states := !states ^ (String.make 1 fileContent.[index]);
      loop (index + 1)
    end;
  in
  loop 0;
  !states

let parseAlphabet fileContent =
  let skipStates = ref 0 in
  let alphabet = ref "" in
  let rec loop index =
    if index >= (String.length fileContent) then begin
      print_string "Alphabet Parse Error!\n";
      exit 0
    end
    else if fileContent.[index] = ':' then begin
      skipStates := 1;
      loop (index + 1)
    end
    else if !skipStates = 0 then begin
      loop (index + 1)
    end
    else if fileContent.[index] = '&' then begin
      ()
    end
    else begin
      alphabet := !alphabet ^ (String.make 1 fileContent.[index]);
      loop (index + 1)
    end;
  in
  loop 0;
  !alphabet


let parseString fileContent =
  let newStr = ref "" in
  let skip = ref 0 in
  let rec loop index =
    if index >= (String.length fileContent) then begin
      print_string "String Parse Error!\n";
      exit 0
    end
    else if fileContent.[index] = '&' then begin
      skip := 1;
      loop (index + 1)
    end
    else if !skip = 0 then begin
      loop (index + 1)
    end
    else if fileContent.[index] = '@' then begin
      ()
    end
    else begin
      newStr := !newStr ^ (String.make 1 fileContent.[index]);
      loop (index + 1)
    end;
  in
  loop 0;
  !newStr

let parseHelper fileContent len =
	let result = Array.make len " " in
	let index = ref 0 in
	let strr = ref "" in
	String.iter (fun x ->
		if x = 'L' || x = 'R' then begin
			strr := !strr ^ (String.make 1 x);
			result.(!index) <- !strr;
			strr := "";
			incr index
		end
		else begin
			strr := !strr ^ (String.make 1 x)
		end;
	) fileContent;
	let rec loop indexx =
		if indexx >= (Array.length result) then begin
			()
		end
		else begin
			if result.(indexx).[0] = '-' then begin
				if indexx = 0 then begin
					print_string "First transisiton cannot be <->!\n";
					exit 0
				end;
				let newStr = ref "" in
				String.iteri (fun i x ->
					if i = 0 then begin
						newStr := String.make 1 result.(indexx - 1).[0]
					end
					else begin
						newStr := !newStr ^ (String.make 1 x)
					end;
				) result.(indexx);
				result.(indexx) <- !newStr;
				loop (indexx + 1)
			end;
			loop (indexx + 1)
		end;
	in
	loop 0;
	result

let checkStates (transitions : transition array) (states : string) =
	let count = ref 0 in
	String.iter (fun x ->
		String.iter (fun y ->
			if x = y then begin incr count end;
		) states;
		if !count != 1 then begin
			print_string "Duplicated States!\n";
			exit 0
		end;
		count := 0
	) states;
	if (states.[(String.length states) - 1] != 'H') then begin
		print_string "State Last Character Must Be \'H\' For Halt!\n";
		exit 0
	end;
	Array.iter (fun (trans : transition) ->
		if not (String.exists (fun x -> x = trans.name.[0]) states) then begin
			print_string "Transition name must be in states!\n";
			exit 0
		end;
		if not (String.exists (fun x -> x = trans.to_state.[0]) states) then begin
			print_string "Transition to_state must be in states!\n";
			exit 0
		end;
	) transitions;
	()

let checkAlphabet alphabet input =
	if (alphabet.[(String.length alphabet) - 1] != input.((Array.length input) - 1)) then begin
		print_string "Alphabet last character must be input last character!\n";
		exit 0
	end;
	if (Array.exists (fun x -> x = alphabet.[0]) input) then begin
		print_string "Alphabet first character is blank character cannot be in input!\n";
		exit 0
	end;
	let count = ref 0 in

	String.iter (fun x ->
		String.iter (fun y ->
			if x = y then begin incr count end;
		) alphabet;
		if !count != 1 then begin
			print_string "Duplicated Alphabet!\n";
			exit 0
		end;
		count := 0
	) alphabet;

	count := 0;

	Array.iter (fun x ->
		if x = (alphabet.[(String.length alphabet) - 1]) then begin
			incr count
		end;	
	) input;
	if !count != 1 then begin
		print_string "Alphabet last character must be unique!\n";
		exit 0
	end;
	Array.iter (fun inp ->
		if not(String.exists (fun c -> c = inp) alphabet) then begin
			print_string "Input must be alphabet character!\n";
			exit 0
		end;
	) input;
	()

let checkTransitions (trans : string array) states alphabet =
	let rec loop index =
		if index >= Array.length trans then begin
			()
		end
		else begin
			if not(String.exists (fun x -> x = trans.(index).[0]) states) then begin
				print_string "Transition Errorr!\n";
				exit 0
			end;
			if not(String.exists (fun x -> x = trans.(index).[1]) alphabet) then begin
				print_string "Transition Errorr!\n";
				exit 0
			end;
			if trans.(index).[2] != '?' then begin
				print_string "Transition Errorr!\n";
				exit 0
			end;
			if not(String.exists (fun x -> x = trans.(index).[3]) alphabet) then begin
				print_string "Transition Error!\n";
				exit 0
			end;
			if trans.(index).[4] != '>' then begin
				print_string "Transition Error!\n";
				exit 0
			end;
			if not(String.exists (fun x -> x = trans.(index).[5]) states) then begin
				print_string "Transition Error!\n";
				exit 0
			end;
			if trans.(index).[6] != '#' then begin
				print_string "Transition Error!\n";
				exit 0
			end;
			if not(trans.(index).[7] = 'L' || trans.(index).[7] = 'R') then begin
				print_string "Transition Error!\n";
				exit 0
			end;
			loop (index + 1)
		end;
	in
	loop 0

let startMachine fileContent input =
  if fileContent.[(String.length fileContent) - 2] != '@' then begin
    print_string "File content error!\n";
    exit 0
  end;
  let states = parseStates fileContent in
  let alphabet = parseAlphabet fileContent in
	checkAlphabet alphabet input;
  let transitionCount = ref 0 in
  String.iter (fun x ->
    if x = 'L' || x = 'R' then begin incr transitionCount end; 
  ) fileContent;
  let fileContent = parseString fileContent in
	let myarray : string array = parseHelper fileContent !transitionCount in
	checkTransitions myarray states alphabet;
  let transitions : transition array = Array.init !transitionCount (fun x ->
    {
      name = String.make 1 myarray.(x).[0];
      to_state = String.make 1 myarray.(x).[5];
      action = String.make 1 myarray.(x).[String.length myarray.(x) - 1];
      write = String.make 1 myarray.(x).[3];
      read = String.make 1 myarray.(x).[1];
    }
  ) in
	checkStates transitions states;
  let result = startAlgoo transitions input in
  Array.iter (fun x -> Printf.printf "%c" x) result
