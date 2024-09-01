open Yojson.Basic.Util
open Types

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
  let inputHitCount = ref (Array.init (Array.length input)(fun _ -> 0)) in
	let rec runAlgo jsonContent (input : char array) state index =
		let transitions = jsonContent.transitions in
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
		if (isFinals jsonContent.finals currTransition.to_state) = true then begin
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
			  runAlgo jsonContent input currTransition.to_state (if currTransition.action = "LEFT" then index - 1 else index + 1)
      end;
    end
	in
	runAlgo jsonContent input jsonContent.initial 0

let findStateToState transitions to_state =
  let rec loop i list =
    if i + 1 > Array.length transitions then begin
      list
    end
    else if transitions.(i).to_state = to_state then begin
      loop (i + 1) (i :: list)
    end
    else begin
      loop (i + 1) list
    end;
  in
  loop 0 []

let isEmptyList lst =
  match lst with
  | [] -> true
  | _ -> false



let printState (state : transition) =
  Printf.printf "[name:%s,read:%s,to_state:%s,write:%s,action:%s]\n" state.name state.read state.to_state state.write state.action

(*
let checkAlgo jsonContent =
  let transitions = jsonContent.transitions in
  if Array.exists (fun x ->
    (Array.exists (fun y -> x.to_state = y) jsonContent.finals) = true 
  ) transitions = false then begin print_string "Transitions to_state must be finals\n"; exit 0 end;
  let tmpFinalsCount = ref 0 in
  let tmpCounter = ref 0 in

  Array.iter (fun x ->
    let finalIndex = findStateToState transitions x in
    tmpFinalsCount := !tmpFinalsCount + List.length finalIndex;
    Printf.printf "Final Counts= %d\n" (List.length finalIndex);
    (*let pseudo = ref "" in*)
    let rec loop indexs =
      if isEmptyList indexs then begin print_string "logic error\n"; exit 0 end;
      List.iter (fun y -> 
        printState transitions.(y);
        (*if transitions.(y).action = "LEFT" then begin !pseudo ^ transitions.(y).write end;
        if transitions.(y).action = "LEFT"  then begin*) 
        if transitions.(y).name = jsonContent.initial then begin
          incr tmpCounter;
          ()
        end
        else if transitions.(y).name = transitions.(y).to_state then begin
          ()
        end
        else begin loop (findStateToState transitions transitions.(y).name) end;
      ) indexs;
    in
    loop finalIndex
  ) jsonContent.finals;
  Printf.printf "FinalsCount = %d\nCounter = %d\n" !tmpFinalsCount !tmpCounter;
  if !tmpCounter < !tmpFinalsCount then begin print_string "logic err!\n"; exit 0 end;

  (*Array.iter (fun x ->
    let finalIndexs = findStateToState transitions x in
    let rec loop indexs =
      if isEmptyList indexs then begin print_string "logic error!\n"; exit 0 end;
      List.iter (fun y ->
        if transitions.(y).name = jsonContent.initial then
          ()
        else
          loop (findStateToState transitions transitions.(y).name)
      ) indexs;
    in
    loop finalIndexs
  ) jsonContent.finals;*)
  ()
*)
let checkDepends (jsonContent : jsonHeaders) inp =
	let blank = jsonContent.blank in
	let lastChCount = ref 0 in
	if (Array.length jsonContent.alphabet) < 3 then begin print_string "alphabet too short"; exit 0 end;
  if Array.exists (fun x -> blank.[0] = x) inp then begin print_string "No Blank Char In Input!!!"; exit 0 end;

	Array.iter (fun x ->
		if x = jsonContent.alphabet.((Array.length jsonContent.alphabet) - 1).[0] then begin
			incr lastChCount
		end
		else if (Array.exists (fun y -> y.[0] = x ) jsonContent.alphabet) = false then begin
			print_string "input characters must be alphabet characters!"; exit 0
		end;
	) inp;

	if !lastChCount != 1 then begin Printf.printf  "The last char must be %s and it should be unique" jsonContent.alphabet.((Array.length jsonContent.alphabet) - 1); exit 0 end;
	if inp.((Array.length inp) - 1) != jsonContent.alphabet.((Array.length jsonContent.alphabet) - 1).[0] then begin
		print_string "Alphabet last character must be input last character!";
		exit 0
	end;

  let transitions = jsonContent.transitions in
  Array.iter (fun (x : transition) ->
    if not (x.action = "RIGHT" || x.action = "LEFT") then begin
      Printf.printf "Action must be LEFT or RIGHT!\nYour Action is %s\n" x.action; exit 0
    end;
    if Array.exists (fun y -> y = x.name) jsonContent.states = false then begin print_string "Transition name must be member of states\n"; exit 0 end;
  ) transitions;
  if Array.exists (fun x ->
    (Array.exists (fun y -> x.to_state = y) jsonContent.finals) = true 
  ) transitions = false then begin print_string "Transitions to_state must be finals\n"; exit 0 end;
  (*
  if Array.exists (fun (x : transition) ->
    (Array.exists (fun y -> x.name = y) jsonContent.states) = true 
  ) transitions = false then begin print_string "Statelerin hepsi states te kayitli olmali!\n"; exit 0 end;*)
  (*let states = jsonContent.states in
  if Array.exists (fun x ->
    (Array.exists (fun (y : transition) -> y.name = x) jsonContent.transitions) = true
  ) states = false then begin print_string "Statelerin hepsi states te kayitli olmali!\n"; exit 0 end;*)
  (*
  let transitions = jsonContent.transitions in
  *)
  ()

let printHeader jsonContent =
  let nameLen = String.length jsonContent.name in
  let rec printLoop c b e = Printf.printf "%c" c; if b < e then printLoop c (b + 1) e else () in 
  printLoop '*' 0 (nameLen * 5);
  print_string "\n";
  printLoop '*' 0 (0);
  printLoop ' ' 0 (nameLen * 2 - 1);
  Printf.printf "%s" jsonContent.name;
  printLoop ' ' 0 (nameLen * 2 - 2);
  printLoop '*' 0 (0);
  print_string "\n";
  printLoop '*' 0 (nameLen * 5);
  print_string "\n"

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

let startMachine fileContent input =
  let jsonContent = Yojson.Basic.from_string fileContent in
  let jsonContent : jsonHeaders = makeRecord jsonContent in
  printHeader jsonContent;
  checkDepends jsonContent input;
  (*checkAlgo jsonContent;*)
  let result = startAlgo jsonContent input in
  Array.iter (fun x -> Printf.printf "%c" x) result;
