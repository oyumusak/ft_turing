
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
	if fileContent.[0] != 'a' || fileContent.[1] != '=' then begin print_string "file contentin a= olarak baslaması lazim!\n"; exit 0 end;
	let rec loop (index : int) =
		if index mod 2 = 1 then begin (*tek sayilar virgul olmali*)
			if fileContent.[index] = '\n' then begin !alphabet end
			else if fileContent.[index] = ',' then begin loop (index + 1) end
			else begin print_string "file format error!\n"; exit 0 end;
		end
		else begin
			if fileContent.[index] = 'a' || fileContent.[index] = 's' || fileContent.[index] = ':' || fileContent.[index] = '?' || fileContent.[index] = '#' || fileContent.[index] = 'R' || fileContent.[index] = 'L' then begin
				Printf.printf "alfabe karakteri %c olamaz!\n" fileContent.[index];
				exit 0
			end
			else begin
				alphabet := !alphabet ^ String.make 1 fileContent.[index];
				loop (index + 1)
			end;
		end;
	in
	loop 2

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
		if index mod 2 = 1 then begin (*tek sayilar virgul olmali*)
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

let startMachine fileContent input =
	(*alphabet a = ... states s=... *)
	let alphabet : string = alphabetParse fileContent in
	let fileContent = removeLine fileContent in
	let states = stateParse fileContent in
	let fileContent = removeLine fileContent in
	Printf.printf "%s\n" fileContent;
	Printf.printf "%s\n" states;
	String.iter (fun c -> Printf.printf "%c," c) alphabet;
	print_string "\n";
	Array.iter (fun c -> Printf.printf "%c," c) input;
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
