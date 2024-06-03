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