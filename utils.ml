open Types

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