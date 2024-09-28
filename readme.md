
# ft_turing

The goal of this project is to write a program able to simulate a single headed, single tape Turing machine from a machine description provided in json.

This project is to be written in functionnal. OCaml is a solution, but any functionnal language is allowed. If you use another langage, make sure you respect the functionnal paradigm. It will be a good occasion to experiment clever type designs and a smart functionnal approach to your program.

You must write a program able to simulate a single headed and single tape Turing ma- chine from a json machine description given as a parameter to your program. The json machine description is sligthly simplier than a formal description of the same machine.

Important Notes:
If you are using OCaml, you must turn-in a Makefile able to compile your code with ocamlopt and ocamlc. Also, your Makefile must detect any missing "libraries and/or tools" needed by your program and install them via OPAM. Your peer-evaluator is never required to install anything himself prior to the defense.

![App Screenshot](https://raw.githubusercontent.com/oyumusak/ft_turing/main/turing.png)
## To-Do
After the virtual turing machine is completed write the json of these programs:
1. A machine able to compute an unary addition.
2. A machine able to decide whether its input is a palindrome or not. Before halting, write the result on the tape as a ’n’ or a ’y’ at the right of the rightmost character of the tape.
3. A machine able to decide if the input is a word of the language 0n1n, for instance the words 000111 or 0000011111. Before halting, write the result on the tape as a ’n’ or a ’y’ at the right of the rightmost character of the tape.
4. A machine able to decide if the input is a word of the language 02n, for instance the words 00 or 0000, but not the words 000 or 00000. Before halting, write the result on the tape as a ’n’ or a ’y’ at the right of the rightmost character of the tape.
5. A machine able to run the first machine of this list, the one computing an unary addition. The machine alphabet, states, transitions and input ARE the input of the machine you are writing, encoded as you see fit.

Control for alphabet in the input.


## Installation (for now)
This will change because the peer evaluator needs to have ocaml installed, therefore we will take the easy way out using docker to make it work anywhere.

After you install opam.

```bash
  opam install yojson
  eval $(opam env)

```
To check if yojson is installed:

```bash
opam info yojson
```
ps: yojson is a module in ocaml for json parsing.
```bash
make new
dune exec ./main.exe path_to_json_file.json input_string
```

"A 1?.>B#R B 1?1>B#R +?1>B#R e?e>H#L @ 111+11="
"alphabet": [ "A", "B", "H", "L", "R", "e", "#", "1", "?", ".", ">", "+" , "="],

"A1?.>B#R B1?1>B#R+?1>B#Re?e>H#L@111+11="


states = A B H
"alphabet": [ "A", "B", "H", "L", "R", "e", "#", "1", "?", ".", ">", "+" , "=", ":","@","&","-"],

"ABH:e1.+&A1?.>B#RB1?1>B#R-+?1>B#R-e?e>H#L@111+11="
