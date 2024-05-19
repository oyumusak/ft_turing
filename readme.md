
# ft_turing

The goal of this project is to write a program able to simulate a single headed, single tape Turing machine from a machine description provided in json.

This project is to be written in functionnal. OCaml is a solution, but any functionnal language is allowed. If you use another langage, make sure you respect the functionnal paradigm. It will be a good occasion to experiment clever type designs and a smart functionnal approach to your program.

You must write a program able to simulate a single headed and single tape Turing ma- chine from a json machine description given as a parameter to your program. The json machine description is sligthly simplier than a formal description of the same machine.

Important Notes:
If you are using OCaml, you must turn-in a Makefile able to compile your code with ocamlopt and ocamlc. Also, your Makefile must detect any missing "libraries and/or tools" needed by your program and install them via OPAM. Your peer-evaluator is never required to install anything himself prior to the defense.


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
dune exec _build/default/main.exe Json_Files/<any_file>.json "<any_input_like:> 111-1="
```




    
## Screenshots

![App Screenshot](https://via.placeholder.com/468x300?text=App+Screenshot+Here)

