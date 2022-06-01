## HRAM0: A Heap Random Access Machine (HRAM) Model
HRAM0 is a type of RAM model developed to aid the investigation of memory safety in programming lanuguages with no memory management.

The formal specification and semantics of HRAM0 are contained in the file 'spec.pdf'. This project entails ongoing work with many objectives and planned developments (see the TODOs below). Included here are the first, early incarnations of a reference implementation of HRAM0 in Python along with an assembler and evaluator written in Python (v3).

## HRAM0 Assembler
The 'assembler' directory contains an assembler written in Python3. See the 'programs' directory for example assembly programs to see the syntax (a proper guide to the assembly syntax is on my TODO list). See the README file in the 'assembler' directory for instructions on how to run the assembler. The assembler takes a ".asm" file (a program written in HRAM0 assembly) and generates a ".prg" file which is JSON-formatted file containing a single object with two attributes "code" and "data", both of which are integer arrays.

## HRAM0 Evaluator
The 'evaluator' directory contains the Python 3 source of the HRAM0 evaluator. See the README file in the 'evaluator' directory for instructions on how to run the evaluator. The evaluator takes a ".prg" file and a program input and runs the program on a HRAM0 machine instance.

## Example Programs
The 'programs' directory contains the example selsort.asm, which is an implementation of selection sort.

## TODOs

1. Top priority is a reference implementation in Haskell.
2. Implementation in Coq modelled on the above.
3. Proofs in Coq of some important properties (ongoing research).
4. Write a guide on the assembler's syntax.
5. Add "imports" (i.e. inclusion of other .asm files) to the assembler.


## Usage and License
It is hoped this project finds utility as an educational tool and/or research tool, or a basis for experimentation in some way. It is licensed under the permisible MIT license (see LICENSE). Use as you see fit.
