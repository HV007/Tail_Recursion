# Source-to-Source Automatic Conversion to Tail Recursion
This program transforms recursive definitions in a SML program to their tail recursive variant automatically, without changing the semantics. Tail recursive functions are shown to be faster and consume less memory, thus improving the overall performance.

## Prerequisites
1. SML
2. mllex
3. ml-yacc
4. mlton

## Compilation
Run `make`

## Execution
Run `./transform <inp-file.sml> <out-file.sml>`

## Notes
Refer to `report.pptx` for description of the program.

## Project Structure
1. `tail.lex` contains lexing code for the grammar and `tail.yacc` contains the yacc specifiction of the grammar.
2. `ebnf.txt` contains the EBNF specification of the grammar.
3. `ast.sml` contains the AST structure and the transformation code.
4. `examples` folder contains some hand crafted programs (both recursive and tail recursive) that were helpful in understanding the transformation algorithm.
5. `tests` folder contains the input and output of the transformation program on `add`, `fib`, and `ack` functions.
6. `timing` folder contains the code to time SML programs
