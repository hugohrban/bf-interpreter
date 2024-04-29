# Brainfuck interpreter
This program is an interpreter of the esoteric programming language Brainfuck implemented in Haskell. Reference: http://brainfuck.org/brainfuck.html

The basic tape size is 30000. The size of each cell is 8 bits. Overflow/underflow may occur.

## Compilation
Compile the program in the command line using the `ghc` compiler with the command:

```bash
ghc brainfuck.hs
```

## Running via command line
The compiled program can be run with a parameter specifying the name of the file containing the Brainfuck source code. An additional parameter specifying the tape size can be added. If the second parameter is not specified, the tape size defaults to 30000.

```bash
./brainfuck path/to/file.bf
```

If the file does not contain valid source code, the program will throw an exception. Characters that are not Brainfuck instructions (`+`, `-`, `<`, `>`, `.`, `,`, `[`, `]`) are ignored. A file is considered valid if the characters `[` and `]` are correctly nested.

## Interactive mode
Using `ghci`, the program can be operated interactively. Interesting functions include:

```hs
runFile :: String -> IO ()
```

This function is also called by the `main` function. It loads the source code from a file and attempts to run it. It uses stdin/stdout.

```hs
run :: String -> IO ()
```

The parameter is directly a string containing the source code.

```hs
parseSourceCode :: String -> Program
```

Returns a list of instructions.

```hs
debugPrint :: IO Tape -> Int -> IO ()
```

Parameters are the tape and the number of elements around the pointer to print to stdout.

## Examples
The `examples` folder contains several sample Brainfuck programs.
Examples:

 - `examples/numwarp.bf` User enters a number and upon pressing Enter, the number is printed diagonally. Example:
    ```bash
    $ ./brainfuck examples/numwarp.bf
    12345
           /
           \/\
          \  /
         \/\
       /\
        /\
     /\  /
      /
    \ \/
     \
    ```
 - `examples/hello.bf` Classic hello world program.
 - `examples/fib.bf` Prints the Fibonacci sequence. The program does not stop on its own; it must be terminated using CTRL+C.
 - `examples/calculator.bf` Calculator. Inputs are in the form of e.g. `4*5=`. Values can overflow, as the cell precision is only 8 bits. (More info in the source code.)
 - `examples/life.bf` Game of life. User sequentially enters commands. A command with the coordinates of a cell (+Enter) toggles that cell alive if it was previously dead, and vice versa. An empty line (just Enter) performs one iteration of the game. Command `q` ends the program. The map is a torus, so the top and bottom are connected, as well as the right and left sides.
 - `examples/squares.bf` Prints the squares of numbers less than 10,000.
 - `examples/tictactoe.bf` Tic-tac-toe game against AI. User enters the number of the cell to play and after some time, the computer plays.
 - `examples/triangle.bf` Draws Sierpiński's triangle.
 - `examples/collatz.bf` User enters a number and the program prints the number of iterations of the Collatz problem until it reaches 1. If a_n is even, then a_(n+1) = a_n / 2; if a_n is odd, then a_(n+1) = 3*a_n + 1. It's not proven that every number will reach 1, but so far this holds for all numbers examined.

_Note_: Program authors, further instructions, and descriptions of some programs are directly in the source codes.
