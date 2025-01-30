
# Brainfuck Interpreter in Haskell

This project is a **Brainfuck Interpreter** implemented in Haskell. The interpreter parses, validates, and executes Brainfuck programs. The Brainfuck programming language is an esoteric, minimalist language consisting of only eight commands.

## Features

- **Parsing and Validation**:
  - Converts Brainfuck source code into commands.
  - Validates syntax, ensuring proper loop matching (`[` and `]`).
- **Execution**:
  - Handles the eight Brainfuck commands (`>`, `<`, `+`, `-`, `.`, `,`, `[`, `]`).
  - Skips comments (any characters not part of Brainfuck commands).
- **Interactive Input**:
  - Prompts the user for input when encountering the `,` command.
  - Ensures that user input is a valid number.
- **Tape Implementation**:
  - Implements the infinite tape data structure to manage memory cells.
- **Error Handling**:
  - Detects unmatched brackets or empty programs and provides appropriate error messages.

## Commands Supported

| Command | Description                                                      |
|---------|------------------------------------------------------------------|
| `>`     | Move the memory pointer to the right.                           |
| `<`     | Move the memory pointer to the left.                            |
| `+`     | Increment the value at the current memory cell.                 |
| `-`     | Decrement the value at the current memory cell.                 |
| `.`     | Output the value at the current memory cell as a character.     |
| `,`     | Input a value into the current memory cell.                     |
| `[`     | Jump forward to the command after the matching `]` if the value at the current memory cell is `0`. |
| `]`     | Jump back to the command after the matching `[` if the value at the current memory cell is non-zero. |

## Installation and Usage

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/your-username/brainfuck-interpreter.git
   cd brainfuck-interpreter
   ```

2. **Compile the Program**:
   Ensure you have GHC installed, then run:
   ```bash
   ghc -o brainfuck-interpreter Main.hs
   ```

3. **Run the Program**:
   Execute the compiled binary:
   ```bash
   ./brainfuck-interpreter
   ```

4. **Provide Input**:
   The program will prompt for a file containing the Brainfuck source code:
   ```
   Filename:
   ```

   Enter the name of the file (e.g., `program.bf`).

5. **Example Brainfuck Program**:
   Save the following example in a file (e.g., `hello_world.bf`):
   ```brainfuck
   ++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.
   ```

   When run, this will output `Hello World!`.

## Error Messages

- **Empty Program**:
  If the source code contains no commands:
  ```
  Empty program!
  ```
- **Unmatched Brackets**:
  If the brackets are not balanced:
  ```
  Nothing
  ```

## Code Structure

- **Modules**:
  - `Brainfuck.hs`: Core functionality including parsing, validation, and execution.
  - `Main.hs`: Entry point for the interpreter.

- **Key Functions**:
  - `parser :: String -> Either String Program`: Parses the source code into a `Program`.
  - `checkSyntax :: String -> Maybe Bool`: Validates the syntax of the source code.
  - `runBrainfuck :: Program -> IO ()`: Executes a parsed Brainfuck program.

- **Data Structures**:
  - `Tape a`: Infinite tape implementation.
  - `Program`: Represents a list of Brainfuck commands.

## Examples

### Valid Input
Source code:
```brainfuck
[->+<]
```

Execution:
```bash
Filename:
program.bf
```

### Invalid Input
Unmatched brackets:
```brainfuck
[->+<]]
```

Output:
```
Nothing
```

## Resources
[write_yourself_a_brainfuck](https://github.com/quchen/articles/blob/master/write_yourself_a_brainfuck.md)
