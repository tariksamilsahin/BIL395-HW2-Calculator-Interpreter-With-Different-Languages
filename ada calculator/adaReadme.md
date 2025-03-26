# ADA Calculator

This project implements a simple arithmetic expression parser and evaluator written in the ADA programming language. The calculator supports basic operations such as addition (`+`), subtraction (`-`), multiplication (`*`), and division (`/`), as well as parentheses.

## Features

- Integer arithmetic
- Parentheses support
- Division by zero error handling
- Fully implemented in standard Ada (no external parser tools needed)

---

## How I Implemented the Solution

This calculator was built using a recursive descent parsing approach in pure ADA.

1. **Tokenization and Parsing**:
   - There is no separate lexer: parsing is done directly from user input using character-level analysis.
   - The parser is implemented using recursive functions: `Parse_Expr`, `Parse_Term`, and `Parse_Factor`, which follow the classic grammar:
     ```
     expr   → term (('+' | '-') term)*
     term   → factor (('*' | '/') factor)*
     factor → number | '(' expr ')'
     ```

2. **AST (Abstract Syntax Tree)**:
   - Expressions are represented as a tree of `Expr` records, where each node contains its kind (e.g., Add, Sub), and left/right subexpressions.

3. **Evaluation**:
   - The `Evaluate` function recursively traverses the AST and computes the result.
   - If a division by zero is encountered, an appropriate error message is printed and zero is returned.

4. **Error Handling**:
   - Errors like division by zero and unmatched parentheses are reported with descriptive messages.

---

## How to Run the Code

### Requirements

- A working Ada compiler such as `GNAT` (you can install it via `apt`, `brew`, or `choco`)

### Build and Run

```bash
gnatmake main.adb
./main

