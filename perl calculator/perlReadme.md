# Perl Calculator

This is a simple arithmetic calculator implemented in Perl. It supports basic mathematical expressions including addition (`+`), subtraction (`-`), multiplication (`*`), division (`/`), and parentheses for grouping.

---

## Features

- Integer arithmetic
- Parentheses and operator precedence support
- Handles division by zero and invalid expressions
- Command-line interface
- Pure Perl implementation (no external parser libraries)

---

## How I Implemented the Solution

This calculator uses a **recursive descent parser** written entirely in Perl.

1. **Input Parsing**:
   - The input string is processed character-by-character using string indexing and position tracking.
   - Whitespace is skipped manually.

2. **Grammar**:
   The parser is structured around the following grammar:
   expr → term (('+' | '-') term)* term → factor (('' | '/') factor) factor → number | '(' expr ')' number → [0-9]+

3. **Evaluation**:
- Expressions are evaluated as they are parsed; no separate AST is built.
- Operator precedence is respected via the `expr`, `term`, and `factor` function structure.
- Division by zero is explicitly checked and reported as an error.

4. **Error Handling**:
- Handles invalid characters, missing parentheses, and division by zero with helpful messages.
- Position information is shown for better debugging of invalid input.

---

## How to Run the Code

### Requirements

- Perl (comes pre-installed on most Unix-based systems)

If not installed:
- On Debian/Ubuntu: `sudo apt install perl`
- On macOS (with Homebrew): `brew install perl`

### Running

Save the script as `calculator.pl`, then run:

```bash
perl calculator.pl



