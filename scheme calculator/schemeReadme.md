# Scheme Infix Calculator

This project is a command-line calculator written in Scheme (Guile), which allows users to enter **infix arithmetic expressions** such as `3 + 5`, `(2 + 4) * 3`, and evaluates them interactively.

It supports operator precedence and parentheses by using a custom **recursive descent parser** and a tokenizer that converts user input into valid Scheme expressions before evaluation.

---

## Features

- Accepts natural infix notation (e.g., `3 + 5`, not `(+ 3 5)`)
- Supports basic arithmetic: `+`, `-`, `*`, `/`
- Parentheses for grouping expressions
- Operator precedence: `*` and `/` bind tighter than `+` and `-`
- Recursive descent parsing and expression tree construction
- REPL loop with graceful exit

---

##  How the Solution Works

### 1. **Tokenizer**

- The `tokenize` function converts an input string like `"3 + (2 * 5)"` into a list of tokens:
("3" "+" "(" "2" "*" "5" ")")

### 2. **Parser**

- A recursive descent parser interprets the tokens according to the grammar:

expr → term ((+ | -) term)* term → factor ((* | /) factor)* factor → number | '(' expr ')'

- It constructs a prefix expression (AST) that can be directly evaluated using `eval`.

### 3. **Evaluation**

- Once the expression tree is built, it is evaluated using Scheme's `eval` function in the current environment.

---

## How to Run the Code

### Requirements

- [Guile Scheme](https://www.gnu.org/software/guile/) installed 
On Ubuntu/Debian:
```bash
sudo apt install guile-3.0
```
### Run

Save the code as calculator.scm, then in your terminal:

guile calculator.scm

