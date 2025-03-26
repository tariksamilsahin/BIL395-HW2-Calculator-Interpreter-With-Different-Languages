# Prolog Calculator

This project is a simple calculator implemented in Prolog. It allows users to enter standard infix arithmetic expressions (such as `3 + 5`, `(2 + 3) * 4`) and evaluates them in a REPL (Read-Eval-Print Loop) interface.

---

## Features

- Supports integers and arithmetic operations: `+`, `-`, `*`, `/`
- Parentheses are supported for grouping
- Handles division by zero with clear error messages
- Accepts user input in **natural infix notation**
- Pure Prolog (no external libraries or parsers)

---

## How I Implemented the Solution

### Expression Handling

- Prolog's built-in `read/1` predicate is used to read user input from the terminal.
- Thanks to Prolog's term parser, expressions like `3 + 5` are automatically converted into internal functors like `+(3,5)`.
- These terms are passed to the built-in `is/2` operator for evaluation.

### Error Handling

- The evaluation is wrapped with `catch/3` to gracefully handle runtime errors like division by zero.
- If an invalid expression is entered, a helpful message is displayed.

### REPL Loop

- The program runs in a loop using `repeat` and allows continuous user input.
- Users can type `q.` to quit the calculator.

---

## How to Run the Code

### Requirements

- [SWI-Prolog](https://www.swi-prolog.org/)

#### On Ubuntu/Debian:
```bash
sudo apt install swi-prolog
Open terminal and launch SWI-Prolog:

swipl

Load the calculator file:

?- consult('calculator.pl').

Start the calculator:

?- calculator.
```
#### Always end each input with a period (.), as required by Prolog syntax.


