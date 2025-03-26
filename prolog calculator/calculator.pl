% evaluate_expr/2
% Evaluates standard infix arithmetic expressions.
evaluate_expr(Expr, Result) :-
    catch(
        (   Result is Expr
        ),
        error(evaluation_error(zero_division), _),
        (   writeln('Error: Division by zero'),
            Result = 0
        )
    ).

% REPL
calculator :-
    writeln('Prolog Calculator — type `q.` to quit.'),
    repeat,
        write('>> '),
        read(Input),
        (   Input == q
        ->  writeln('Goodbye!'), !
        ;   (   number(Input) ; compound(Input) )
        ->  evaluate_expr(Input, Result),
            format('= ~w~n', [Result]),
            fail
        ;   writeln('Error: Invalid expression'),
            fail).

