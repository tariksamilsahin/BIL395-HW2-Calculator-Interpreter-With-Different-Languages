#!/usr/bin/perl
use strict;
use warnings;

print "Perl Calculator — type 'q' to quit.\n";

while (1) {
    print ">> ";
    chomp(my $line = <STDIN>);
    last if $line eq 'q';

    my $result = eval_expr($line);
    if (defined $result) {
        print "= $result\n";
    }
}

# Entry point to evaluation
sub eval_expr {
    my ($input) = @_;
    my $pos = 0;

    my $expr = parse_expr($input, \$pos);
    if (!defined $expr) {
        print "Error: Invalid expression.\n";
        return;
    }

    if ($pos < length($input)) {
        print "Error: Unexpected character at position $pos.\n";
        return;
    }

    return $expr;
}

# Skip whitespace
sub skip_ws {
    my ($input, $posref) = @_;
    $$posref++ while substr($input, $$posref, 1) =~ /\s/;
}

# expr ::= term (('+'|'-') term)*
sub parse_expr {
    my ($input, $posref) = @_;
    my $lhs = parse_term($input, $posref);
    return undef unless defined $lhs;

    skip_ws($input, $posref);
    while (substr($input, $$posref, 1) =~ /[+-]/) {
        my $op = substr($input, $$posref, 1);
        $$posref++;
        skip_ws($input, $posref);
        my $rhs = parse_term($input, $posref);
        return undef unless defined $rhs;

        $lhs = ($op eq '+') ? $lhs + $rhs : $lhs - $rhs;
        skip_ws($input, $posref);
    }

    return $lhs;
}

# term ::= factor (('*'|'/') factor)*
sub parse_term {
    my ($input, $posref) = @_;
    my $lhs = parse_factor($input, $posref);
    return undef unless defined $lhs;

    skip_ws($input, $posref);
    while (substr($input, $$posref, 1) eq '*' || substr($input, $$posref, 1) eq '/') {
        my $op = substr($input, $$posref, 1);
        $$posref++;
        skip_ws($input, $posref);
        my $rhs = parse_factor($input, $posref);
        return undef unless defined $rhs;

        if ($op eq '/' && $rhs == 0) {
            print "Error: Division by zero.\n";
            return undef;
        }

        $lhs = ($op eq '*') ? $lhs * $rhs : $lhs / $rhs;
        skip_ws($input, $posref);
    }

    return $lhs;
}

# factor ::= number | '(' expr ')'
sub parse_factor {
    my ($input, $posref) = @_;
    skip_ws($input, $posref);
    my $char = substr($input, $$posref, 1);

    if ($char eq '(') {
        $$posref++;
        my $value = parse_expr($input, $posref);
        skip_ws($input, $posref);
        if (substr($input, $$posref, 1) ne ')') {
            print "Error: Expected ')'\n";
            return undef;
        }
        $$posref++;
        return $value;
    }

    return parse_number($input, $posref);
}

# number ::= [0-9]+
sub parse_number {
    my ($input, $posref) = @_;
    skip_ws($input, $posref);

    if (substr($input, $$posref) =~ /\G(-?\d+)/gc) {
        $$posref += length($1);
        return $1 + 0;
    } else {
        print "Error: Expected number at position $$posref.\n";
        return undef;
    }
}

