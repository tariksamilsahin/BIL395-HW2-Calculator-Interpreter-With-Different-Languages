use std::io::{self, Write};

fn main() {
    println!("Rust Calculator — type 'q' to quit.");

    loop {
        print!(">> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            println!("Error reading input.");
            continue;
        }

        let trimmed = input.trim();
        if trimmed == "q" {
            break;
        }

        match parse_and_eval(trimmed) {
            Ok(result) => println!("= {}", result),
            Err(e) => println!("Error: {}", e),
        }
    }
}
#[derive(Debug)]
enum Expr {
    Number(i64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
}

fn parse_and_eval(input: &str) -> Result<i64, String> {
    let tokens = tokenize(input)?;
    let mut parser = Parser::new(tokens);
    let expr = parser.parse_expr()?;
    eval(&expr)
}
#[derive(Debug, Clone, PartialEq)]
enum Token {
    Number(i64),
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
}

fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        match chars[i] {
            c if c.is_whitespace() => i += 1,
            c if c.is_digit(10) => {
                let mut num = 0;
                while i < chars.len() && chars[i].is_digit(10) {
                    num = num * 10 + chars[i].to_digit(10).unwrap() as i64;
                    i += 1;
                }
                tokens.push(Token::Number(num));
            }
            '+' => { tokens.push(Token::Plus); i += 1; }
            '-' => { tokens.push(Token::Minus); i += 1; }
            '*' => { tokens.push(Token::Star); i += 1; }
            '/' => { tokens.push(Token::Slash); i += 1; }
            '(' => { tokens.push(Token::LParen); i += 1; }
            ')' => { tokens.push(Token::RParen); i += 1; }
            c => return Err(format!("Invalid character: {}", c)),
        }
    }

    Ok(tokens)
}
struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn consume(&mut self) -> Option<Token> {
        if self.pos < self.tokens.len() {
            let tok = self.tokens[self.pos].clone();
            self.pos += 1;
            Some(tok)
        } else {
            None
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        let mut node = self.parse_term()?;
        while let Some(tok) = self.peek() {
            match tok {
                Token::Plus => {
                    self.consume();
                    node = Expr::Add(Box::new(node), Box::new(self.parse_term()?));
                }
                Token::Minus => {
                    self.consume();
                    node = Expr::Sub(Box::new(node), Box::new(self.parse_term()?));
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_term(&mut self) -> Result<Expr, String> {
        let mut node = self.parse_factor()?;
        while let Some(tok) = self.peek() {
            match tok {
                Token::Star => {
                    self.consume();
                    node = Expr::Mul(Box::new(node), Box::new(self.parse_factor()?));
                }
                Token::Slash => {
                    self.consume();
                    node = Expr::Div(Box::new(node), Box::new(self.parse_factor()?));
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_factor(&mut self) -> Result<Expr, String> {
        match self.consume() {
            Some(Token::Number(n)) => Ok(Expr::Number(n)),
            Some(Token::LParen) => {
                let expr = self.parse_expr()?;
                if self.consume() != Some(Token::RParen) {
                    return Err("Expected ')'".to_string());
                }
                Ok(expr)
            }
            _ => Err("Unexpected token".to_string()),
        }
    }
}
fn eval(expr: &Expr) -> Result<i64, String> {
    match expr {
        Expr::Number(n) => Ok(*n),
        Expr::Add(a, b) => Ok(eval(a)? + eval(b)?),
        Expr::Sub(a, b) => Ok(eval(a)? - eval(b)?),
        Expr::Mul(a, b) => Ok(eval(a)? * eval(b)?),
        Expr::Div(a, b) => {
            let divisor = eval(b)?;
            if divisor == 0 {
                Err("Division by zero".to_string())
            } else {
                Ok(eval(a)? / divisor)
            }
        }
    }
}

