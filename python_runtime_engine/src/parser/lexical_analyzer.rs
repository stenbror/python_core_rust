
use std::str::Chars;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Eof(u32),
    Newline(u32, u32, char, char),
    Indent(u32),
    Dedent(u32),

    False(u32, u32),
    None(u32, u32),
    True(u32, u32),
    And(u32, u32),
    As(u32, u32),
    Assert(u32, u32),
    Async(u32, u32),
    Await(u32, u32),
    Break(u32, u32),
    Class(u32, u32),
    Continue(u32, u32),
    Def(u32, u32),
    Del(u32, u32),
    Elif(u32, u32),
    Else(u32, u32),
    Except(u32, u32),
    Finally(u32, u32),
    For(u32, u32),
    From(u32, u32),
    Global(u32, u32),
    If(u32, u32),
    Import(u32, u32),
    In(u32, u32),
    Is(u32, u32),
    Lambda(u32, u32),
    Nonlocal(u32, u32),
    Not(u32, u32),
    Or(u32, u32),
    Pass(u32, u32),
    Raise(u32, u32),
    Return(u32, u32),
    Try(u32, u32),
    While(u32, u32),
    With(u32, u32),
    Yield(u32, u32),

    DoubleDivAssign(u32, u32),
    DoubleDiv(u32, u32),
    DivAssign(u32, u32),
    Div(u32, u32),
    PowerAssign(u32, u32),
    Power(u32, u32),
    MulAssign(u32, u32),
    Mul(u32, u32),
    Less(u32, u32),
    LessEqual(u32, u32),
    Equal(u32, u32),
    GreaterEqual(u32, u32),
    Greater(u32, u32),
    NotEqual(u32, u32),
    ShiftLeftAssign(u32, u32),
    ShiftLeft(u32, u32),
    ShiftRightAssign(u32, u32),
    ShiftRight(u32, u32),
    PlusAssign(u32, u32),
    Plus(u32, u32),
    MinusAssign(u32, u32),
    Minus(u32, u32),
    Arrow(u32, u32),
    ModuloAssign(u32, u32),
    Modulo(u32, u32),
    DecoratorAssign(u32, u32),
    Decorator(u32, u32),
    BitAndAssign(u32, u32),
    BitAnd(u32, u32),
    BitOrAssign(u32, u32),
    BitOr(u32, u32),
    BitXorAssign(u32, u32),
    BitXor(u32, u32),
    BitInvert(u32, u32),
    ColonAssign(u32, u32),
    Colon(u32, u32),
    SemiColon(u32, u32),
    Comma(u32, u32),
    Dot(u32, u32),
    Elipsis(u32, u32),
    LeftParen(u32, u32),
    RightParen(u32, u32),
    LeftBracket(u32, u32),
    RightBracket(u32, u32),
    LeftCurly(u32, u32),
    RightCurly(u32, u32),
    Assign(u32, u32),

    Name(u32, u32, String),
    String(u32, u32, Box<Vec<Box<String>>>),
    Number(u32, u32, String)
}




pub fn lexer(input: &String) -> Result<Vec<Token>, String> {
    let mut result = Vec::new();

    let mut it = input.chars().peekable();
    while let Some(&c) = it.peek() {
        if c.is_alphabetic() || c == '_' {
            it.next();
            
        }
        match c {
            'T' | 'e' | 's'| 't' => {
                println!(".");
                it.next(); 
            },
            _ => return Err("Syntax Error!".to_string())
        }
    }

    Ok(result)
}

/// Analyze source code for reserved keyword or name literal
pub fn is_reserved_keyword_or_name(text: &mut Chars, index: u32) -> Option<(Token, u32)> {
    let mut buffer = String::new();
    
    match buffer.as_str() {
        "False"     => Some((Token::False(index, index + 5), 5)),
        "None"      => Some((Token::None(index, index + 4), 4)),
        "True"      => Some((Token::True(index, index + 4), 4)),
        "and"       => Some((Token::And(index, index + 3), 3)),
        "as"        => Some((Token::As(index, index + 2), 2)),
        "assert"    => Some((Token::Assert(index, index + 6), 6)),
        "async"     => Some((Token::Async(index, index + 5), 5)),
        "await"     => Some((Token::Await(index, index + 5), 5)),
        "break"     => Some((Token::Break(index, index + 5), 5)),
        "class"     => Some((Token::Class(index, index + 5), 5)),
        "continue"  => Some((Token::Continue(index, index + 8), 8)),
        "def"       => Some((Token::Def(index, index + 3), 3)),
        "del"       => Some((Token::Del(index, index + 3), 3)),
        "elif"      => Some((Token::Elif(index, index + 4), 4)),
        "else"      => Some((Token::Else(index, index + 4), 4)),
        "except"    => Some((Token::Except(index, index + 6), 6)),
        "finally"   => Some((Token::Finally(index, index + 7), 7)),
        "for"       => Some((Token::For(index, index + 3), 3)),
        "from"      => Some((Token::From(index, index + 4), 4)),
        "global"    => Some((Token::Global(index, index + 6), 6)),
        "if"        => Some((Token::If(index, index + 2), 2)),
        "import"    => Some((Token::Import(index, index + 6), 6)),
        "in"        => Some((Token::In(index, index + 2), 2)),
        "is"        => Some((Token::True(index, index + 2), 2)),
        "lambda"    => Some((Token::Lambda(index, index + 6), 6)),
        "nonlocal"  => Some((Token::Nonlocal(index, index + 8), 8)),
        "not"       => Some((Token::Not(index, index + 3), 3)),
        "or"        => Some((Token::Or(index, index + 2), 2)),
        "pass"      => Some((Token::Pass(index, index + 4), 4)),
        "raise"     => Some((Token::Raise(index, index + 5), 5)),
        "return"    => Some((Token::Return(index, index + 6), 6)),
        "try"       => Some((Token::Try(index, index + 3), 3)),
        "while"     => Some((Token::While(index, index + 5), 5)),
        "with"      => Some((Token::With(index, index + 4), 4)),
        "yield"     => Some((Token::Yield(index, index + 5), 5)),
        _ => None
    } 
}

/// Analyzes source code for operators or delimiters.
pub fn is_operator_or_delimiter(c1: char, c2: char, c3: char, index: u32) -> Option<(Token, u8)> {
    match (c1, c2, c3) {
        ('/', '/', '=') => Some((Token::DoubleDivAssign(index, index + 3), 3)),
        ('/', '/', _ )  => Some((Token::DoubleDiv(index, index + 2), 2)),
        ('/', '=', _ )  => Some((Token::DivAssign(index, index + 2), 2)),
        ('/', _ , _ )   => Some((Token::Div(index, index + 1), 1)),
        ('*', '*', '=') => Some((Token::PowerAssign(index, index + 3), 3)),
        ('*', '*', _)   => Some((Token::Power(index, index + 2), 2)),
        ('*', '=', _)   => Some((Token::MulAssign(index, index + 2), 2)),
        ('*', _ , _)    => Some((Token::Mul(index, index + 1), 1)),
        ('<', '<', '=') => Some((Token::ShiftLeftAssign(index, index + 3), 3)),
        ('<', '<', _)   => Some((Token::ShiftLeft(index, index + 2), 2)),
        ('<', '=', _)   => Some((Token::LessEqual(index, index + 2), 2)),
        ('<', _ , _)    => Some((Token::Less(index, index + 1), 1)),
        ('>', '>', '=') => Some((Token::ShiftRightAssign(index, index + 3), 3)),
        ('>', '>', _)   => Some((Token::ShiftRight(index, index + 2), 2)),
        ('>', '=', _)   => Some((Token::GreaterEqual(index, index + 2), 2)),
        ('>', _ , _)    => Some((Token::Greater(index, index + 1), 1)),
        ('+', '=', _)   => Some((Token::PlusAssign(index, index + 2), 2)),
        ('+', _ , _)    => Some((Token::Plus(index, index + 1), 1)),
        ('-', '=', _)   => Some((Token::MinusAssign(index, index + 2), 2)),
        ('-', '>', _)   => Some((Token::Arrow(index, index + 2), 2)),
        ('-', _ , _)    => Some((Token::Minus(index, index + 1), 1)),
        ('%','=' , _)   => Some((Token::ModuloAssign(index, index + 2), 2)),
        ('%', _ , _)    => Some((Token::Modulo(index, index + 1), 1)),
        ('@','=' , _)   => Some((Token::DecoratorAssign(index, index + 2), 2)),
        ('@', _ , _)    => Some((Token::Decorator(index, index + 1), 1)),
        ('&','=' , _)   => Some((Token::BitAndAssign(index, index + 2), 2)),
        ('&', _ , _)    => Some((Token::BitAnd(index, index + 1), 1)),
        ('|','=' , _)   => Some((Token::BitOrAssign(index, index + 2), 2)),
        ('|', _ , _)    => Some((Token::BitOr(index, index + 1), 1)),
        ('^','=' , _)   => Some((Token::BitXorAssign(index, index + 2), 2)),
        ('^', _ , _)    => Some((Token::BitXor(index, index + 1), 1)),
        ('!','=' , _)   => Some((Token::NotEqual(index, index + 2), 2)),
        ('=','=' , _)   => Some((Token::Equal(index, index + 2), 2)),
        ('=', _ , _)    => Some((Token::Assign(index, index + 1), 1)),
        (':','=' , _)   => Some((Token::ColonAssign(index, index + 2), 2)),
        (':', _ , _)    => Some((Token::Colon(index, index + 1), 1)),
        (';', _ , _)    => Some((Token::SemiColon(index, index + 1), 1)),
        ('.','.', '.')  => Some((Token::Elipsis(index, index + 3), 3)),
        ('.', '.', _)   => None,
        ('.', _ , _)    => Some((Token::Dot(index, index + 1), 1)),
        (',', _ , _)    => Some((Token::Comma(index, index + 1), 1)),
        ('~', _ , _)    => Some((Token::BitInvert(index, index + 1), 1)),
        ('(', _ , _)    => Some((Token::LeftParen(index, index + 1), 1)),
        (')', _ , _)    => Some((Token::RightParen(index, index + 1), 1)),
        ('[', _ , _)    => Some((Token::LeftBracket(index, index + 1), 1)),
        (']', _ , _)    => Some((Token::RightBracket(index, index + 1), 1)),
        ('{', _ , _)    => Some((Token::LeftCurly(index, index + 1), 1)),
        ('}', _ , _)    => Some((Token::RightCurly(index, index + 1), 1)),
        _ => None
    }
}

/// Unittests for Lexical Analyzer module
#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn operator_or_delimiter_double_div_assign() {
        let res = is_operator_or_delimiter('/', '/', '=', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::DoubleDivAssign(4, 7));
                assert_eq!(y, 3);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_double_div() {
        let res = is_operator_or_delimiter('/', '/', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::DoubleDiv(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_div_assign() {
        let res = is_operator_or_delimiter('/', '=', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::DivAssign(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_div() {
        let res = is_operator_or_delimiter('/', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Div(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_power_assign() {
        let res = is_operator_or_delimiter('*', '*', '=', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::PowerAssign(4, 7));
                assert_eq!(y, 3);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_power() {
        let res = is_operator_or_delimiter('*', '*', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Power(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_mul_assign() {
        let res = is_operator_or_delimiter('*', '=', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::MulAssign(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_mul() {
        let res = is_operator_or_delimiter('*', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Mul(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_shift_left_assign() {
        let res = is_operator_or_delimiter('<', '<', '=', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::ShiftLeftAssign(4, 7));
                assert_eq!(y, 3);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_shift_left() {
        let res = is_operator_or_delimiter('<', '<', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::ShiftLeft(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_less_equal() {
        let res = is_operator_or_delimiter('<', '=', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::LessEqual(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_less() {
        let res = is_operator_or_delimiter('<', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Less(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_shift_right_assign() {
        let res = is_operator_or_delimiter('>', '>', '=', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::ShiftRightAssign(4, 7));
                assert_eq!(y, 3);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_shift_right() {
        let res = is_operator_or_delimiter('>', '>', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::ShiftRight(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_greater_equal() {
        let res = is_operator_or_delimiter('>', '=', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::GreaterEqual(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_greater() {
        let res = is_operator_or_delimiter('>', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Greater(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_equal() {
        let res = is_operator_or_delimiter('=', '=', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Equal(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_not_equal() {
        let res = is_operator_or_delimiter('!', '=', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::NotEqual(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_assign() {
        let res = is_operator_or_delimiter('=', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Assign(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_colon_assign() {
        let res = is_operator_or_delimiter(':', '=', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::ColonAssign(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_colon() {
        let res = is_operator_or_delimiter(':', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Colon(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_semicolon() {
        let res = is_operator_or_delimiter(';', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::SemiColon(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_plus_assign() {
        let res = is_operator_or_delimiter('+', '=', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::PlusAssign(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_plus() {
        let res = is_operator_or_delimiter('+', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Plus(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_minus_assign() {
        let res = is_operator_or_delimiter('-', '=', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::MinusAssign(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_arrow() {
        let res = is_operator_or_delimiter('-', '>', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Arrow(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_minus() {
        let res = is_operator_or_delimiter('-', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Minus(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_modulo_assign() {
        let res = is_operator_or_delimiter('%', '=', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::ModuloAssign(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_modulo() {
        let res = is_operator_or_delimiter('%', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Modulo(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_decorator_assign() {
        let res = is_operator_or_delimiter('@', '=', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::DecoratorAssign(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_decorator() {
        let res = is_operator_or_delimiter('@', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Decorator(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_bit_and_assign() {
        let res = is_operator_or_delimiter('&', '=', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::BitAndAssign(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_bit_and() {
        let res = is_operator_or_delimiter('&', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::BitAnd(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_bit_or_assign() {
        let res = is_operator_or_delimiter('|', '=', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::BitOrAssign(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_bit_or() {
        let res = is_operator_or_delimiter('|', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::BitOr(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_bit_xor_assign() {
        let res = is_operator_or_delimiter('^', '=', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::BitXorAssign(4, 6));
                assert_eq!(y, 2);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_bit_xor() {
        let res = is_operator_or_delimiter('^', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::BitXor(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_bit_invert() {
        let res = is_operator_or_delimiter('~', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::BitInvert(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_elipsis() {
        let res = is_operator_or_delimiter('.', '.', '.', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Elipsis(4, 7));
                assert_eq!(y, 3);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_dot() {
        let res = is_operator_or_delimiter('.', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Dot(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_comma() {
        let res = is_operator_or_delimiter(',', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::Comma(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_left_paren() {
        let res = is_operator_or_delimiter('(', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::LeftParen(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_right_paren() {
        let res = is_operator_or_delimiter(')', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::RightParen(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_left_bracket() {
        let res = is_operator_or_delimiter('[', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::LeftBracket(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_right_bracket() {
        let res = is_operator_or_delimiter(']', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::RightBracket(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_left_curly() {
        let res = is_operator_or_delimiter('{', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::LeftCurly(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_right_curly() {
        let res = is_operator_or_delimiter('}', ' ', ' ', 4);
        match res {
            Some((x, y)) => {
                assert_eq!(x, Token::RightCurly(4, 5));
                assert_eq!(y, 1);
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_error_double_dot() {
        let res = is_operator_or_delimiter('.', '.', ' ', 4);
        match res {
            Some(_) => {
                assert!(false)
            },
            None => assert!(true)
        }
    }

    #[test]
    fn operator_or_delimiter_error_unknown_character() {
        let res = is_operator_or_delimiter('$', ' ', ' ', 4);
        match res {
            Some(_) => {
                assert!(false)
            },
            None => assert!(true)
        }
    }

}