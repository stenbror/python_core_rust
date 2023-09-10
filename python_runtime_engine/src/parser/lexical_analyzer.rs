
use std::str::Chars;
use super::source_buffer::*;

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

fn is_letter_or_digit(ch: char) -> bool {
    ch.is_alphanumeric()  || ch == '_'
}

/// Analyze source code for reserved keyword or name literal
pub fn is_reserved_keyword_or_name(text: &Chars, index: u32) -> Option<(Token, u32)> {
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
pub fn is_operator_or_delimiter_from_buffer(buffer: &mut SourceBuffer) -> Option<Token> {
    let start = buffer.index();
    match buffer.peek_three_chars() {
        ('/', '/', '=') => {
            buffer.next_three();
            Some(Token::DoubleDivAssign(start, buffer.index()))
        },
        ('/', '/', _ ) => {
            buffer.next_two();
            Some(Token::DoubleDiv(start, buffer.index()))
        },
        ('/', '=', _ ) => {
            buffer.next_two();
            Some(Token::DivAssign(start, buffer.index()))
        },
        ('/', _ , _ ) => {
            buffer.next();
            Some(Token::Div(start, buffer.index()))
        },
        ('*', '*', '=') => {
            buffer.next_three();
            Some(Token::PowerAssign(start, buffer.index()))
        },
        ('*', '*', _ ) => {
            buffer.next_two();
            Some(Token::Power(start, buffer.index()))
        },
        ('*', '=', _ ) => {
            buffer.next_two();
            Some(Token::MulAssign(start, buffer.index()))
        },
        ('*', _ , _ ) => {
            buffer.next();
            Some(Token::Mul(start, buffer.index()))
        },
        ('<', '<', '=') => {
            buffer.next_three();
            Some(Token::ShiftLeftAssign(start, buffer.index()))
        },
        ('<', '<', _ ) => {
            buffer.next_two();
            Some(Token::ShiftLeft(start, buffer.index()))
        },
        ('<', '=', _ ) => {
            buffer.next_two();
            Some(Token::LessEqual(start, buffer.index()))
        },
        ('<', _ , _ ) => {
            buffer.next();
            Some(Token::Less(start, buffer.index()))
        },

        ('>', '>', '=') => {
            buffer.next_three();
            Some(Token::ShiftRightAssign(start, buffer.index()))
        },
        ('>', '>', _ ) => {
            buffer.next_two();
            Some(Token::ShiftRight(start, buffer.index()))
        },
        ('>', '=', _ ) => {
            buffer.next_two();
            Some(Token::GreaterEqual(start, buffer.index()))
        },
        ('>', _ , _ ) => {
            buffer.next();
            Some(Token::Greater(start, buffer.index()))
        },
        ('=', '=', _ ) => {
            buffer.next_two();
            Some(Token::Equal(start, buffer.index()))
        },
        ('!', '=', _ ) => {
            buffer.next_two();
            Some(Token::NotEqual(start, buffer.index()))
        },
        ('=', _ , _ ) => {
            buffer.next();
            Some(Token::Assign(start, buffer.index()))
        },
        (':', '=', _ ) => {
            buffer.next_two();
            Some(Token::ColonAssign(start, buffer.index()))
        },
        (':', _ , _ ) => {
            buffer.next();
            Some(Token::Colon(start, buffer.index()))
        },
        (';', _ , _ ) => {
            buffer.next();
            Some(Token::SemiColon(start, buffer.index()))
        },
        ('+', '=', _ ) => {
            buffer.next_two();
            Some(Token::PlusAssign(start, buffer.index()))
        },
        ('+', _ , _ ) => {
            buffer.next();
            Some(Token::Plus(start, buffer.index()))
        },
        ('-', '=', _ ) => {
            buffer.next_two();
            Some(Token::MinusAssign(start, buffer.index()))
        },
        ('-', '>', _ ) => {
            buffer.next_two();
            Some(Token::Arrow(start, buffer.index()))
        },
        ('-', _ , _ ) => {
            buffer.next();
            Some(Token::Minus(start, buffer.index()))
        },
        ('%', '=', _ ) => {
            buffer.next_two();
            Some(Token::ModuloAssign(start, buffer.index()))
        },
        ('%', _ , _ ) => {
            buffer.next();
            Some(Token::Modulo(start, buffer.index()))
        },
        ('@', '=', _ ) => {
            buffer.next_two();
            Some(Token::DecoratorAssign(start, buffer.index()))
        },
        ('@', _ , _ ) => {
            buffer.next();
            Some(Token::Decorator(start, buffer.index()))
        },
        ('&', '=', _ ) => {
            buffer.next_two();
            Some(Token::BitAndAssign(start, buffer.index()))
        },
        ('&', _ , _ ) => {
            buffer.next();
            Some(Token::BitAnd(start, buffer.index()))
        },
        ('|', '=', _ ) => {
            buffer.next_two();
            Some(Token::BitOrAssign(start, buffer.index()))
        },
        ('|', _ , _ ) => {
            buffer.next();
            Some(Token::BitOr(start, buffer.index()))
        },
        ('^', '=', _ ) => {
            buffer.next_two();
            Some(Token::BitXorAssign(start, buffer.index()))
        },
        ('^', _ , _ ) => {
            buffer.next();
            Some(Token::BitXor(start, buffer.index()))
        },
        ('.', '.', '.') => {
            buffer.next_three();
            Some(Token::Elipsis(start, buffer.index()))
        },
        ('.', '.', _ ) => {
            None
        },
        ('.', _ , _ ) => {
            buffer.next();
            Some(Token::Dot(start, buffer.index()))
        },
        ('~', _ , _ ) => {
            buffer.next();
            Some(Token::BitInvert(start, buffer.index()))
        },
        (',', _ , _ ) => {
            buffer.next();
            Some(Token::Comma(start, buffer.index()))
        },
        ('(', _ , _ ) => {
            buffer.next();
            Some(Token::LeftParen(start, buffer.index()))
        },
        ('[', _ , _ ) => {
            buffer.next();
            Some(Token::LeftBracket(start, buffer.index()))
        },
        ('{', _ , _ ) => {
            buffer.next();
            Some(Token::LeftCurly(start, buffer.index()))
        },
        (')', _ , _ ) => {
            buffer.next();
            Some(Token::RightParen(start, buffer.index()))
        },
        (']', _ , _ ) => {
            buffer.next();
            Some(Token::RightBracket(start, buffer.index()))
        },
        ('}', _ , _ ) => {
            buffer.next();
            Some(Token::RightCurly(start, buffer.index()))
        },
        _ => None
    }
}

/// Unittests for Lexical Analyzer module
#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn operator_or_delimiter_double_div_assign() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("//=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::DoubleDivAssign(0, 3));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_double_div() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("//");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::DoubleDiv(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_div_assign() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("/=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::DivAssign(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_div() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("/");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Div(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_power_assign() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("**=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::PowerAssign(0, 3));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_power() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("**");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Power(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_mul_assign() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("*=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::MulAssign(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_mul() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("*");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Mul(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_shift_left_assign() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("<<=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::ShiftLeftAssign(0, 3));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_shift_left() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("<<");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::ShiftLeft(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_less_equal() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("<=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::LessEqual(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_less() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("<");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Less(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_shift_right_assign() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text(">>=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::ShiftRightAssign(0, 3));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_shift_right() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text(">>");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::ShiftRight(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_greater_equal() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text(">=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::GreaterEqual(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_greater() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text(">");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Greater(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_equal() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("==");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Equal(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_not_equal() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("!=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::NotEqual(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_assign() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Assign(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_colon_assign() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text(":=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::ColonAssign(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_colon() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text(":");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Colon(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_semicolon() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text(";");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::SemiColon(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_plus_assign() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("+=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::PlusAssign(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_plus() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("+");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Plus(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_minus_assign() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("-=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::MinusAssign(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_arrow() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("->");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Arrow(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_minus() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("-");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Minus(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_modulo_assign() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("%=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::ModuloAssign(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_modulo() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("%");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Modulo(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_decorator_assign() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("@=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::DecoratorAssign(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_decorator() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("@");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Decorator(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_bit_and_assign() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("&=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::BitAndAssign(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_bit_and() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("&");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::BitAnd(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_bit_or_assign() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("|=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::BitOrAssign(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_bit_or() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("|");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::BitOr(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_bit_xor_assign() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("^=");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::BitXorAssign(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_bit_xor() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("^");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::BitXor(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_bit_invert() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("~");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::BitInvert(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_elipsis() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("...");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Elipsis(0, 3));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_dot() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text(".");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Dot(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_comma() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text(",");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Comma(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_left_paren() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("(");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::LeftParen(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_right_paren() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text(")");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::RightParen(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_left_bracket() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("[");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::LeftBracket(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_right_bracket() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("]");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::RightBracket(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_left_curly() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("{");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::LeftCurly(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_right_curly() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("}");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::RightCurly(0, 1));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn operator_or_delimiter_error_double_dot() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("..");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(_) => {
                assert!(false)
            },
            None => assert!(true)
        }
    }

    #[test]
    fn operator_or_delimiter_error_unknown_character() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("$");
        
        let res = is_operator_or_delimiter_from_buffer(&mut buffer);
        
        match res {
            Some(_) => {
                assert!(false)
            },
            None => assert!(true)
        }
    }

}