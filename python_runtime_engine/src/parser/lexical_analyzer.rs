use super::source_buffer::*;

/// Token types that parser is operating on from source buffer.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Error(u32, String),
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
    String(u32, u32, String, bool, bool, bool),
    Number(u32, u32, String)
}

/// Analyze source code for strings.
pub fn is_string_from_buffer( buffer: &mut SourceBuffer, 
                              start: u32,
                              is_raw: bool,
                              is_unicode: bool,
                              is_format: bool) -> Option<Token> {
    
    let element = buffer.slice(start, buffer.index() - 1);

    match element {
        Some(text)  => {
            Some(Token::String(start, buffer.index(), text, is_raw, is_unicode, is_format))
        },
        _ => None
    }
}

/// Analyze source code for numbers.
pub fn is_number_from_buffer(buffer: &mut SourceBuffer) -> Option<Token> {
    let start = buffer.index();

    // Handle numbers

    let element = buffer.slice(start, buffer.index() - 1);

    match element {
        Some(text)  => {
            Some(Token::Number(start, buffer.index(), text))
        },
        _ => None
    }
}

/// Analyze source code for reserved keyword or name literal
pub fn is_reserved_keyword_or_name_from_buffer(buffer: &mut SourceBuffer) -> Option<Token> {
    let start = buffer.index();

    if buffer.is_literal_start() {
        while buffer.is_literal_character() {
            buffer.next();
        }

        let element = buffer.slice(start, buffer.index() - 1);

        match element {
            Some(text) => {
                match text.as_str() {
                    "False"     =>  return Some(Token::False(start, buffer.index())),
                    "None"      =>  return Some(Token::None(start, buffer.index())),
                    "True"      =>  return Some(Token::True(start, buffer.index())),
                    "and"       =>  return Some(Token::And(start, buffer.index())),
                    "as"        =>  return Some(Token::As(start, buffer.index())),
                    "assert"    =>  return Some(Token::Assert(start, buffer.index())),
                    "async"     =>  return Some(Token::Async(start, buffer.index())),
                    "await"     =>  return Some(Token::Await(start, buffer.index())),
                    "break"     =>  return Some(Token::Break(start, buffer.index())),
                    "class"     =>  return Some(Token::Class(start, buffer.index())),
                    "continue"  =>  return Some(Token::Continue(start, buffer.index())),
                    "def"       =>  return Some(Token::Def(start, buffer.index())),
                    "del"       =>  return Some(Token::Del(start, buffer.index())),
                    "elif"      =>  return Some(Token::Elif(start, buffer.index())),
                    "else"      =>  return Some(Token::Else(start, buffer.index())),
                    "except"    =>  return Some(Token::Except(start, buffer.index())),
                    "finally"   =>  return Some(Token::Finally(start, buffer.index())),
                    "for"       =>  return Some(Token::For(start, buffer.index())),
                    "from"      =>  return Some(Token::From(start, buffer.index())),
                    "global"    =>  return Some(Token::Global(start, buffer.index())),
                    "if"        =>  return Some(Token::If(start, buffer.index())),
                    "import"    =>  return Some(Token::Import(start, buffer.index())),
                    "in"        =>  return Some(Token::In(start, buffer.index())),
                    "is"        =>  return Some(Token::Is(start, buffer.index())),
                    "lambda"    =>  return Some(Token::Lambda(start, buffer.index())),
                    "nonlocal"  =>  return Some(Token::Nonlocal(start, buffer.index())),
                    "not"       =>  return Some(Token::Not(start, buffer.index())),
                    "or"        =>  return Some(Token::Or(start, buffer.index())),
                    "pass"      =>  return Some(Token::Pass(start, buffer.index())),
                    "raise"     =>  return Some(Token::Raise(start, buffer.index())),
                    "return"    =>  return Some(Token::Return(start, buffer.index())),
                    "try"       =>  return Some(Token::Try(start, buffer.index())),
                    "while"     =>  return Some(Token::While(start, buffer.index())),
                    "with"      =>  return Some(Token::With(start, buffer.index())),
                    "yield"     =>  return Some(Token::Yield(start, buffer.index())),
                    
                    "r" | "u" | "R" | "U" | "f" | "F" | "fr" | "Fr" | "fR" | "FR" | "rf" | "rF" | "Rf" | "RF" => {
                        if buffer.peek_char() == '"' || buffer.peek_char() == '\'' {
                            let mut is_raw: bool = false;
                            let mut is_unicode: bool = false;
                            let mut is_format: bool = false;
                            
                            match text.as_str() {
                                "r" | "R" => {
                                    is_raw = true;
                                },
                                "u" | "U" => {
                                    is_unicode = true;
                                },
                                "f" | "F" => {
                                    is_format = true;
                                },
                                _ => {
                                    is_raw = true; 
                                    is_format = true;
                                }
                            }
                            
                            return is_string_from_buffer(buffer, start, is_raw, is_unicode, is_format);
                        }
                        return Some(Token::Name(start, buffer.index(), text))
                    }
                    _   => return Some(Token::Name(start, buffer.index(), text))
                }
            },
            _ => return None
        } 
    }
    None
}

/// Analyze source code for operators or delimiters.
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
            Some(Token::Error(buffer.index(), "Expecting single '.' or tripple '...', found '..'".to_string()))
        },
        ('.', _ , _ ) => {
            let res = buffer.peek_three_chars();
                
            match res {
                ( '.', '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9', _ ) 
                    => return is_number_from_buffer(buffer),
                _ => ()
            } 

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

/// Tokenize a source buffer for parsing
pub fn tokenize_from_buffer(buffer: &mut SourceBuffer) -> Result<Vec<Token>, Token> {
    let tokens = Vec::<Token>::new();

    Ok(tokens)
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
            Some(x) => {
                match x {
                    Token::Error( a , b ) => {
                        assert_eq!(a, 0);
                        assert_eq!(b, "Expecting single '.' or tripple '...', found '..'".to_string())
                    },
                    _ => assert!(false)
                }
            },
            None => assert!(false)
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

    #[test]
    fn reserved_keyword_false() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("False");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::False(0, 5));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_none() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("None");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::None(0, 4));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_true() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("True");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::True(0, 4));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_and() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("and");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::And(0, 3));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_as() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("as");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::As(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_assert() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("assert");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Assert(0, 6));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_async() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("async");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Async(0, 5));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_await() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("await");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Await(0, 5));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_break() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("break");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Break(0, 5));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_class() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("class");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Class(0, 5));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_continue() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("continue");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Continue(0, 8));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_def() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("def");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Def(0, 3));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_del() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("del");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Del(0, 3));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_elif() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("elif");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Elif(0, 4));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_else() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("else");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Else(0, 4));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_except() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("except");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Except(0, 6));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_finally() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("finally");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Finally(0, 7));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_for() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("for");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::For(0, 3));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_from() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("from");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::From(0, 4));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_global() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("global");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Global(0, 6));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_if() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("if");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::If(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_import() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("import");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Import(0, 6));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_in() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("in");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::In(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_is() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("is");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Is(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_lambda() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("lambda");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Lambda(0, 6));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_nonlocal() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("nonlocal");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Nonlocal(0, 8));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_not() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("not");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Not(0, 3));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_or() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("or");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Or(0, 2));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_pass() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("pass");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Pass(0, 4));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_raise() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("raise");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Raise(0, 5));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_return() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("return");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Return(0, 6));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_try() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("try");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Try(0, 3));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_while() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("while");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::While(0, 5));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_with() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("with");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::With(0, 4));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_yield() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("yield");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Yield(0, 5));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn reserved_keyword_name() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("__init__");
        
        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);
        
        match res {
            Some(x) => {
                assert_eq!(x, Token::Name(0, 8, "__init__".to_string()));
            },
            None => assert!(false)
        }
    }

}