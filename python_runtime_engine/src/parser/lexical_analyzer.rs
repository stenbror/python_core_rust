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
    Ellipsis(u32, u32),
    LeftParen(u32, u32),
    RightParen(u32, u32),
    LeftBracket(u32, u32),
    RightBracket(u32, u32),
    LeftCurly(u32, u32),
    RightCurly(u32, u32),
    Assign(u32, u32),

    Name(u32, u32, String),
    String(u32, u32, String, bool, bool, bool),
    Number(u32, u32, String),

    TypeComment(u32, u32, String)
}

/// Analyze source code for strings.
pub fn is_string_from_buffer( buffer: &mut SourceBuffer, 
                              start: u32,
                              is_raw: bool,
                              is_unicode: bool,
                              is_format: bool) -> Option<Token> {

    let mut is_triple = false;
    let mut is_double_quote = false;
    let mut is_empty_string = false;

    // Handle start of string and analyze for single, triple or empty string
    match buffer.peek_three_chars() {
        ('"', '"', '"') => {
            buffer.next_three();
            is_triple = true; is_double_quote = true
        },
        ('"', '"', _) => {
            buffer.next();
            buffer.next();
            is_empty_string = true; is_double_quote = true
        },
        ('"', _ , _) => {
            buffer.next();
            is_double_quote = true
        },

        ('\'', '\'', '\'') => {
            buffer.next_three();
            is_triple = true; is_double_quote = false
        },
        ('\'', '\'', _) => {
            buffer.next();
            buffer.next();
            is_empty_string = true; is_double_quote = false
        },
        ('\'', _ , _) => {
            buffer.next();
            is_double_quote = false
        },
        _ => return None
    }

    if !is_empty_string {
        // Handle string content and closing quote.
        loop {
            match buffer.peek_three_chars() {
                ('"', '"', '"') => {
                    if is_triple && is_double_quote {
                        buffer.next_three();
                        break
                    }
                    buffer.next()
                },
                ('\'', '\'', '\'') => {
                    if is_triple && !is_double_quote {
                        buffer.next_three();
                        break
                    }
                    buffer.next()
                },
                ('"', _ , _) => {
                    if !is_triple && is_double_quote {
                        buffer.next();
                        break
                    }
                    buffer.next()
                },
                ('\'', _ , _) => {
                    if !is_triple && !is_double_quote {
                        buffer.next();
                        break
                    }
                    buffer.next()
                },
                ('\r', _ , _) |
                ('\n', _ , _) => {
                    if !is_triple {
                        return Some(Token::Error(buffer.index(), "Line shift inside single quote not allowed!".to_string()))
                    }
                    buffer.next()
                },
                _ => {
                    if buffer.is_end_of_file() {
                        return Some(Token::Error(buffer.index(), "End of file inside string!".to_string()))
                    }
                    buffer.next()
                }
            }
        }
    }

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
    match buffer.peek_char() {
        '0' => {
            buffer.next();
            match buffer.peek_char() {
                'b' | 'B' => {
                    buffer.next();
                    loop {
                        if buffer.peek_char() == '_' {
                            buffer.next()
                        }
                        if !buffer.is_binary_digit() {
                            return Some(Token::Error(buffer.index(), "Expecting '0' or '1' only in binary number!".to_string()))
                        }
                        loop {
                            buffer.next();
                            if !buffer.is_binary_digit() {
                                break
                            }
                        }
                        if buffer.peek_char() != '_' {
                            break
                        }
                    }
                    if buffer.is_digit() {
                        return Some(Token::Error(buffer.index(), "Expecting only '0' or '1' in binary number!".to_string()))
                    }
                },
                'o' | 'O' => {
                    buffer.next();
                    loop {
                        if buffer.peek_char() == '_' {
                            buffer.next()
                        }
                        if !buffer.is_octet_digit() {
                            return Some(Token::Error(buffer.index(), "Expecting '0' .. '7' only in octet number!".to_string()))
                        }
                        loop {
                            buffer.next();
                            if !buffer.is_octet_digit() {
                                break
                            }
                        }
                        if buffer.peek_char() != '_' {
                            break
                        }
                    }
                    if buffer.is_digit() {
                        return Some(Token::Error(buffer.index(), "Expecting only '0' .. '7' in octet number!".to_string()))
                    }
                },
                'x' | 'X' => {
                    buffer.next();
                    loop {
                        if buffer.peek_char() == '_' {
                            buffer.next()
                        }
                        if !buffer.is_hex_digit() {
                            return Some(Token::Error(buffer.index(), "Expecting '0' .. '9', 'a' .. 'f' or 'A' .. 'F' only in hexadecimal number!".to_string()))
                        }
                        loop {
                            buffer.next();
                            if !buffer.is_hex_digit() {
                                break
                            }
                        }
                        if buffer.peek_char() != '_' {
                            break
                        }
                    }
                },
                _ => {
                    let mut nonzero = false;

                    if buffer.peek_char() != '.' {
                        loop {
                            loop {
                                if buffer.is_digit() && buffer.peek_char() != '0' {
                                    nonzero = true
                                }
                                buffer.next();
                                if !buffer.is_digit() {
                                    break
                                }
                            }
                            if buffer.peek_char() != '_' {
                                break
                            }
                            buffer.next();
                            if !buffer.is_digit() {
                                return Some(Token::Error(buffer.index(), "Expecting digit after '_' in Number!".to_string()))
                            }
                        }
                    }

                    if buffer.peek_char() == '.' {
                        buffer.next();
                        loop {
                            while buffer.is_digit() {
                                buffer.next()
                            }
                            if buffer.peek_char() != '_' {
                                break
                            }
                            buffer.next();
                            if !buffer.is_digit() {
                                return Some(Token::Error(buffer.index(), "Expecting digit after '_' in Number!".to_string()))
                            }
                        }
                    }

                    match buffer.peek_char() { // Handling exponent
                        'e' | 'E' => {
                            buffer.next();
                            match buffer.peek_char() {
                                '+' | '-' => {
                                    buffer.next();
                                    if !buffer.is_digit() {
                                        return Some(Token::Error(buffer.index(), "Expecting digit after '+' or '-' in exponent!".to_string()))
                                    }
                                },
                                _ => ()
                            }
                            if !buffer.is_digit() {
                                return Some(Token::Error(buffer.index(), "Expecting digits in exponent!".to_string()))
                            }
                            loop {
                                while buffer.is_digit() {
                                    buffer.next()
                                }
                                if buffer.peek_char() != '_' {
                                    break
                                }
                                buffer.next();
                                if !buffer.is_digit() {
                                    return Some(Token::Error(buffer.index(), "Missing digit after '_' in number!".to_string()))
                                }
                            }

                        },
                        _ => ()
                    }

                    match buffer.peek_char() { // Imaginary number
                        'j' | 'J' => {
                            buffer.next()
                        },
                        _ => ()
                    }

                    if nonzero {
                        return Some(Token::Error(buffer.index(), "Nonzero digit in number starting with zero!".to_string()))
                    }
                }
            }
        },
        '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' => {

            if buffer.peek_char() != '.' {
                loop {
                    loop {
                        buffer.next();
                        if !buffer.is_digit() {
                            break
                        }
                    }
                    if buffer.peek_char() != '_' {
                        break
                    }
                    buffer.next();
                    if !buffer.is_digit() {
                        return Some(Token::Error(buffer.index(), "Expecting digit after '_' in Number!".to_string()))
                    }
                }
            }

            if buffer.peek_char() == '.' {
                buffer.next();
                loop {
                    while buffer.is_digit() {
                        buffer.next()
                    }
                    if buffer.peek_char() != '_' {
                        break
                    }
                    buffer.next();
                    if !buffer.is_digit() {
                        return Some(Token::Error(buffer.index(), "Expecting digit after '_' in Number!".to_string()))
                    }
                }
            }

            match buffer.peek_char() { // Handling exponent
                'e' | 'E' => {
                    buffer.next();
                    match buffer.peek_char() {
                        '+' | '-' => {
                            buffer.next();
                            if !buffer.is_digit() {
                                return Some(Token::Error(buffer.index(), "Expecting digit after '+' or '-' in exponent!".to_string()))
                            }
                        },
                        _ => ()
                    }
                    if !buffer.is_digit() {
                        return Some(Token::Error(buffer.index(), "Expecting digits in exponent!".to_string()))
                    }
                    loop {
                        while buffer.is_digit() {
                            buffer.next()
                        }
                        if buffer.peek_char() != '_' {
                            break
                        }
                        buffer.next();
                        if !buffer.is_digit() {
                            return Some(Token::Error(buffer.index(), "Missing digit after '_' in number!".to_string()))
                        }
                    }

                },
                _ => ()
            }

            match buffer.peek_char() { // Imaginary number
                'j' | 'J' => {
                    buffer.next()
                },
                _ => ()
            }
        },
        _ => return None
    }
    
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
            Some(Token::Ellipsis(start, buffer.index()))
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

/// Get the next token symbol from stream
pub fn advance(buffer: &mut SourceBuffer, stack: &mut Vec<char>, is_at_beginning_of_line: bool, is_blank_line: bool) -> Result<Token, Token> {
    loop {
        let mut start = buffer.index();

        match buffer.peek_char() {
            ' ' | '\t' => {
                buffer.next();
                continue
            },
            '#' => {
                loop {
                    if buffer.is_end_of_file() {
                        return Err(Token::Error(buffer.index(), "Unexpected end  of file inside comment!".to_string()))
                    }
                    match buffer.peek_three_chars() {

                        ( '\r', '\n', _ ) => {
                            buffer.next();
                            buffer.next();
                            break;
                        },
                        ( '\r', _ , _ ) | ( '\n', _ , _ ) => {
                            buffer.next();
                            break;
                        },
                        _ => {
                            if buffer.is_end_of_file() {
                                return Err(Token::Error(buffer.index(), "Unexpected end  of file inside comment!".to_string()))
                            }
                            buffer.next()
                        }
                    }
                }

                let element = buffer.slice(start, buffer.index() - 1);

                match element {
                    Some(text)  => {
                        if text.starts_with("# type:") {
                            return Ok(Token::TypeComment(start, buffer.index(), text))
                        }
                        continue
                    },
                    _ => return Err(Token::Error(buffer.index(), "Unexpected end  of file inside comment!".to_string()))
                }
            },
            '\r' | '\n' => {
                match buffer.peek_three_chars() {
                    ( '\r', '\n', _ ) => {
                        buffer.next();
                        buffer.next();
                        match is_blank_line {
                            true => continue,
                            _ => {
                                return Ok(Token::Newline(start, buffer.index(), '\r', '\n'))
                            }
                        }
                    },
                    ( '\r', _ , _ ) | ( '\n', _ , _ ) => {
                        let ch =  buffer.peek_char();
                        buffer.next();
                        match is_blank_line {
                            true => continue,
                            _ => {
                                return Ok(Token::Newline(start, buffer.index(), ch, ' '))
                            }
                        }
                    }
                    _ => ()
                }
            },
            '\\' => {
              match buffer.peek_three_chars() {
                  ( '\\', '\r', '\n') => {
                      buffer.next_three();
                      continue
                  },
                  ( '\\', '\r', _ ) | ( '\\', '\n', _ ) => {
                      buffer.next();
                      buffer.next();
                      continue
                  },
                  _ => return Err(Token::Error(buffer.index(), "Newline should follow line continuation character '\\'".to_string()))
              }
            },
            _ => {
                if buffer.is_end_of_file() {
                    return Ok(Token::Eof(buffer.index()))
                }
            }
        }

        // Handle reserved keywords or literal name or prefixed strings.
        let reserved_keywords_or_literal = is_reserved_keyword_or_name_from_buffer(buffer);
        match reserved_keywords_or_literal {
            Some(Token::Error( a , b )) => {
                return Err(Token::Error(a, b))
            },
            Some(symbol) => {
                return Ok(symbol)
            },
            _ => ()
        }

        match buffer.peek_three_chars() {
            ('.', '.', '.') => (),
            _ => {

                // Handle numbers except for those starting with a dot.
                let number_literal = is_number_from_buffer(buffer);
                match number_literal {
                    Some(Token::Error( a , b )) => {
                        return Err(Token::Error(a, b))
                    },
                    Some(symbol) => {
                        return Ok(symbol)
                    },
                    _ => ()
                }
            }
        }

        // Handle strings except those starting with prefix.
        let string_literal = is_string_from_buffer(buffer, buffer.index(), false, false, false);
        match string_literal {
            Some(Token::Error( a , b )) => {
                return Err(Token::Error(a, b))
            },
            Some(symbol) => {
                return Ok(symbol)
            },
            _ => ()
        }

        // Handle operator or delimiter or special cases like '(', '['. '{', ')', ']', '}' or '.' digit(s)
        let operator_or_delimiter = is_operator_or_delimiter_from_buffer(buffer);
        match  operator_or_delimiter {
            Some(Token::Error( a , b )) => {
                return Err(Token::Error(a, b))
            },
            Some(symbol) => {
                match symbol {
                    Token::LeftParen( _ , _ ) => {
                        stack.push('(')
                    },
                    Token::LeftBracket( _ , _ ) => {
                        stack.push('[')
                    },
                    Token::LeftCurly( _ , _ ) => {
                        stack.push('{')
                    },
                    Token::RightParen( _ , _ ) => {
                        if stack.is_empty() || stack.pop() != Some('(') {
                            return Err(Token::Error(start, "')' does not match '('".to_string()))
                        }
                    },
                    Token::RightBracket( _ , _ ) => {
                        if stack.is_empty() || stack.pop() != Some('[') {
                            return Err(Token::Error(start, "']' does not match '['".to_string()))
                        }
                    },
                    Token::RightCurly( _ , _ ) => {
                        if stack.is_empty() || stack.pop() != Some('{') {
                            return Err(Token::Error(start, "'}' does not match '{'".to_string()))
                        }
                    },
                    _ => ()
                }
                return Ok(symbol)
            }
            _ => {
                return Err(Token::Error(buffer.index(), "Unexpected character found in sourcecode!".to_string()))
            }
        }
    }
}

/// Tokenize a source buffer for parsing
pub fn tokenize_from_buffer(buffer: &mut SourceBuffer, tab_size: u8, is_interactive: bool) -> Result<Vec<Token>, Token> {
    let mut tokens : Vec<Token> = Vec::<Token>::new();
    let mut stack : Vec<char> = Vec::<char>::new();
    let mut at_beginning_of_line: bool = true;
    let mut is_blank_line = false;
    let mut indent_stack : Vec<u32> = Vec::new();
    let mut pending : i8 = 0;

    loop {

        if at_beginning_of_line {
            at_beginning_of_line = false;
            let mut col : u32 = 0;
            is_blank_line = false;

            loop {
                match buffer.peek_char() {
                    ' ' => col += 1,
                    '\t' => col += tab_size as u32,
                    _ => break
                }
                buffer.next()
            }

            match buffer.peek_char() {
                '#' => {
                    if is_interactive {
                        col = 0;
                        is_blank_line = false
                    }
                    else {
                        is_blank_line = true
                    }
                },
                '\r' | '\n' => {
                    if col == 0 && is_interactive {
                        is_blank_line = false
                    }
                    else if is_interactive {
                        col = 0;
                        is_blank_line = false
                    }
                    else {
                        is_blank_line = true
                    }
                },
                _ => ()
            }

            if !is_blank_line && stack.len() == 0 {

                let el_last = match indent_stack.last() {
                    Some(x) => x,
                    _ => &0u32
                };

                if col > el_last.clone() {
                    indent_stack.push(col);
                    pending += 1
                }
                else if col < el_last.clone() {
                    let mut el : u32 = el_last.clone();
                    loop {
                        if indent_stack.len() > 1 && col < el {
                            pending -= 1;
                            let _ = indent_stack.pop();
                        }
                        if indent_stack.len() == 0 || col == el {
                            break
                        }
                    }

                    if col != el {
                        return Err(Token::Error(buffer.index(), "Inconsistent indentation level!".to_string()))
                    }
                }
            }
        }

        // Handle pending indent or dedent(s)
        if pending != 0 {
            if pending < 0 {
                pending += 1;
                tokens.push(Token::Dedent(buffer.index()))
            }
            else {
                pending -= 1;
                tokens.push(Token::Indent(buffer.index()))
            }
            continue
        }

        let symbol = advance(buffer, &mut stack, at_beginning_of_line, is_blank_line);

        match symbol {
            Ok(x) => {
                match x {
                    Token::Eof( _ ) => {
                        tokens.push(x);
                        break;
                    },
                    Token::Newline(_ , _ , _ , _ ) => {
                        at_beginning_of_line = true;
                        tokens.push(x);
                        break;
                    }
                    _ => {
                        at_beginning_of_line = false;
                        tokens.push(x)
                    }
                }
            },
            Err(x) => {
                return Err(x)
            }
            _ => break
        }
    }

    if buffer.is_end_of_file() {
        tokens.push(Token::Eof(buffer.index()))
    }

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
                assert_eq!(x, Token::Ellipsis(0, 3));
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
    fn reserved_keyword_break_advance_whitespace() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut at_beginning_of_line = false;
        buffer.from_text("    break");

        let res = advance(&mut buffer, &mut stack, at_beginning_of_line, false);

        match res {
            Ok(x) => {
                assert_eq!(x, Token::Break(4, 9));
            },
            Err(_) => assert!(false)
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
    fn reserved_keyword_pass_advance() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("pass");

        let res = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res {
            Ok(x) => {
                assert_eq!(x, Token::Pass(0, 4));
            },
            Err(_) => assert!(false)
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

    #[test]
    fn handle_number_dot_zero() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text(".0");

        let res = is_operator_or_delimiter_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 2, ".0".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_zero_dot_zero() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0.0");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 3, "0.0".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_zero_dot_zero_j() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0.0j");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 4, "0.0j".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_zero_dot_zero_capital_j() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0.0J");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 4, "0.0J".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_zero_dot_zero_with_exponent_1() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0.0e37");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 6, "0.0e37".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_zero_dot_zero_with_exponent_2() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0.0e-37");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 7, "0.0e-37".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_zero_dot_zero_with_exponent_2_advance() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("0.0e-37");

        let res = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res {
            Ok(x) => {
                assert_eq!(x, Token::Number(0, 7, "0.0e-37".to_string()));
            },
            Err(_) => assert!(false)
        }
    }

    #[test]
    fn handle_number_zero_dot_zero_with_exponent_3() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0.0e+37");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 7, "0.0e+37".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_zero_dot_zero_with_exponent_4() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0.0e-37j");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 8, "0.0e-37j".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_zero_dot_zero_with_exponent_5() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0.6787e-37j");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 11, "0.6787e-37j".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_binary_digits_1() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0b101");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 5, "0b101".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_binary_digits_2() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0B101");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 5, "0B101".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_binary_digits_limits_higher() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0B12");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(Token::Error(col, text)) => {
                assert_eq!(col, 3);
                assert_eq!(text, "Expecting only '0' or '1' in binary number!".to_string())
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn handle_number_octet_digits_limits_higher() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0o78");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(Token::Error(col, text)) => {
                assert_eq!(col, 3);
                assert_eq!(text, "Expecting only '0' .. '7' in octet number!".to_string())
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn handle_number_octet_digits_1() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0o76");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 4, "0o76".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_octet_digits_2() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0O76");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 4, "0O76".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_hexadecimal_digits_1() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0x00");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 4, "0x00".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_hexadecimal_digits_2() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0X00");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 4, "0X00".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_hexadecimal_digits_3() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0xaf");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 4, "0xaf".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_hexadecimal_digits_4() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0Xaf");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 4, "0Xaf".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_nonzero_errors() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("007");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(Token::Error(x, y)) => {
                assert_eq!(x, 3);
                assert_eq!(y, "Nonzero digit in number starting with zero!".to_string())
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn handle_number_single_zero_digit() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 1, "0".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_single_nonzero_digit() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("1");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 1, "1".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_integer() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("1234567890");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 10, "1234567890".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_integer_imaginary_low() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("1j");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 2, "1j".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_integer_imaginary_high() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("1J");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 2, "1J".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_float_simple() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("1.0");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 3, "1.0".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_float_simple_exponent_low_1() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("1.0e4");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 5, "1.0e4".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_float_simple_exponent_low_2() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("1.0e-4");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 6, "1.0e-4".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_float_simple_exponent_low_3() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("1.0e+4");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 6, "1.0e+4".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_float_simple_exponent_low_4() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("1.0E-4");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 6, "1.0E-4".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_number_float_simple_exponent_low_5() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("1.0E-4j");

        let res = is_number_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Number(0, 7, "1.0E-4j".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_single_quote() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("''");

        let res = is_string_from_buffer(&mut buffer, 0, false, false, false);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 2, "''".to_string(), false, false, false));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_single_quote_advance() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("''");

        let res = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res {
            Ok(x) => {
                assert_eq!(x, Token::String(0, 2, "''".to_string(), false, false, false));
            },
            Err(_) => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_double_quote() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("\"\"");

        let res = is_string_from_buffer(&mut buffer, 0, false, false, false);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 2, "\"\"".to_string(), false, false, false));
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_double_quote_with_raw_low() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("r\"\"");

        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 3, "r\"\"".to_string(), true, false, false));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_double_quote_with_raw_high() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("R\"\"");

        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 3, "R\"\"".to_string(), true, false, false));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_double_quote_with_unicode_low() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("u\"\"");

        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 3, "u\"\"".to_string(), false, true, false));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_double_quote_with_unicode_high() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("U\"\"");

        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 3, "U\"\"".to_string(), false, true, false));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_double_quote_with_format_low() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("f\"\"");

        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 3, "f\"\"".to_string(), false, false, true));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_double_quote_with_format_high() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("F\"\"");

        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 3, "F\"\"".to_string(), false, false, true));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_double_quote_with_format_raw_1() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("rf\"\"");

        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 4, "rf\"\"".to_string(), true, false, true));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_double_quote_with_format_raw_2() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("RF\"\"");

        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 4, "RF\"\"".to_string(), true, false, true));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_double_quote_with_format_raw_3() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("Rf\"\"");

        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 4, "Rf\"\"".to_string(), true, false, true));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_double_quote_with_format_raw_4() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("rF\"\"");

        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 4, "rF\"\"".to_string(), true, false, true));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_double_quote_with_format_raw_5() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("fr\"\"");

        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 4, "fr\"\"".to_string(), true, false, true));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_double_quote_with_format_raw_6() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("FR\"\"");

        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 4, "FR\"\"".to_string(), true, false, true));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_double_quote_with_format_raw_7() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("fR\"\"");

        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 4, "fR\"\"".to_string(), true, false, true));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_empty_double_quote_with_format_raw_8() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("Fr\"\"");

        let res = is_reserved_keyword_or_name_from_buffer(&mut buffer);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 4, "Fr\"\"".to_string(), true, false, true));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_triple_double_with_single_quote() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("\"\"\"This is a 'test' for you! \"\"\"");

        let res = is_string_from_buffer(&mut buffer, 0, false, false, false);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 32, "\"\"\"This is a 'test' for you! \"\"\"".to_string(), false, false, false));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_triple_single_with_double_quote() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("'''This is a \"test\" for you! '''");

        let res = is_string_from_buffer(&mut buffer, 0, false, false, false);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 32, "'''This is a \"test\" for you! '''".to_string(), false, false, false));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_triple_double_empty() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("\"\"\"\"\"\"");

        let res = is_string_from_buffer(&mut buffer, 0, false, false, false);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 6, "\"\"\"\"\"\"".to_string(), false, false, false));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_triple_single_empty() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("''''''");

        let res = is_string_from_buffer(&mut buffer, 0, false, false, false);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 6, "''''''".to_string(), false, false, false));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_double_empty() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("\"\"");

        let res = is_string_from_buffer(&mut buffer, 0, false, false, false);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 2, "\"\"".to_string(), false, false, false));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_single_empty() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("''");

        let res = is_string_from_buffer(&mut buffer, 0, false, false, false);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 2, "''".to_string(), false, false, false));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_double_data() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("\"Hello, World!\"");

        let res = is_string_from_buffer(&mut buffer, 0, false, false, false);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 15, "\"Hello, World!\"".to_string(), false, false, false));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_single_data() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("'Hello, World!'");

        let res = is_string_from_buffer(&mut buffer, 0, false, false, false);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 15, "'Hello, World!'".to_string(), false, false, false));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_triple_data_with_newline() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("'''Hello, World!\r\nYes!'''");

        let res = is_string_from_buffer(&mut buffer, 0, false, false, false);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 25, "'''Hello, World!\r\nYes!'''".to_string(), false, false, false));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_triple_data_with_double_quote() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("'''''Hello, World!\r\nYes!'''");

        let res = is_string_from_buffer(&mut buffer, 0, false, false, false);

        match res {
            Some(x) => {
                assert_eq!(x, Token::String(0, 27, "'''''Hello, World!\r\nYes!'''".to_string(), false, false, false));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_single_data_with_newline_error() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("'Hello, World!\r\nYes!'");

        let res = is_string_from_buffer(&mut buffer, 0, false, false, false);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Error(14, "Line shift inside single quote not allowed!".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_string_single_data_with_end_of_file_error() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("'Hello, World!Yes!");

        let res = is_string_from_buffer(&mut buffer, 0, false, false, false);

        match res {
            Some(x) => {
                assert_eq!(x, Token::Error(18, "End of file inside string!".to_string()));
            },
            None => assert!(false)
        }
    }

    #[test]
    fn handle_parenthesis_1() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("()");

        let res1 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);
        let res2 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res1 {
            Ok(x) => {
                assert_eq!(x, Token::LeftParen(0, 1))
            },
            _ => assert!(false)
        }

        match res2 {
            Ok(y) => {
                assert_eq!(y, Token::RightParen(1, 2))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn handle_parenthesis_2() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("[]");

        let res1 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);
        let res2 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res1 {
            Ok(x) => {
                assert_eq!(x, Token::LeftBracket(0, 1))
            },
            _ => assert!(false)
        }

        match res2 {
            Ok(y) => {
                assert_eq!(y, Token::RightBracket(1, 2))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn handle_parenthesis_3() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("{}");

        let res1 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);
        let res2 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res1 {
            Ok(x) => {
                assert_eq!(x, Token::LeftCurly(0, 1))
            },
            _ => assert!(false)
        }

        match res2 {
            Ok(y) => {
                assert_eq!(y, Token::RightCurly(1, 2))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn handle_parenthesis_4() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("(]");

        let res1 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);
        let res2 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res1 {
            Ok(x) => {
                assert_eq!(x, Token::LeftParen(0, 1))
            },
            _ => assert!(false)
        }

        match res2 {
            Ok(y) => {
                assert!(false)
            },
            Err(y) => assert_eq!(y, Token::Error(1, "']' does not match '['".to_string() ))
        }
    }

    #[test]
    fn handle_parenthesis_5() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("(}");

        let res1 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);
        let res2 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res1 {
            Ok(x) => {
                assert_eq!(x, Token::LeftParen(0, 1))
            },
            _ => assert!(false)
        }

        match res2 {
            Ok(y) => {
                assert!(false)
            },
            Err(y) => assert_eq!(y, Token::Error(1, "'}' does not match '{'".to_string() ))
        }
    }

    #[test]
    fn handle_parenthesis_6() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("((([}");

        let res1 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);
        let res2 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);
        let res3 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);
        let res4 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);
        let res5 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res1 {
            Ok(x) => {
                assert_eq!(x, Token::LeftParen(0, 1))
            },
            _ => assert!(false)
        }

        match res2 {
            Ok(x) => {
                assert_eq!(x, Token::LeftParen(1, 2))
            },
            _ => assert!(false)
        }

        match res3 {
            Ok(x) => {
                assert_eq!(x, Token::LeftParen(2, 3))
            },
            _ => assert!(false)
        }

        match res4 {
            Ok(x) => {
                assert_eq!(x, Token::LeftBracket(3, 4))
            },
            _ => assert!(false)
        }

        match res5 {
            Ok(y) => {
                assert!(false)
            },
            Err(y) => assert_eq!(y, Token::Error(4, "'}' does not match '{'".to_string() ))
        }
    }

    #[test]
    fn handle_parenthesis_7() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("((()");

        let res1 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);
        let res2 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);
        let res3 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);
        let res4 = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res1 {
            Ok(x) => {
                assert_eq!(x, Token::LeftParen(0, 1))
            },
            _ => assert!(false)
        }

        match res2 {
            Ok(x) => {
                assert_eq!(x, Token::LeftParen(1, 2))
            },
            _ => assert!(false)
        }

        match res3 {
            Ok(x) => {
                assert_eq!(x, Token::LeftParen(2, 3))
            },
            _ => assert!(false)
        }

        match res4 {
            Ok(y) => {
                assert_eq!(y, Token::RightParen(3, 4))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn handle_type_comment() {
        let mut buffer = SourceBuffer::new();
        let mut stack: Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("# type: int\r\n");

        let res = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res {
            Ok(y) => {
                assert_eq!(y, Token::TypeComment(0, 13, "# type: int\r\n".to_string()))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn handle_comment() {
        let mut buffer = SourceBuffer::new();
        let mut stack: Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("# What happens here!\r\n");

        let res = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res {
            Ok(y) => {
                assert_eq!(y, Token::Eof(22))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn handle_eof() {
        let mut buffer = SourceBuffer::new();
        let mut stack: Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("");

        let res = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res {
            Ok(y) => {
                assert_eq!(y, Token::Eof(0))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn line_continuation_test() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("\\\r\n");

        let res = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res {
            Ok(x) => {
                assert_eq!(x, Token::Eof(3));
            },
            Err(_) => assert!(false)
        }
    }

    #[test]
    fn line_continuation_test_should_fail() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("\\ ");

        let res = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res {
            Ok(x) => {
                assert!(false)
            },
            Err(y) => {
                assert_eq!(y, Token::Error(0, "Newline should follow line continuation character '\\'".to_string()))
            }
        }
    }

    #[test]
    fn handle_newline_with_token_1() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("\r\n");

        let res = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res {
            Ok(x) => {
                assert_eq!(x, Token::Newline(0, 2, '\r', '\n'))
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn handle_newline_with_token_2() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("\r\n");

        let res = advance(&mut buffer, &mut stack, is_beginning_of_line, true);

        match res {
            Ok(x) => {
                assert_eq!(x, Token::Eof(2))
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn handle_newline_with_token_3() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("\r");

        let res = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res {
            Ok(x) => {
                assert_eq!(x, Token::Newline(0, 1, '\r', ' '))
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn handle_newline_with_token_4() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("\r");

        let res = advance(&mut buffer, &mut stack, is_beginning_of_line, true);

        match res {
            Ok(x) => {
                assert_eq!(x, Token::Eof(1))
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn handle_newline_with_token_5() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("\n");

        let res = advance(&mut buffer, &mut stack, is_beginning_of_line, false);

        match res {
            Ok(x) => {
                assert_eq!(x, Token::Newline(0, 1, '\n', ' '))
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn handle_newline_with_token_6() {
        let mut buffer = SourceBuffer::new();
        let mut stack : Vec<char> = Vec::new();
        let mut is_beginning_of_line = false;
        buffer.from_text("\n");

        let res = advance(&mut buffer, &mut stack, is_beginning_of_line, true);

        match res {
            Ok(x) => {
                assert_eq!(x, Token::Eof(1))
            },
            Err(_) => {
                assert!(false)
            }
        }
    }

    #[test]
    fn handle_a_few_tokens() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("for i in range(1, 100): pass\r\n");

        let res = tokenize_from_buffer(&mut buffer, 4, false);

       match res {
           Ok(x) => {
               assert_eq!(x.len(), 13);
               assert_eq!(x.get(0), Some(Token::For(0, 3)).as_ref());
               assert_eq!(x.get(1), Some(Token::Name(4, 5, "i".to_string())).as_ref());
               assert_eq!(x.get(2), Some(Token::In(6, 8)).as_ref());
               assert_eq!(x.get(3), Some(Token::Name(9, 14, "range".to_string())).as_ref());
               assert_eq!(x.get(4), Some(Token::LeftParen(14, 15)).as_ref());
               assert_eq!(x.get(5), Some(Token::Number(15, 16, "1".to_string())).as_ref());
               assert_eq!(x.get(6), Some(Token::Comma(16, 17)).as_ref());
               assert_eq!(x.get(7), Some(Token::Number(18, 21, "100".to_string())).as_ref());
               assert_eq!(x.get(8), Some(Token::RightParen(21, 22)).as_ref());
               assert_eq!(x.get(9), Some(Token::Colon(22, 23)).as_ref());
               assert_eq!(x.get(10), Some(Token::Pass(24, 28)).as_ref());
               assert_eq!(x.get(11), Some(Token::Newline(28, 30, '\r', '\n')).as_ref());
               assert_eq!(x.get(12), Some(Token::Eof(30)).as_ref());
           },
           _ => assert!(false)
       }
    }
}