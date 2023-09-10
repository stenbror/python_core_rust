
pub trait SourceBufferMethods {
    fn new() -> Self;
    fn from_text(&mut self, text: &'static str) -> ();
    fn from_file(&mut self, path_and_file: &'static str) -> bool;
    fn peek_char(&self) -> char;
    fn peek_three_chars(&self) -> (char, char, char);
    fn next(&mut self) -> ();
    fn next_two(&mut self) -> ();
    fn next_three(&mut self) -> ();
    fn is_end_of_file(&self) -> bool;
    fn index(&self) -> u32;
    fn is_literal_start(&self) -> bool;
    fn is_literal_character(&self) -> bool;
    fn is_digit(&self) -> bool;
    fn is_octet_digit(&self) -> bool;
    fn is_hex_digit(&self) -> bool;
    fn is_binary_digit(&self) -> bool;
    fn length(&self) -> u32;
    fn slice(&self, start: u32, end: u32) -> String;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceBuffer {
    buffer: Vec<char>,
    position: u32
}

impl SourceBufferMethods for SourceBuffer {
    
    /// Initialize a new source buffer
    fn new() -> SourceBuffer {
        SourceBuffer {
            buffer: Vec::new(),
            position: 0
          }
    }

    /// Initialize source buffer from a string
    fn from_text(&mut self, text: &'static str) -> () {
        self.buffer = text.chars().collect();
    }

    /// Initialize source buffer from given file and path.
    fn from_file(&mut self, path_and_file: &'static str) -> bool {
        true
    }

    /// Peek current character
    fn peek_char(&self) -> char {
        match self.buffer.get(self.position as usize) {
            Some(x) => {
                return x.clone()
            },
            _ => '\0'       
        }
    }

    /// Peek the next three characters in buffer
    fn peek_three_chars(&self) -> (char, char, char) {
        let c1 = match self.buffer.get(self.position as usize) {
            Some(x) => x.clone(),
            _ => ' '
        };
        let c2 = match self.buffer.get((self.position + 1) as usize) {
            Some(x) => x.clone(),
            _ => ' '
        };
        let c3 = match self.buffer.get((self.position + 2) as usize) {
            Some(x) => x.clone(),
            _ => ' '
        };
        (c1, c2, c3)
    }

    /// Foreward one character in buffer
    fn next(&mut self) -> () {
        if self.position < (self.length() - 2) {
             self.position = self.position + 1;
        }
    }

    /// Foreward two characters in buffer
    fn next_two(&mut self) -> () {
        if self.position < (self.length() - 3) {
            self.position = self.position + 2;
       }
    }

    /// Foreward three characters in buffer
    fn next_three(&mut self) -> () {
        if self.position < (self.length() - 4) {
            self.position = self.position + 3;
       }
    }

    /// Check for if we are at end of file.
    fn is_end_of_file(&self) -> bool {
        self.position >= self.length()
    }

    /// Current position in source buffer.
    fn index(&self) -> u32 {
        self.position
    }

    /// Checks current character if it's valid start character.
    fn is_literal_start(&self) -> bool {
        match self.buffer.get(self.position as usize) {
            Some(x) => {
                match x {
                    '_' => true,
                    _ => {
                        x.is_alphabetic()
                    }
                }
            }, _ => false
        }
    }
    
    /// Checks current character if it's valid character.
    fn is_literal_character(&self) -> bool {
        match self.buffer.get(self.position as usize) {
            Some(x) => {
                match x {
                    '_' => true,
                    _ => {
                        x.is_alphanumeric()
                    }
                }
            }, _ => false
        }
    }

    /// Checks current character for decimal digits
    fn is_digit(&self) -> bool {
        match self.buffer.get(self.position as usize) {
            Some(x) => {
                match x {
                    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => true,
                    _ => false
                }
            },
            _ => false
        }
    }

    /// Checks current character for octal digits
    fn is_octet_digit(&self) -> bool {
        match self.buffer.get(self.position as usize) {
            Some(x) => {
                match x {
                    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' => true,
                    _ => false
                }
            },
            _ => false
        }
    }

    /// Checks current character for hexadecimal digits
    fn is_hex_digit(&self) -> bool {
        match self.buffer.get(self.position as usize) {
            Some(x) => {
                match x {
                    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => true,
                    'a' | 'b' | 'c' | 'd' | 'e' | 'f'  => true,
                    'A' | 'B' | 'C' | 'D' | 'E' | 'F'  => true,
                    _ => false
                }
            },
            _ => false
        }
    }

    /// Checks current character for binary digits
    fn is_binary_digit(&self) -> bool {
        match self.buffer.get(self.position as usize) {
            Some(x) => {
                match x {
                    '0' | '1' => true,
                    _ => false
                }
            },
            _ => false
        }
    }

    /// Total characters in source buffer.
    fn length(&self) -> u32 {
        self.buffer.len() as u32
    }

    /// Slice out part of source buffer
    fn slice(&self, start: u32, end: u32) -> String {
        return String::new();
    }
}

/// Unittests for Lexical Analyzer module
#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn create_buffer_from_text_and_check_length() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("for i in range(0, 10): pass");

        assert_eq!(buffer.length(), 27);
    }
}