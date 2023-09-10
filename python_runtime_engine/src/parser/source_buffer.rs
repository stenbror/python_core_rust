
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
    fn slice(&self, start: u32, end: u32) -> Option<String>;
    fn splice(&mut self, text: &'static str) -> ();
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
        if self.position <= (self.length() - 1) {
             self.position = self.position + 1;
        }
    }

    /// Foreward two characters in buffer
    fn next_two(&mut self) -> () {
        if self.position <= (self.length() - 2) {
            self.position = self.position + 2;
       }
    }

    /// Foreward three characters in buffer
    fn next_three(&mut self) -> () {
        if self.position <= (self.length() - 3) {
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
    fn slice(&self, start: u32, end: u32) -> Option<String> {
        let mut res = String::new();
        let mut index = start;
        
        if start >= end || end >= self.length() {
            return None
        }

        while index <= end {
            match self.buffer.get(index as usize) {
                Some(x) => {
                    res.push(x.clone());
                    index = index + 1
                },
                _ => return None
            }
        }

        return Some(res);
    }

    /// Add more text to buffer at end. Interactive mode support
    fn splice(&mut self, text: &'static str) -> () {
        let mut rhs = text.chars().collect::<Vec<char>>();
        self.buffer.append(&mut rhs);
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

    #[test]
    fn collect_slice_from_buffer() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("for i in range(0, 10): pass");

        match buffer.slice(9,13) {
            Some(x) => {
                assert_eq!(x, "range")
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn collect_splice_from_buffer() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("for i in range(0, 10): pass");

        assert_eq!(buffer.length(), 27);

        buffer.splice("\r\npass");

        assert_eq!(buffer.length(), 33);

        match buffer.slice(29,32) {
            Some(x) => {
                assert_eq!(x, "pass")
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn peek_one_character() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("for i in range(0, 10): pass");

        assert_eq!(buffer.peek_char(), 'f');

        buffer.next();
        assert_eq!(buffer.peek_char(), 'o');

        buffer.next();
        assert_eq!(buffer.peek_char(), 'r');
    }

    #[test]
    fn peek_one_character_at_end() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("fo");

        assert_eq!(buffer.peek_char(), 'f');

        buffer.next();
        assert_eq!(buffer.peek_char(), 'o');

        buffer.next();
        assert_eq!(buffer.peek_char(), '\0');
    }

    #[test]
    fn peek_three_character() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("for i in range(0, 10): pass");

        assert_eq!(buffer.peek_three_chars(), ('f', 'o', 'r'));
    }

    #[test]
    fn peek_three_character_at_end() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("f");

        assert_eq!(buffer.peek_three_chars(), ('f', ' ', ' '));
    }

    #[test]
    fn check_for_binary_digit() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("102");

        assert!(buffer.is_binary_digit());
        buffer.next();
        
        assert!(buffer.is_binary_digit());
        buffer.next();

        assert!(!buffer.is_binary_digit());
    }

    #[test]
    fn check_for_octet_digit() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("012345678");

        for n in 1 .. 9 {
            assert!(buffer.is_octet_digit());
            buffer.next();
        }

        assert!(!buffer.is_binary_digit());
    }

    #[test]
    fn check_for_decimal_digit() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0123456789a");

        for n in 1 .. 11 {
            assert!(buffer.is_digit());
            buffer.next();
        }
        
        assert!(!buffer.is_binary_digit());
    }

    #[test]
    fn check_for_hexadecimal_digit() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("0123456789abcdefABCDEFx");

        for n in 1 .. 23 {
            assert!(buffer.is_hex_digit());
            buffer.next();
        }
        
        assert!(!buffer.is_binary_digit());
    }

    #[test]
    fn check_for_start_literal() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("_aA1");

        for n in 1 .. 4 {
            assert!(buffer.is_literal_start());
            buffer.next();
        }
        
        assert!(!buffer.is_literal_start());
    }

    #[test]
    fn check_for_character_not_first_literal() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("_aA1+");

        for n in 1 .. 5 {
            assert!(buffer.is_literal_character());
            buffer.next();
        }
        
        assert!(!buffer.is_literal_character());
    }

    #[test]
    fn collect_position_in_buffer() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("for i in range(0, 10): pass");

        assert_eq!(buffer.index(), 0);
        buffer.next();

        assert_eq!(buffer.index(), 1);
        buffer.next_two();

        assert_eq!(buffer.index(), 3);
        buffer.next_three();

        assert_eq!(buffer.index(), 6);
    }

    #[test]
    fn check_for_end_of_file() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("");

        assert!(buffer.is_end_of_file());
    }

    #[test]
    fn check_for_end_of_file_not() {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("test");

        assert!(!buffer.is_end_of_file());
    }
    
}