
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

    // Peek current character
    fn peek_char(&self) -> char {
        ' '
    }

    /// Peek the next three characters in buffer
    fn peek_three_chars(&self) -> (char, char, char) {
        (' ', ' ', ' ')
    }

    /// Foreward one character in buffer
    fn next(&mut self) -> () {

    }

    /// Foreward two characters in buffer
    fn next_two(&mut self) -> () {

    }

    /// Foreward three characters in buffer
    fn next_three(&mut self) -> () {

    }

    /// Check for if we are at end of file.
    fn is_end_of_file(&self) -> bool {
        true
    }

    /// Current position in source buffer.
    fn index(&self) -> u32 {
        self.position
    }

    /// Checks current character if it's valid start character.
    fn is_literal_start(&self) -> bool {
        false
    }
    
    /// Checks current character if it's valid character.
    fn is_literal_character(&self) -> bool {
        false
    }

    /// Checks current character for decimal digits
    fn is_digit(&self) -> bool {
        false
    }

    /// Checks current character for octal digits
    fn is_octet_digit(&self) -> bool {
        false
    }

    /// Checks current character for hexadecimal digits
    fn is_hex_digit(&self) -> bool {
        false
    }

    /// Checks current character for binary digits
    fn is_binary_digit(&self) -> bool {
        false
    }
}