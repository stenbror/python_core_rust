use crate::parser::parser::{Parser, ParserMethods};
use crate::parser::abstract_syntax_tree_nodes::*;
use crate::parser::lexical_analyzer::Token;
use crate::parser::syntax_error::{SyntaxError, SyntaxErrorMethods};

pub trait ExpressionMethods {
	fn parse_atom(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_atom_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
}

impl ExpressionMethods for Parser {

	/// Rule: atom := 'False' | 'None' | 'True' | '...' | Name | Number | String+ | Set | Dictionary | List | Set
	fn parse_atom(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let symbol = self.get_symbol();
		match &symbol {
			Token::None( _, _ ) => {
				self.advance();
				Ok(Box::new(ParseNode::PyNone(pos, self.get_position(), Box::new(symbol))))
			},
			Token::False( _, _ ) => {
				self.advance();
				Ok(Box::new(ParseNode::PyFalse(pos, self.get_position(), Box::new(symbol))))
			},
			Token::True( _, _ ) => {
				self.advance();
				Ok(Box::new(ParseNode::PyTrue(pos, self.get_position(), Box::new(symbol))))
			},
			Token::Ellipsis( _, _ ) => {
				self.advance();
				Ok(Box::new(ParseNode::PyEllipsis(pos, self.get_position(), Box::new(symbol))))
			},
			Token::Name( _, _ , _ ) => {
				self.advance();
				Ok(Box::new(ParseNode::PyName(pos, self.get_position(), Box::new(symbol))))
			},
			Token::Number( _, _ , _ ) => {
				self.advance();
				Ok(Box::new(ParseNode::PyNumber(pos, self.get_position(), Box::new(symbol))))
			},
			_ => Err(SyntaxError::new("Expecting valid literal!".to_string(), pos))
		}
	}

	fn parse_atom_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		todo!()
	}
}

#[cfg(test)]
mod tests {
	use crate::parser::parser::ParserMethods;
	use crate::parser::source_buffer::{SourceBuffer, SourceBufferMethods};
	use super::*;

	#[test]
	fn parse_atom_none() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("None\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_atom();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyNone(0, 4, Box::new(Token::None(0, 4)))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_atom_false() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("False\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_atom();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyFalse(0, 5, Box::new(Token::False(0, 5)))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_atom_true() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("True\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_atom();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyTrue(0, 4, Box::new(Token::True(0, 4)))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_atom_ellipsis() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("...\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_atom();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyEllipsis(0, 3, Box::new(Token::Ellipsis(0, 3)))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_atom_name() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("__init__\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_atom();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyName(0, 8, Box::new(Token::Name(0, 8, "__init__".to_string())))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_atom_number() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text(".0J\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_atom();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyNumber(0, 3, Box::new(Token::Number(0, 3, ".0J".to_string())))))
			},
			_ => assert!(false)
		}
	}
}