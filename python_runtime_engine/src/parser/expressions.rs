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
			Token::String( _ , _ , _ , _ , _ , _  ) => {
				let mut texts : Vec<Box<Token>> = Vec::new();
				texts.push(Box::new(symbol));
				self.advance();
				loop {
					let symbol1 = self.get_symbol();
					match &symbol1 {
						Token::String( _ , _ , _ , _ , _, _ ) => {
							texts.push(Box::new(symbol1));
							self.advance();
						},
						_ => break
					}
				}
				Ok(Box::new(ParseNode::PyString(pos, self.get_position(), Box::new(texts))))
			},
			Token::Error(e_pos, e_text) => Err(SyntaxError::new(e_text.clone(), e_pos.clone())),
			_ => Err(SyntaxError::new("Expecting valid literal!".to_string(), pos))
		}
	}

	/// Rule: atom_Expr := [ 'await' ] atom [ Trailer+ ]
	fn parse_atom_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let mut symbol = self.get_symbol();
		let await_symbol = match &symbol {
			Token::Await( _ , _ ) => {
				self.advance();
				Some(symbol.clone())
			},
			_ => None
		};
		let right = self.parse_atom()?;
		let mut trailers : Vec<Box<ParseNode>> = Vec::new();

		// todo! add trailer handling here!

		match &await_symbol {
			Some(Token::Await( _, _ )) => {
				Ok(Box::new(ParseNode::PyAtomExpr(pos, self.get_position(), await_symbol, right, Box::new(trailers))))
			},
			_ => {
				match trailers.len() {
					0 => Ok(right),
					_ => Ok(Box::new(ParseNode::PyAtomExpr(pos, self.get_position(), await_symbol, right, Box::new(trailers))))
				}
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use crate::parser::parser::ParserMethods;
	use crate::parser::source_buffer::{SourceBuffer, SourceBufferMethods};
	use super::*;


	#[test]
	fn parse_atom_expr_await_name() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("await test\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_atom_expr();

		match res {
			Ok(x) => {
				let trailers : Box<Vec<Box<ParseNode>>> = Box::new(Vec::new());

				assert_eq!(x, Box::new(ParseNode::PyAtomExpr(0, 10,
                Some(Token::Await(0, 5)),
                Box::new(ParseNode::PyName(6, 10, Box::new(Token::Name(6, 10, "test".to_string())))),
				trailers )));
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_atom_expr_none() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("None\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_atom_expr();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyNone(0, 4, Box::new(Token::None(0, 4)))))
			},
			_ => assert!(false)
		}
	}

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

	#[test]
	fn parse_atom_simple_string() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("'Hello, World!'\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_atom();

		match res {
			Ok(x) => {
				let mut fasit = Vec::new();
				fasit.push(Box::new(Token::String(0, 15, "'Hello, World!'".to_string(), false, false, false)));

				assert_eq!(x, Box::new(ParseNode::PyString(0, 15, Box::new( fasit))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_atom_multiple_string() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("'Hello, World!'\\\r\n'__init__'\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_atom();

		match res {
			Ok(x) => {
				let mut fasit = Vec::new();
				fasit.push(Box::new(Token::String(0, 15, "'Hello, World!'".to_string(), false, false, false)));
				fasit.push(Box::new(Token::String(18, 28, "'__init__'".to_string(), false, false, false)));

				assert_eq!(x, Box::new(ParseNode::PyString(0, 28, Box::new( fasit))))
			},
			_ => assert!(false)
		}
	}
}