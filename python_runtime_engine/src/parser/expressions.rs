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
}