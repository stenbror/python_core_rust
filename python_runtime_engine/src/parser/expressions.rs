use crate::parser::parser::{Parser, ParserMethods};
use crate::parser::abstract_syntax_tree_nodes::*;
use crate::parser::lexical_analyzer::Token;
use crate::parser::syntax_error::{SyntaxError, SyntaxErrorMethods};

pub trait ExpressionMethods {
	fn parse_atom(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_atom_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_power(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_factor(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_term(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_arith(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_shift(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_and_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_xor_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_or_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_star_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_comparison(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
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
		let symbol = self.get_symbol();
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

	/// Rule: power := atom_expr [ '**' factor ]
	fn parse_power(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let left = self.parse_atom_expr()?;
		match self.get_symbol() {
			Token::Power( _ , _ ) => {
				let symbol = self.get_symbol();
				self.advance();
				let right = self.parse_factor()?;
				Ok(Box::new(ParseNode::PyPower(pos, self.get_position(), left, Box::new(symbol), right)))
			},
			_ => Ok(left)
		}
	}

	// Rule: factor := ( '+' | '-' | '~' ) factor | power
	fn parse_factor(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		match self.get_symbol() {
			Token::Plus( _ , _ ) => {
				let symbol_plus = self.get_symbol();
				self.advance();
				let right_plus = self.parse_factor()?;
				Ok(Box::new(ParseNode::PyUnaryPlus(pos, self.get_position(), Box::new(symbol_plus), right_plus)))
			},
			Token::Minus( _ , _ ) => {
				let symbol_minus = self.get_symbol();
				self.advance();
				let right_minus = self.parse_factor()?;
				Ok(Box::new(ParseNode::PyUnaryMinus(pos, self.get_position(), Box::new(symbol_minus), right_minus)))
			},
			Token::BitInvert( _ , _ ) => {
				let symbol_invert = self.get_symbol();
				self.advance();
				let right_invert = self.parse_factor()?;
				Ok(Box::new(ParseNode::PyUnaryBitInvert(pos, self.get_position(), Box::new(symbol_invert), right_invert)))
			},
			_ => self.parse_power()
		}
	}

	/// Rule: term := factor ( ( '*' | '/' | '//' | '%' | '@' ) factor  )*
	fn parse_term(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let mut res = self.parse_factor()?;

		loop {
			match self.get_symbol() {
				Token::Mul( _ , _ ) => {
					let symbol_mul = self.get_symbol();
					self.advance();
					let right = self.parse_factor()?;
					res = Box::new(ParseNode::PyMul(pos, self.get_position(), res, Box::new(symbol_mul), right))
				},
				Token::Div( _ , _ ) => {
					let symbol_div = self.get_symbol();
					self.advance();
					let right = self.parse_factor()?;
					res = Box::new(ParseNode::PyDiv(pos, self.get_position(), res, Box::new(symbol_div), right))
				},
				Token::DoubleDiv( _ , _ ) => {
					let symbol_double_div = self.get_symbol();
					self.advance();
					let right = self.parse_factor()?;
					res = Box::new(ParseNode::PyFloorDiv(pos, self.get_position(), res, Box::new(symbol_double_div), right))
				},
				Token::Modulo( _ , _ ) => {
					let symbol_modulo = self.get_symbol();
					self.advance();
					let right = self.parse_factor()?;
					res = Box::new(ParseNode::PyModulo(pos, self.get_position(), res, Box::new(symbol_modulo), right))
				},
				Token::Decorator( _ , _ ) => {
					let symbol_matrices = self.get_symbol();
					self.advance();
					let right = self.parse_factor()?;
					res = Box::new(ParseNode::PyMatrices(pos, self.get_position(), res, Box::new(symbol_matrices), right))
				},
				_ => break
			}
		}

		Ok(res)
	}

	/// Rule: arith :- term ( ( '+' | '-' ) term )*
	fn parse_arith(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let mut res = self.parse_term()?;

		loop {
			match self.get_symbol() {
				Token::Plus(_, _) => {
					let symbol_plus = self.get_symbol();
					self.advance();
					let right = self.parse_term()?;
					res = Box::new(ParseNode::PyPlus(pos, self.get_position(), res, Box::new(symbol_plus), right))
				},
				Token::Minus(_, _) => {
					let symbol_minus = self.get_symbol();
					self.advance();
					let right = self.parse_term()?;
					res = Box::new(ParseNode::PyMinus(pos, self.get_position(), res, Box::new(symbol_minus), right))
				},
				_ => break
			}
		}

		Ok(res)
	}

	/// Rule: arith :- arith ( ( '<<' | '>>' ) arith )*
	fn parse_shift(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let mut res = self.parse_arith()?;

		loop {
			match self.get_symbol() {
				Token::ShiftLeft(_, _) => {
					let symbol_shift_left = self.get_symbol();
					self.advance();
					let right = self.parse_arith()?;
					res = Box::new(ParseNode::PyShiftLeft(pos, self.get_position(), res, Box::new(symbol_shift_left), right))
				},
				Token::ShiftRight(_, _) => {
					let symbol_shift_right = self.get_symbol();
					self.advance();
					let right = self.parse_arith()?;
					res = Box::new(ParseNode::PyShiftRight(pos, self.get_position(), res, Box::new(symbol_shift_right), right))
				},
				_ => break
			}
		}

		Ok(res)
	}

	/// Rule: and_expr := shift ( '&' shift )*
	fn parse_and_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let mut res = self.parse_shift()?;

		loop {
			match self.get_symbol() {
				Token::BitAnd(_, _) => {
					let symbol_bit_and = self.get_symbol();
					self.advance();
					let right = self.parse_shift()?;
					res = Box::new(ParseNode::PyBitAnd(pos, self.get_position(), res, Box::new(symbol_bit_and), right))
				},
				_ => break
			}
		}

		Ok(res)
	}

	/// Rule: xor_Expr := and_expr ( '^' and_expr )*
	fn parse_xor_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let mut res = self.parse_and_expr()?;

		loop {
			match self.get_symbol() {
				Token::BitXor(_, _) => {
					let symbol_bit_xor = self.get_symbol();
					self.advance();
					let right = self.parse_and_expr()?;
					res = Box::new(ParseNode::PyBitXor(pos, self.get_position(), res, Box::new(symbol_bit_xor), right))
				},
				_ => break
			}
		}

		Ok(res)
	}

	/// Rule: or_expr := xor_expr ( '|' xor_expr )*
	fn parse_or_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let mut res = self.parse_xor_expr()?;

		loop {
			match self.get_symbol() {
				Token::BitOr(_, _) => {
					let symbol_bit_or = self.get_symbol();
					self.advance();
					let right = self.parse_xor_expr()?;
					res = Box::new(ParseNode::PyBitOr(pos, self.get_position(), res, Box::new(symbol_bit_or), right))
				},
				_ => break
			}
		}

		Ok(res)
	}

	/// Rule: star_expr := '*' or_expr
	fn parse_star_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		match self.get_symbol() {
			Token::Mul( _ , _ ) => {
				let symbol = self.get_symbol();
				self.advance();
				let right = self.parse_or_expr()?;
				Ok(Box::new(ParseNode::PyStarExpr(pos, self.get_position(), Box::new(symbol), right)))
			},
			_ => Err(SyntaxError::new("Expecting '*' in star expression!".to_string(), self.get_position()))
		}
	}

	/// Rule: comparison := or_expr ( ( '<' | '<=' | '==' | '!=' | '>=' | '>' | 'is' | 'is' 'not' | 'in' | 'not' 'in' ) or_expr )*
	fn parse_comparison(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let mut res = self.parse_or_expr()?;

		loop {
			match self.get_symbol() {
				Token::Less(_, _) => {
					let symbol_less = self.get_symbol();
					self.advance();
					let right = self.parse_or_expr()?;
					res = Box::new(ParseNode::PyLess(pos, self.get_position(), res, Box::new(symbol_less), right))
				},
				Token::LessEqual(_, _) => {
					let symbol_less_equal = self.get_symbol();
					self.advance();
					let right = self.parse_or_expr()?;
					res = Box::new(ParseNode::PyLessEqual(pos, self.get_position(), res, Box::new(symbol_less_equal), right))
				},
				Token::Equal(_, _) => {
					let symbol_equal = self.get_symbol();
					self.advance();
					let right = self.parse_or_expr()?;
					res = Box::new(ParseNode::PyLess(pos, self.get_position(), res, Box::new(symbol_equal), right))
				},
				Token::GreaterEqual(_, _) => {
					let symbol_greater_equal = self.get_symbol();
					self.advance();
					let right = self.parse_or_expr()?;
					res = Box::new(ParseNode::PyGreaterEqual(pos, self.get_position(), res, Box::new(symbol_greater_equal), right))
				},
				Token::Greater(_, _) => {
					let symbol_equal = self.get_symbol();
					self.advance();
					let right = self.parse_or_expr()?;
					res = Box::new(ParseNode::PyGreater(pos, self.get_position(), res, Box::new(symbol_equal), right))
				},
				Token::NotEqual(_, _) => {
					let symbol_not_equal = self.get_symbol();
					self.advance();
					let right = self.parse_or_expr()?;
					res = Box::new(ParseNode::PyNotEqual(pos, self.get_position(), res, Box::new(symbol_not_equal), right))
				},
				Token::In(_, _) => {
					let symbol_in = self.get_symbol();
					self.advance();
					let right = self.parse_or_expr()?;
					res = Box::new(ParseNode::PyIn(pos, self.get_position(), res, Box::new(symbol_in), right))
				},
				Token::Is(_ , _) => {
					let symbol_is = self.get_symbol();
					self.advance();
					match self.get_symbol() {
						Token::Not(_ , _) => {
							let symbol_not = self.get_symbol();
							self.advance();
							let right = self.parse_or_expr()?;
							res = Box::new(ParseNode::PyIsNot(pos, self.get_position(), res, Box::new(symbol_is), Box::new(symbol_not), right))
						},
						_ => {
							let right = self.parse_or_expr()?;
							res = Box::new(ParseNode::PyIs(pos, self.get_position(), res, Box::new(symbol_is), right))
						}
					}
				},
				Token::Not(_ , _) => {
					let symbol_not = self.get_symbol();
					self.advance();
					match self.get_symbol() {
						Token::In(_ , _) => {
							let symbol_in = self.get_symbol();
							self.advance();
							let right = self.parse_or_expr()?;
							res = Box::new(ParseNode::PyNotIn(pos, self.get_position(), res, Box::new(symbol_not), Box::new(symbol_in), right))
						},
						_ => return Err(SyntaxError::new("Expecting 'not' 'in' and not 'not' alone! ".to_string(), self.get_position()))
					}
				}
				_ => break
			}
		}

		Ok(res)
	}
}

#[cfg(test)]
mod tests {
	use crate::parser::parser::ParserMethods;
	use crate::parser::source_buffer::{SourceBuffer, SourceBufferMethods};
	use super::*;




	#[test]
	fn parse_star_expr() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("*a\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_star_expr();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyStarExpr(0, 2, Box::new(Token::Mul(0, 1)), Box::new(ParseNode::PyName(1, 2, Box::new(Token::Name(1, 2, "a".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_bit_and_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a & b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_and_expr();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyBitAnd(0, 5, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::BitAnd(2, 3)), Box::new(ParseNode::PyName(4, 5, Box::new(Token::Name(4, 5, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_double_bit_and_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a & b & c\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_and_expr();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyBitAnd(0, 9, Box::new(
					ParseNode::PyBitAnd(0, 6, Box::new(
						ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))
					), Box::new(Token::BitAnd(2, 3)), Box::new(
						ParseNode::PyName(4, 6, Box::new(Token::Name(4, 5, "b".to_string())))
					))
				), Box::new(Token::BitAnd(6, 7)), Box::new(ParseNode::PyName(8, 9, Box::new(Token::Name(8, 9, "c".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_bit_xor_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a ^ b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_xor_expr();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyBitXor(0, 5, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::BitXor(2, 3)), Box::new(ParseNode::PyName(4, 5, Box::new(Token::Name(4, 5, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_double_bit_xor_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a ^ b ^ c\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_xor_expr();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyBitXor(0, 9, Box::new(
					ParseNode::PyBitXor(0, 6, Box::new(
						ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))
					), Box::new(Token::BitXor(2, 3)), Box::new(
						ParseNode::PyName(4, 6, Box::new(Token::Name(4, 5, "b".to_string())))
					))
				), Box::new(Token::BitXor(6, 7)), Box::new(ParseNode::PyName(8, 9, Box::new(Token::Name(8, 9, "c".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_bit_or_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a | b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_or_expr();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyBitOr(0, 5, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::BitOr(2, 3)), Box::new(ParseNode::PyName(4, 5, Box::new(Token::Name(4, 5, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_double_bit_or_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a | b | c\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_or_expr();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyBitOr(0, 9, Box::new(
					ParseNode::PyBitOr(0, 6, Box::new(
						ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))
					), Box::new(Token::BitOr(2, 3)), Box::new(
						ParseNode::PyName(4, 6, Box::new(Token::Name(4, 5, "b".to_string())))
					))
				), Box::new(Token::BitOr(6, 7)), Box::new(ParseNode::PyName(8, 9, Box::new(Token::Name(8, 9, "c".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_shift_left_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a << b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_shift();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyShiftLeft(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::ShiftLeft(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_shift_right_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a >> b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_shift();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyShiftRight(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::ShiftRight(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_double_shift_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a << b >> c\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_shift();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyShiftRight(0, 11, Box::new(
					ParseNode::PyShiftLeft(0, 7, Box::new(
						ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))
					), Box::new(Token::ShiftLeft(2, 4)), Box::new(
						ParseNode::PyName(5, 7, Box::new(Token::Name(5, 6, "b".to_string())))
					))
				), Box::new(Token::ShiftRight(7, 9)), Box::new(ParseNode::PyName(10, 11, Box::new(Token::Name(10, 11, "c".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_plus_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a + b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_arith();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyPlus(0, 5, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Plus(2, 3)), Box::new(ParseNode::PyName(4, 5, Box::new(Token::Name(4, 5, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_minus_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a - b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_arith();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyMinus(0, 5, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Minus(2, 3)), Box::new(ParseNode::PyName(4, 5, Box::new(Token::Name(4, 5, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_double_arith_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a + b - c\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_arith();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyMinus(0, 9, Box::new(
					ParseNode::PyPlus(0, 6, Box::new(
						ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))
					), Box::new(Token::Plus(2, 3)), Box::new(
						ParseNode::PyName(4, 6, Box::new(Token::Name(4, 5, "b".to_string())))
					))
				), Box::new(Token::Minus(6, 7)), Box::new(ParseNode::PyName(8, 9, Box::new(Token::Name(8, 9, "c".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_mul_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a * b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_term();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyMul(0, 5, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Mul(2, 3)), Box::new(ParseNode::PyName(4, 5, Box::new(Token::Name(4, 5, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_div_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a / b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_term();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyDiv(0, 5, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Div(2, 3)), Box::new(ParseNode::PyName(4, 5, Box::new(Token::Name(4, 5, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_modulo_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a % b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_term();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyModulo(0, 5, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Modulo(2, 3)), Box::new(ParseNode::PyName(4, 5, Box::new(Token::Name(4, 5, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_matrices_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a @ b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_term();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyMatrices(0, 5, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Decorator(2, 3)), Box::new(ParseNode::PyName(4, 5, Box::new(Token::Name(4, 5, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_floor_div_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a // b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_term();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyFloorDiv(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::DoubleDiv(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}
	
	#[test]
	fn parse_double_mul_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a * b * c\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_term();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyMul(0, 9, Box::new(
					ParseNode::PyMul(0, 6, Box::new(
						ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))
					), Box::new(Token::Mul(2, 3)), Box::new(
						ParseNode::PyName(4, 6, Box::new(Token::Name(4, 5, "b".to_string())))
					))
				), Box::new(Token::Mul(6, 7)), Box::new(ParseNode::PyName(8, 9, Box::new(Token::Name(8, 9, "c".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_unary_plus() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("+a\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_factor();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyUnaryPlus(0, 2, Box::new(Token::Plus(0, 1)), Box::new(ParseNode::PyName(1, 2, Box::new(Token::Name(1, 2, "a".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_unary_minus() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("-a\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_factor();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyUnaryMinus(0, 2, Box::new(Token::Minus(0, 1)), Box::new(ParseNode::PyName(1, 2, Box::new(Token::Name(1, 2, "a".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_unary_bit_invert() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("~a\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_factor();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyUnaryBitInvert(0, 2, Box::new(Token::BitInvert(0, 1)), Box::new(ParseNode::PyName(1, 2, Box::new(Token::Name(1, 2, "a".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_power_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a ** b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_factor();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyPower(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Power(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_no_power_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_factor();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyName(0, 1, Box::new(Token::Name(0, 1, "a".to_string())))))
			},
			_ => assert!(false)
		}
	}

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