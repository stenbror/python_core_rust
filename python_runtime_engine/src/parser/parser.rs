use crate::parser::lexical_analyzer::{Token, tokenize_from_buffer};
use crate::parser::source_buffer::SourceBuffer;

pub trait ParserMethods {
	fn new(buffer: &mut SourceBuffer, tab_size: u8) -> Self;
	fn get_symbol(&self) -> Token;
	fn get_position(&self) -> u32;
	fn advance(&mut self) -> ();
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parser {
	symbols: Vec::<Token>,
	index: u32
}

impl ParserMethods for Parser {
	/// Create and populate parser with tokens
	fn new(buffer: &mut SourceBuffer, tab_size: u8) -> Parser {
		let mut _symbols = Vec::<Token>::new();

		let nodes = tokenize_from_buffer(buffer, tab_size, true);

		match nodes {
			Ok(x) => {
				_symbols = x
			},
			Err(err) => {
				_symbols.push(err)
			}
		}

		Parser {
			symbols: _symbols,
			index: 0
		}
	}

	fn get_symbol(&self) -> Token {
		match self.symbols.get(self.index as usize) {
			Some(x) => {
				x.clone()
			},
			_ => Token::Eof(0)
		}
	}

	fn get_position(&self) -> u32 {
		match self.symbols.get(self.index as usize) {
			Some(x) => {
				match x.clone() {
					Token::Error(s, _) => s,
					Token::Eof(s) => s,
					Token::Newline(s, _ , _ , _) => s,
					Token::Indent(s) => s,
					Token::Dedent(s) => s,
					Token::False(s, _) => s,
					Token::None(s, _) => s,
					Token::True(s, _) => s,
					Token::And(s, _) => s,
					Token::As(s, _) => s,
					Token::Assert(s, _) => s,
					Token::Async(s, _) => s,
					Token::Await(s, _) => s,
					Token::Break(s, _) => s,
					Token::Class(s, _) => s,
					Token::Continue(s, _) => s,
					Token::Def(s, _) => s,
					Token::Del(s, _) => s,
					Token::Elif(s, _) => s,
					Token::Else(s, _) => s,
					Token::Except(s, _) => s,
					Token::Finally(s, _) => s,
					Token::For(s, _) => s,
					Token::From(s, _) => s,
					Token::Global(s, _) => s,
					Token::If(s, _) => s,
					Token::Import(s, _) => s,
					Token::In(s, _) => s,
					Token::Is(s, _) => s,
					Token::Lambda(s, _) => s,
					Token::Nonlocal(s, _) => s,
					Token::Not(s, _) => s,
					Token::Or(s, _) => s,
					Token::Pass(s, _) => s,
					Token::Raise(s, _) => s,
					Token::Return(s, _) => s,
					Token::Try(s, _) => s,
					Token::While(s, _) => s,
					Token::With(s, _) => s,
					Token::Yield(s, _) => s,
					Token::DoubleDivAssign(s, _) => s,
					Token::DoubleDiv(s, _) => s,
					Token::DivAssign(s, _) => s,
					Token::Div(s, _) => s,
					Token::PowerAssign(s, _) => s,
					Token::Power(s, _) => s,
					Token::MulAssign(s, _) => s,
					Token::Mul(s, _) => s,
					Token::Less(s, _) => s,
					Token::LessEqual(s, _) => s,
					Token::Equal(s, _) => s,
					Token::GreaterEqual(s, _) => s,
					Token::Greater(s, _) => s,
					Token::NotEqual(s, _) => s,
					Token::ShiftLeftAssign(s, _) => s,
					Token::ShiftLeft(s, _) => s,
					Token::ShiftRightAssign(s, _) => s,
					Token::ShiftRight(s, _) => s,
					Token::PlusAssign(s, _) => s,
					Token::Plus(s, _) => s,
					Token::MinusAssign(s, _) => s,
					Token::Minus(s, _) => s,
					Token::Arrow(s, _) => s,
					Token::ModuloAssign(s, _) => s,
					Token::Modulo(s, _) => s,
					Token::DecoratorAssign(s, _) => s,
					Token::Decorator(s, _) => s,
					Token::BitAndAssign(s, _) => s,
					Token::BitAnd(s, _) => s,
					Token::BitOrAssign(s, _) => s,
					Token::BitOr(s, _) => s,
					Token::BitXorAssign(s, _) => s,
					Token::BitXor(s, _) => s,
					Token::BitInvert(s, _) => s,
					Token::ColonAssign(s, _) => s,
					Token::Colon(s, _) => s,
					Token::SemiColon(s, _) => s,
					Token::Comma(s, _) => s,
					Token::Dot(s, _) => s,
					Token::Ellipsis(s, _) => s,
					Token::LeftParen(s, _) => s,
					Token::RightParen(s, _) => s,
					Token::LeftBracket(s, _) => s,
					Token::RightBracket(s, _) => s,
					Token::LeftCurly(s, _) => s,
					Token::RightCurly(s, _) => s,
					Token::Assign(s, _) => s,
					Token::Name(s, _ , _) => s,
					Token::String(s, _, _, _, _, _) => s,
					Token::Number(s, _ , _) => s,
					Token::TypeComment(s, _, _) => s
				}
			},
			_ => 0
		}
	}

	fn advance(&mut self) -> () {
		self.index = self.index + 1
	}
}

#[cfg(test)]
mod tests {
	use crate::parser::source_buffer::SourceBufferMethods;
	use super::*;

	#[test]
	fn test_general_parsing() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("for i in range(1, 100): pass\r\n");

		let mut parser = Parser::new(&mut buffer, 4);

		assert_eq!(parser.get_symbol(), Token::For(0, 3));
		assert_eq!(parser.get_position(), 0);
		parser.advance();

		assert_eq!(parser.get_symbol(), Token::Name(4, 5, "i".to_string()));
		assert_eq!(parser.get_position(), 4);
		parser.advance();

		assert_eq!(parser.get_symbol(), Token::In(6, 8));
		assert_eq!(parser.get_position(), 6);
		parser.advance();

		assert_eq!(parser.get_symbol(), Token::Name(9, 14, "range".to_string()));
		assert_eq!(parser.get_position(), 9);
		parser.advance();

		assert_eq!(parser.get_symbol(), Token::LeftParen(14, 15));
		assert_eq!(parser.get_position(), 14);
		parser.advance();

		assert_eq!(parser.get_symbol(), Token::Number(15, 16, "1".to_string()));
		assert_eq!(parser.get_position(), 15);
		parser.advance();

		assert_eq!(parser.get_symbol(), Token::Comma(16, 17));
		assert_eq!(parser.get_position(), 16);
		parser.advance();

		assert_eq!(parser.get_symbol(), Token::Number(18, 21, "100".to_string()));
		assert_eq!(parser.get_position(), 18);
		parser.advance();

		assert_eq!(parser.get_symbol(), Token::RightParen(21, 22));
		assert_eq!(parser.get_position(), 21);
		parser.advance();

		assert_eq!(parser.get_symbol(), Token::Colon(22, 23));
		assert_eq!(parser.get_position(), 22);
		parser.advance();

		assert_eq!(parser.get_symbol(), Token::Pass(24, 28));
		assert_eq!(parser.get_position(), 24);
		parser.advance();

		assert_eq!(parser.get_symbol(), Token::Newline(28, 30, '\r', '\n'));
		assert_eq!(parser.get_position(), 28);
		parser.advance();

		assert_eq!(parser.get_symbol(), Token::Eof(30));
		assert_eq!(parser.get_position(), 30);
	}
}