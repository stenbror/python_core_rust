use crate::parser::lexical_analyzer::Token;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseNode {
	PyNone(u32, u32, Box<Token>),
	PyFalse(u32, u32, Box<Token>),
	PyTrue(u32, u32, Box<Token>),
	PyEllipsis(u32, u32, Box<Token>)
}