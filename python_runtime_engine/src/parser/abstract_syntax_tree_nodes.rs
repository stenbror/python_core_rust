use crate::parser::lexical_analyzer::Token;

/// Nodes that represents a Python sourcecode parsed correctly
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseNode {
	PyNone(u32, u32, Box<Token>),
	PyFalse(u32, u32, Box<Token>),
	PyTrue(u32, u32, Box<Token>),
	PyEllipsis(u32, u32, Box<Token>),
	PyName(u32, u32, Box<Token>),
	PyNumber(u32, u32, Box<Token>),
	PyString(u32, u32, Box<Vec<Box<Token>>>),
	PyAtomExpr(u32, u32, Option<Token>, Box<ParseNode>, Box<Vec<Box<ParseNode>>>),
	PyPower(u32, u32, Box<ParseNode>, Box<Token>, Box<ParseNode>),
	PyUnaryPlus(u32, u32, Box<Token>, Box<ParseNode>),
	PyUnaryMinus(u32, u32, Box<Token>, Box<ParseNode>),
	PyUnaryBitInvert(u32, u32, Box<Token>, Box<ParseNode>),
	PyMul(u32, u32, Box<ParseNode>, Box<Token>, Box<ParseNode>),
	PyDiv(u32, u32, Box<ParseNode>, Box<Token>, Box<ParseNode>),
	PyFloorDiv(u32, u32, Box<ParseNode>, Box<Token>, Box<ParseNode>),
	PyModulo(u32, u32, Box<ParseNode>, Box<Token>, Box<ParseNode>),
	PyMatrices(u32, u32, Box<ParseNode>, Box<Token>, Box<ParseNode>)
}