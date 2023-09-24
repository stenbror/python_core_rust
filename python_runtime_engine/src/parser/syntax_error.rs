
pub trait SyntaxErrorMethods {
	fn new(msg: String, pos: u32) -> Self;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyntaxError {
	message: String,
	position: u32
}

impl SyntaxErrorMethods for SyntaxError {
	fn new(msg: String, pos: u32) -> Self {
		SyntaxError {
			message: msg,
			position: pos
		}
	}
}