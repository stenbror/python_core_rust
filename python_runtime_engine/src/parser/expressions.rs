use crate::parser::parser::{Parser, ParserMethods};
use crate::parser::abstract_syntax_tree_nodes::ParseNode;
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
	fn parse_not_test(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_and_test(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_or_test(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_lambda(&mut self, is_cond : bool) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_test_no_cond(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_test(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_named_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_expr_list(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_test_list(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_arg_list(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_subscript_list(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_argument(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_subscript(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_test_list_comp(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_trailer(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_dictionary_set_maker(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_comp_iter(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_comp_for(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_comp_sync_for(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_comp_if(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_yield_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
	fn parse_test_list_star_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
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
			Token::LeftParen( _ , _ ) => {
				let symbol1 = self.get_symbol();
				self.advance();
				match self.get_symbol() {
					Token::RightParen( _ , _ ) => {
						let symbol2 = self.get_symbol();
						self.advance();
						Ok(Box::new(ParseNode::PyTuple(pos, self.get_position(), Box::new(symbol), None, Box::new(symbol2))))
					},
					_ => {
						Ok(Box::new(ParseNode::PyNone(0, 0, Box::new(Token::None(0, 0))))) // Temporary placeholder!
					}
				}
			},
			Token::LeftBracket( _ , _ ) => {
				let symbol1 = self.get_symbol();
				self.advance();
				match self.get_symbol() {
					Token::RightBracket( _ , _ ) => {
						let symbol2 = self.get_symbol();
						self.advance();
						Ok(Box::new(ParseNode::PyList(pos, self.get_position(), Box::new(symbol), None, Box::new(symbol2))))
					},
					_ => {
						Ok(Box::new(ParseNode::PyNone(0, 0, Box::new(Token::None(0, 0))))) // Temporary placeholder!
					}
				}
			},
			Token::LeftCurly( _ , _ ) => {
				let symbol1 = self.get_symbol();
				self.advance();
				match self.get_symbol() {
					Token::RightCurly( _ , _ ) => {
						let symbol2 = self.get_symbol();
						self.advance();
						Ok(Box::new(ParseNode::PyDictionary(pos, self.get_position(), Box::new(symbol), None, Box::new(symbol2))))
					},
					_ => {
						Ok(Box::new(ParseNode::PyNone(0, 0, Box::new(Token::None(0, 0))))) // Temporary placeholder!
					}
				}
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

	/// Rule: factor := ( '+' | '-' | '~' ) factor | power
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
					res = Box::new(ParseNode::PyEqual(pos, self.get_position(), res, Box::new(symbol_equal), right))
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

	/// Rule: not_test := 'not' not_test | comparison
	fn parse_not_test(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		match self.get_symbol() {
			Token::Not( _ , _ ) => {
				let symbol = self.get_symbol();
				self.advance();
				let right = self.parse_not_test()?;
				Ok(Box::new(ParseNode::PyNotTest(pos, self.get_position(), Box::new(symbol), right)))
			},
			_ => self.parse_comparison()
		}
	}

	/// Rule: and_test := not_test ('and' not_test)*
	fn parse_and_test(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let mut res = self.parse_not_test()?;

		loop {
			match self.get_symbol() {
				Token::And(_, _) => {
					let symbol_and_test = self.get_symbol();
					self.advance();
					let right = self.parse_not_test()?;
					res = Box::new(ParseNode::PyAndTest(pos, self.get_position(), res, Box::new(symbol_and_test), right))
				},
				_ => break
			}
		}

		Ok(res)
	}

	/// Rule: or_test := and_test ('or' and_test)*
	fn parse_or_test(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let mut res = self.parse_and_test()?;

		loop {
			match self.get_symbol() {
				Token::Or(_, _) => {
					let symbol_or_test = self.get_symbol();
					self.advance();
					let right = self.parse_and_test()?;
					res = Box::new(ParseNode::PyOrTest(pos, self.get_position(), res, Box::new(symbol_or_test), right))
				},
				_ => break
			}
		}

		Ok(res)
	}

	/// Rule: lambda := 'lambda' [ VarArgsList ] ':' ( test | test_no_cond )
	fn parse_lambda(&mut self, is_cond: bool) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		match self.get_symbol() {
			Token::Lambda(_ , _) => {
				let symbol1 = self.get_symbol();
				self.advance();

				let first : Option<Box<ParseNode>> = match self.get_symbol() {
					Token::Colon(_ , _) => None,
					_ => None // Some(parse_var_args_list()?)
				};

				match self.get_symbol() {
					Token::Colon(_ , _) => {
						let symbol2 = self.get_symbol();
						self.advance();
						let second = match is_cond {
							true => self.parse_test()?,
							false => self.parse_test_no_cond()?
						};

						Ok(Box::new(ParseNode::PyLambda(pos, self.get_position(), Box::new(symbol1), first, Box::new(symbol2), second)))
					},
					_ => Err(SyntaxError::new("Missing ':' in 'lambda' statement!".to_string(), self.get_position()))
 				}
			},
			_ => Err(SyntaxError::new("Expecting 'lambda' in lambda expression!".to_string(), self.get_position()))
		}
	}

	/// Rule: test_no_cond := or_test | lambda
	fn parse_test_no_cond(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		match self.get_symbol() {
			Token::Lambda(_ , _) => self.parse_lambda(false),
			_ => self.parse_or_test()
		}
	}

	/// Rule: test := or_test [ 'if' or_test 'else' test ] | lambda
	fn parse_test(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		match self.get_symbol() {
			Token::Lambda(_ , _) => self.parse_lambda(true),
			_ => {
				let first = self.parse_or_test()?;
				match self.get_symbol() {
					Token::If(_ , _ ) => {
						let symbol1 = self.get_symbol();
						self.advance();
						let second = self.parse_or_test()?;
						match self.get_symbol() {
							Token::Else(_ , _) => {
								let symbol2 = self.get_symbol();
								self.advance();
								let third = self.parse_test()?;
								Ok(Box::new(ParseNode::PyTest(pos, self.get_position(), first, Box::new(symbol1), second, Box::new(symbol2), third)))
							},
							_ => Err(SyntaxError::new("Expecting 'else' in test expression!".to_string(), self.get_position()))
						}
					},
					_ => Ok(first)
				}
			}
		}
	}

	/// Rule: named_expr := test ( ':=' test )
	fn parse_named_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let left = self.parse_test()?;
		match self.get_symbol() {
			Token::ColonAssign(_ , _) => {
				let symbol = self.get_symbol();
				self.advance();
				let right = self.parse_test()?;
				Ok(Box::new(ParseNode::PyNamedExpr(pos, self.get_position(), left, Box::new(symbol),right)))
			},
			_ => Ok(left)
		}
	}

	/// Rule: expr_list := (expr|star_expr) (',' (expr|star_expr))* [',']
	fn parse_expr_list(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let first = match self.get_symbol() {
			Token::Mul( _ , _ ) => self.parse_star_expr(),
			_ => self.parse_or_expr()
		};
		match self.get_symbol() {
			Token::Comma( _ , _ ) => {
				let mut symbols = Vec::<Box<Token>>::new();
				let mut nodes = Vec::<Box<ParseNode>>::new();
				nodes.push(first?);
				loop {
					match self.get_symbol() {
						Token::Comma( _ , _ ) => {
							symbols.push(Box::new(self.get_symbol()));
							self.advance();
							match self.get_symbol() {
								Token::In( _, _ ) => break,
								Token::Mul( _, _ ) => nodes.push( self.parse_star_expr()? ),
								_ => nodes.push( self.parse_or_expr()? )
							}
						},
						_ => break
					}
				};
				return Ok(Box::new(ParseNode::PyExprList(pos, self.get_position(), Box::new(nodes), Box::new(symbols))))
			},
			_ => ()
		}
		first
	}

	/// Rule: test_list := test (',' test)* [',']
	fn parse_test_list(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let first = self.parse_test();
		match self.get_symbol() {
			Token::Comma( _ , _ ) => {
				let mut symbols = Vec::<Box<Token>>::new();
				let mut nodes = Vec::<Box<ParseNode>>::new();
				nodes.push(first?);
				loop {
					match self.get_symbol() {
						Token::Comma( _ , _ ) => {
							symbols.push(Box::new(self.get_symbol()));
							self.advance();
							match self.get_symbol() {
								Token::SemiColon( _, _ ) | Token::Newline( _ , _ , _ , _ ) => break,
								_ => nodes.push( self.parse_test()? )
							}
						},
						_ => break
					}
				};
				return Ok(Box::new(ParseNode::PyTestList(pos, self.get_position(), Box::new(nodes), Box::new(symbols))))
			},
			_ => ()
		}
		first
	}

	/// Rule: arg_list := argument (',' argument)*  [',']
	fn parse_arg_list(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let first = self.parse_argument();
		match self.get_symbol() {
			Token::Comma( _ , _ ) => {
				let mut symbols = Vec::<Box<Token>>::new();
				let mut nodes = Vec::<Box<ParseNode>>::new();
				nodes.push(first?);
				loop {
					match self.get_symbol() {
						Token::Comma( _ , _ ) => {
							symbols.push(Box::new(self.get_symbol()));
							self.advance();
							match self.get_symbol() {
								Token::RightParen( _, _ ) => break,
								_ => nodes.push( self.parse_argument()? )
							}
						},
						_ => break
					}
				};
				return Ok(Box::new(ParseNode::PyArgList(pos, self.get_position(), Box::new(nodes), Box::new(symbols))))
			},
			_ => ()
		}
		first
	}

	/// Rule: subscript_list := subscript (',' subscript)* [',']
	fn parse_subscript_list(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let first = self.parse_subscript();
		match self.get_symbol() {
			Token::Comma( _ , _ ) => {
				let mut symbols = Vec::<Box<Token>>::new();
				let mut nodes = Vec::<Box<ParseNode>>::new();
				nodes.push(first?);
				loop {
					match self.get_symbol() {
						Token::Comma( _ , _ ) => {
							symbols.push(Box::new(self.get_symbol()));
							self.advance();
							match self.get_symbol() {
								Token::RightBracket( _, _ ) => break,
								_ => nodes.push( self.parse_subscript()? )
							}
						},
						_ => break
					}
				};
				return Ok(Box::new(ParseNode::PySubscriptList(pos, self.get_position(), Box::new(nodes), Box::new(symbols))))
			},
			_ => ()
		}
		first
	}

	/// Rule: argument := ( test [comp_for] |
	///             test ':=' test |
	///             test '=' test |
	///             '**' test |
	///             '*' test )
	fn parse_argument(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		match self.get_symbol() {
			Token::Mul( _ , _ ) => {
				let symbol = self.get_symbol();
				self.advance();
				let node = self.parse_test()?;
				Ok(Box::new(ParseNode::PyArgumentVarList(pos, self.get_position(), Box::new(symbol), node)))
			},
			Token::Power( _ , _ ) => {
				let symbol = self.get_symbol();
				self.advance();
				let node = self.parse_test()?;
				Ok(Box::new(ParseNode::PyArgumentKeywordList(pos, self.get_position(), Box::new(symbol), node)))
			},
			_ => {
				let left = self.parse_test()?;
				match self.get_symbol() {
					Token::For( _ , _ ) | Token::Async( _ , _ ) | Token::If( _ , _ ) => {
						let right = self.parse_comp_for()?;
						Ok(Box::new(ParseNode::PyArgument(pos, self.get_position(), left, None, Some(right))))
					},
					Token::ColonAssign( _ , _ ) | Token::Assign( _ , _ ) => {
						let symbol = self.get_symbol();
						self.advance();
						let right = self.parse_test()?;
						Ok(Box::new(ParseNode::PyArgument(pos, self.get_position(), left, Some(Box::new(symbol)), Some(right))))
					},
					_ => Ok(left)
				}
			}
		}
	}

	/// Rule: subscript := test | [test] ':' [test] [ ':' [test] ]
	fn parse_subscript(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let mut symbol1 : Option<Box<Token>> = None;
		let mut symbol2 : Option<Box<Token>> = None;
		let mut first : Option<Box<ParseNode>> = None;
		let mut second : Option<Box<ParseNode>> = None;
		let mut third : Option<Box<ParseNode>> = None;

		match self.get_symbol() {
			Token::Colon( _ , _ ) => (),
			_ => {
				first = Some(self.parse_test()?)
			}
		}

		match self.get_symbol() {
			Token::Colon( _ , _ ) => {
				symbol1 = Some(Box::new(self.get_symbol()));
				self.advance();
				match self.get_symbol() {
					Token::Comma( _ , _ ) | Token::RightBracket( _ , _ ) => (),
					_ => {
						match self.get_symbol() {
							Token::Colon( _ , _ ) => (),
							_ => second = Some(self.parse_test()?)
						}
						match self.get_symbol() {
							Token::Colon( _ , _ ) => {
								symbol2 = Some(Box::new(self.get_symbol()));
								self.advance();
								match self.get_symbol() {
									Token::Comma( _ , _ ) | Token::RightBracket( _ , _ ) => (),
									_ => third = Some(self.parse_test()?)
								}
							},
							_ => ()
						}
					}
				}
				Ok(Box::new(ParseNode::PySubscript(pos, self.get_position(), first, symbol1, second, symbol2, third)))
			},
			_ => {
				match first {
					Some(x) => Ok(x),
					_ => Err(SyntaxError::new("Subscript need one element or ':' to be valid!".to_string(), pos))
				}
			}
		}
	}

	/// Rule: test_list_comp := (namedexpr_test|star_expr) ( comp_for | (',' (namedexpr_test|star_expr))* [','] )
	fn parse_test_list_comp(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let first = match self.get_symbol() {
			Token::Mul( _ , _ ) => self.parse_star_expr()?,
			_ => self.parse_named_expr()?
		};
		match self.get_symbol() {
			Token::For( _ , _ ) | Token::Async( _ , _ ) => {
				let node = self.parse_comp_for()?;
				let mut nodes = Vec::<Box<ParseNode>>::new();
				nodes.push(first);
				nodes.push(node);
				Ok(Box::new(ParseNode::PyTestList(pos, self.get_position(), Box::new(nodes), Box::new(Vec::<Box<Token>>::new()))))
			},
			Token::Comma( _ , _ ) => {
				let mut nodes = Vec::<Box<ParseNode>>::new();
				nodes.push(first);
				let mut symbols = Vec::<Box<Token>>::new();
				loop {
					match self.get_symbol() {
						Token::Comma( _ , _ ) => {
							symbols.push(Box::new(self.get_symbol()));
							self.advance();
							match self.get_symbol() {
								Token::RightParen( _ , _ ) | Token::RightBracket( _ , _ ) => break,
								_ => nodes.push(match self.get_symbol() {
									Token::Mul( _ , _ ) => self.parse_star_expr()?,
									_ => self.parse_named_expr()?
								})
							}
						},
						_=> break
					}
				}
				Ok(Box::new(ParseNode::PyTestList(pos, self.get_position(), Box::new(nodes), Box::new(symbols))))
			},
			_ => Ok(first)
		}
	}

	fn parse_trailer(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		todo!()
	}

	fn parse_dictionary_set_maker(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		todo!()
	}

	/// Rule: comp_iter := comp_for | comp_if
	fn parse_comp_iter(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		match self.get_symbol() {
			Token::For( _ , _ ) | Token::Async( _ , _ ) => self.parse_comp_for(),
			_ => self.parse_comp_if()
		}
	}

	/// Rule: comp_for := [ASYNC] sync_comp_for
	fn parse_comp_for(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		match self.get_symbol() {
			Token::Async( _ , _ ) => {
				let symbol = self.get_symbol();
				self.advance();
				let node = self.parse_comp_sync_for()?;
				Ok(Box::new(ParseNode::PyCompFor(pos, self.get_position(), Box::new(symbol), node)))
			},
			_ => self.parse_comp_sync_for()
		}
	}

	/// Rule: comp_sync_for := 'for' exprlist 'in' or_test [comp_iter]
	fn parse_comp_sync_for(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		match self.get_symbol() {
			Token::For( _ , _ ) => {
				let symbol1 = self.get_symbol();
				self.advance();
				let left = self.parse_expr_list()?;
				match self.get_symbol() {
					Token::In( _ , _ ) => {
						let symbol2 = self.get_symbol();
						self.advance();
						let right = self.parse_or_test()?;
						let next = match self.get_symbol() {
							Token::For( _ , _ ) | Token::Async( _ , _ ) | Token::If( _ , _ ) => Some(self.parse_comp_iter()?),
							_ => None
						};
						Ok(Box::new(ParseNode::PySyncCompFor(pos, self.get_position(), Box::new(symbol1), left, Box::new(symbol2), right, next)))
					},
					_ => Err(SyntaxError::new("Expect 'in' in comprehension expression!".to_string(), pos))
				}
			},
			_ => Err(SyntaxError::new("Expect 'for' in comprehension expression!".to_string(), pos))
		}
	}

	/// Rule: comp_if := 'if' test_nocond [comp_iter]
	fn parse_comp_if(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		match self.get_symbol() {
			Token::If( _ , _ ) => {
				let symbol = self.get_symbol();
				self.advance();
				let left = self.parse_test_no_cond()?;
				let right = match self.get_symbol() {
					Token::For( _ , _ ) | Token::Async( _ , _ ) | Token::If( _ , _ ) => Some(self.parse_comp_iter()?),
					_ => None
				};
				Ok(Box::new(ParseNode::PyCompIf(pos, self.get_position(), Box::new(symbol), left, right)))
			},
			_ => Err(SyntaxError::new("Expect 'if' in comprehension expression!".to_string(), pos))
		}
	}

	/// Rule: yield_expr := yield' [ 'from' test | testlist_star_expr ]
	fn parse_yield_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		match self.get_symbol() {
			Token::Yield( _, _ ) => {
				let symbol1 = self.get_symbol();
				self.advance();
				match self.get_symbol() {
					Token::From( _ , _ ) => {
						let symbol2 = self.get_symbol();
						self.advance();
						let right = self.parse_test()?;
						Ok(Box::new(ParseNode::PyYieldFromExpr(pos, self.get_position(), Box::new(symbol1), Box::new(symbol2), right)))
					},
					_ => {
						let right = self.parse_test_list_star_expr()?;
						Ok(Box::new(ParseNode::PyYieldExpr(pos, self.get_position(), Box::new(symbol1), right)))
					}
				}
			},
			_ => Err(SyntaxError::new("Expect 'yield' in yield expression!".to_string(), pos))
		}
	}

	/// Rule: test_list_star_expr := (test|star_expr) (',' (test|star_expr))* [',']
	fn parse_test_list_star_expr(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
		let pos = self.get_position();
		let first = match self.get_symbol() {
			Token::Mul( _ , _ ) => self.parse_star_expr()?,
			_ => self.parse_test()?
		};
		match self.get_symbol() {
			Token::Comma( _ , _ ) => {
				let mut symbols = Vec::<Box<Token>>::new();
				let mut nodes = Vec::<Box<ParseNode>>::new();
				nodes.push(first);
				loop {
					match self.get_symbol() {
						Token::Comma( _ , _ ) => {
							symbols.push(Box::new(self.get_symbol()));
							self.advance();
							match self.get_symbol() {
								Token::RightParen( _, _ ) => break,
								_ => nodes.push( match self.get_symbol() {
									Token::Mul( _ , _ ) => self.parse_star_expr()?,
									_ => self.parse_test()?
								} )
							}
						},
						_ => break
					}
				};
				return Ok(Box::new(ParseNode::PyTestList(pos, self.get_position(), Box::new(nodes), Box::new(symbols))))
			},
			_ => ()
		}
		Ok(first)
	}
}

// Unittests for expression parser rules ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {

	use crate::parser::abstract_syntax_tree_nodes::ParseNode;
	use crate::parser::parser::ParserMethods;
	use crate::parser::source_buffer::{SourceBuffer, SourceBufferMethods};
	use super::*;


	#[test]
	fn parse_atom_empty_tuple() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("()\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_named_expr();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyTuple(0, 2, Box::new(Token::LeftParen(0, 1)), None, Box::new(Token::RightParen(1, 2)))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_atom_empty_list() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("[]\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_named_expr();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyList(0, 2, Box::new(Token::LeftBracket(0, 1)), None, Box::new(Token::RightBracket(1, 2)))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_atom_empty_dictionary() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("{}\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_named_expr();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyDictionary(0, 2, Box::new(Token::LeftCurly(0, 1)), None, Box::new(Token::RightCurly(1, 2)))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_named_expr() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a := b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_named_expr();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyNamedExpr(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::ColonAssign(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_empty_test() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_test();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyName(0, 1, Box::new(Token::Name(0, 1, "a".to_string())))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_test() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a if b else c\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_test();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyTest(0, 13,
											  Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))),
											  Box::new(Token::If(2, 4)),
											  Box::new(ParseNode::PyName(5, 7, Box::new(Token::Name(5, 6, "b".to_string())))),
											  Box::new(Token::Else(7, 11)),
											  Box::new(ParseNode::PyName(12, 13, Box::new(Token::Name(12, 13, "c".to_string())))))
				))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_or_test() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a or b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_or_test();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyOrTest(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Or(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_multiple_or_test() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a or b or c\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_or_test();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyOrTest(0, 11, Box::new(
					ParseNode::PyOrTest(0, 7, Box::new(
						ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))
					), Box::new(Token::Or(2, 4)), Box::new(
						ParseNode::PyName(5, 7, Box::new(Token::Name(5, 6, "b".to_string())))
					))
				), Box::new(Token::Or(7, 9)), Box::new(ParseNode::PyName(10, 11, Box::new(Token::Name(10, 11, "c".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_and_test() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a and b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_and_test();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyAndTest(0, 7, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::And(2, 5)), Box::new(ParseNode::PyName(6, 7, Box::new(Token::Name(6, 7, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_multiple_and_test() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a and b and c\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_and_test();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyAndTest(0, 13, Box::new(
					ParseNode::PyAndTest(0, 8, Box::new(
						ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))
					), Box::new(Token::And(2, 5)), Box::new(
						ParseNode::PyName(6, 8, Box::new(Token::Name(6, 7, "b".to_string())))
					))
				), Box::new(Token::And(8, 11)), Box::new(ParseNode::PyName(12, 13, Box::new(Token::Name(12, 13, "c".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_not_test() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("not a\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_not_test();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyNotTest(0, 5, Box::new(Token::Not(0, 3)), Box::new(ParseNode::PyName(4, 5, Box::new(Token::Name(4, 5, "a".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_multiple_not_test() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("not not a\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_not_test();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyNotTest(0, 9, Box::new(Token::Not(0, 3)),  Box::new(ParseNode::PyNotTest(4, 9, Box::new(Token::Not(4, 7)), Box::new(ParseNode::PyName(8, 9, Box::new(Token::Name(8, 9, "a".to_string()))))))   )))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_less_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a < b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_comparison();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyLess(0, 5, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Less(2, 3)), Box::new(ParseNode::PyName(4, 5, Box::new(Token::Name(4, 5, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_greater_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a > b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_comparison();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyGreater(0, 5, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Greater(2, 3)), Box::new(ParseNode::PyName(4, 5, Box::new(Token::Name(4, 5, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_less_equal_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a <= b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_comparison();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyLessEqual(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::LessEqual(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_greater_equal_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a >= b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_comparison();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyGreaterEqual(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::GreaterEqual(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_equal_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a == b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_comparison();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyEqual(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Equal(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_not_equal_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a != b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_comparison();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyNotEqual(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::NotEqual(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_is_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a is b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_comparison();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyIs(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Is(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_in_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a in b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_comparison();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyIn(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::In(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_is_not_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a is not b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_comparison();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyIsNot(0, 10, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Is(2, 4)), Box::new(Token::Not(5, 8)), Box::new(ParseNode::PyName(9, 10, Box::new(Token::Name(9, 10, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_single_not_in_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a not in b\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_comparison();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyNotIn(0, 10, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Not(2, 5)), Box::new(Token::In(6, 8)), Box::new(ParseNode::PyName(9, 10, Box::new(Token::Name(9, 10, "b".to_string())))))))
			},
			_ => assert!(false)
		}
	}

	#[test]
	fn parse_multiple_less_operator() {
		let mut buffer = SourceBuffer::new();
		buffer.from_text("a < b < c\r\n");

		let mut parser = Parser::new(&mut buffer, 4);
		let res = parser.parse_comparison();

		match res {
			Ok(x) => {
				assert_eq!(x, Box::new(ParseNode::PyLess(0, 9, Box::new(
					ParseNode::PyLess(0, 6, Box::new(
						ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))
					), Box::new(Token::Less(2, 3)), Box::new(
						ParseNode::PyName(4, 6, Box::new(Token::Name(4, 5, "b".to_string())))
					))
				), Box::new(Token::Less(6, 7)), Box::new(ParseNode::PyName(8, 9, Box::new(Token::Name(8, 9, "c".to_string())))))))
			},
			_ => assert!(false)
		}
	}

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