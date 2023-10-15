use crate::parser::abstract_syntax_tree_nodes::ParseNode;
use crate::parser::expressions::ExpressionMethods;
use crate::parser::lexical_analyzer::Token;
use crate::parser::parser::{Parser, ParserMethods};
use crate::parser::syntax_error::{SyntaxError, SyntaxErrorMethods};

pub trait StatementMethods {
    fn parse_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_simple_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_small_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_expr_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_annotate_assignment(&mut self, pos: u32,  left : Box<ParseNode>) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_del_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_pass_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_flow_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_break_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_continue_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_return_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_yield_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_raise_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_import_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_import_name_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_import_from_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_import_as_name_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_dotted_name_as_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_import_as_names_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_dotted_as_names_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_dotted_name_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_global_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_nonlocal_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_assert_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
    fn parse_compound_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
}

impl StatementMethods for Parser {

    /// Rule: stmt := compound_stmt | simple_stmt
    fn parse_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        match self.get_symbol() {
            Token::Name( _ , _ , text ) => {
                match text.as_str() {
                    "match" => self.parse_compound_stmt(), // Positional keyword 'match'
                    _ => self.parse_simple_stmt()
                }
            },
            Token::If( _ , _ ) |
            Token::For( _ , _ ) |
            Token::Async( _ , _ ) |
            Token::While( _ , _ ) |
            Token::With( _ , _ ) |
            Token::Try( _ , _ ) |
            Token::Class( _ , _ ) |
            Token::Def( _ , _ ) |
            Token::Decorator( _ , _ ) => self.parse_compound_stmt(),
            _ => self.parse_simple_stmt()
        }
    }

    /// Rule: simple_stmt := small_stmt (';' small_stmt)* [';'] NEWLINE
    fn parse_simple_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        let pos = self.get_position();
        let mut nodes = Vec::<Box<ParseNode>>::new();
        let mut separators = Vec::<Box<Token>>::new();
        nodes.push(self.parse_small_stmt()?);
        loop {
            match self.get_symbol() {
                Token::SemiColon( _ , _ ) => {
                    separators.push(Box::new(self.get_symbol()));
                    self.advance();
                    match self.get_symbol() {
                        Token::Newline( _ , _ , _ , _ ) => break,
                        _ => nodes.push(self.parse_small_stmt()?)
                    }
                },
                _ => break
            }
        }
        match self.get_symbol() {
            Token::Newline( _ , _ , _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                Ok(Box::new(ParseNode::PySimpleStmt(pos, self.get_position(), Box::new(nodes), Box::new(separators), Box::new(symbol))))
            },
            _ => Err(SyntaxError::new("Missing <NewLine> in simple statement!".to_string(), pos))
        }
    }

    /// Rule: small_stmt := (expr_stmt | del_stmt | pass_stmt | flow_stmt |
    ///              import_stmt | global_stmt | nonlocal_stmt | assert_stmt)
    fn parse_small_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        match self.get_symbol() {
            Token::Pass( _ , _ ) => self.parse_pass_stmt(),
            Token::Del( _ , _ ) => self.parse_del_stmt(),
            Token::Break( _ , _ ) |
            Token::Continue( _ , _ ) |
            Token::Return( _ , _ ) |
            Token::Raise( _ , _ ) |
            Token::Yield( _ , _ ) => self.parse_flow_stmt(),
            Token::Import( _ , _ ) |
            Token::From( _ , _ ) => self.parse_import_stmt(),
            Token::Global( _ , _ ) => self.parse_global_stmt(),
            Token::Nonlocal( _ , _ ) => self.parse_nonlocal_stmt(),
            Token::Assert( _ , _ ) => self.parse_assert_stmt(),
            _ => self.parse_expr_list()
        }
    }

    /// Rule: expr_stmt := testlist_star_expr (annassign | augassign (yield_expr|testlist) |
    ///                    [('=' (yield_expr|testlist_star_expr))+ [TYPE_COMMENT]] )
    ///                     ('+=' | '-=' | '*=' | '@=' | '/=' | '%=' | '&=' | '|=' | '^=' |
    ///                     '<<=' | '>>=' | '**=' | '//=')
    fn parse_expr_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        let pos = self.get_position();
        let left = self.parse_test_list_star_expr()?;
        match self.get_symbol() {
            Token::PlusAssign( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                let right = match self.get_symbol() {
                    Token::Yield( _ , _ ) => self.parse_yield_expr()?,
                    _ => self.parse_test_list()?
                };
                Ok(Box::new(ParseNode::PyPlusAssign(pos, self.get_position(), left, Box::new(symbol), right)))
            },
            Token::MinusAssign( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                let right = match self.get_symbol() {
                    Token::Yield( _ , _ ) => self.parse_yield_expr()?,
                    _ => self.parse_test_list()?
                };
                Ok(Box::new(ParseNode::PyMinusAssign(pos, self.get_position(), left, Box::new(symbol), right)))
            },
            Token::MulAssign( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                let right = match self.get_symbol() {
                    Token::Yield( _ , _ ) => self.parse_yield_expr()?,
                    _ => self.parse_test_list()?
                };
                Ok(Box::new(ParseNode::PyMulAssign(pos, self.get_position(), left, Box::new(symbol), right)))
            },
            Token::DivAssign( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                let right = match self.get_symbol() {
                    Token::Yield( _ , _ ) => self.parse_yield_expr()?,
                    _ => self.parse_test_list()?
                };
                Ok(Box::new(ParseNode::PyDivAssign(pos, self.get_position(), left, Box::new(symbol), right)))
            },
            Token::DoubleDivAssign( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                let right = match self.get_symbol() {
                    Token::Yield( _ , _ ) => self.parse_yield_expr()?,
                    _ => self.parse_test_list()?
                };
                Ok(Box::new(ParseNode::PyFloorDivAssign(pos, self.get_position(), left, Box::new(symbol), right)))
            },
            Token::PowerAssign( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                let right = match self.get_symbol() {
                    Token::Yield( _ , _ ) => self.parse_yield_expr()?,
                    _ => self.parse_test_list()?
                };
                Ok(Box::new(ParseNode::PyPowerAssign(pos, self.get_position(), left, Box::new(symbol), right)))
            },
            Token::ModuloAssign( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                let right = match self.get_symbol() {
                    Token::Yield( _ , _ ) => self.parse_yield_expr()?,
                    _ => self.parse_test_list()?
                };
                Ok(Box::new(ParseNode::PyModuloAssign(pos, self.get_position(), left, Box::new(symbol), right)))
            },
            Token::BitAndAssign( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                let right = match self.get_symbol() {
                    Token::Yield( _ , _ ) => self.parse_yield_expr()?,
                    _ => self.parse_test_list()?
                };
                Ok(Box::new(ParseNode::PyBitAndAssign(pos, self.get_position(), left, Box::new(symbol), right)))
            },
            Token::BitXorAssign( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                let right = match self.get_symbol() {
                    Token::Yield( _ , _ ) => self.parse_yield_expr()?,
                    _ => self.parse_test_list()?
                };
                Ok(Box::new(ParseNode::PyBitXorAssign(pos, self.get_position(), left, Box::new(symbol), right)))
            },
            Token::BitOrAssign( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                let right = match self.get_symbol() {
                    Token::Yield( _ , _ ) => self.parse_yield_expr()?,
                    _ => self.parse_test_list()?
                };
                Ok(Box::new(ParseNode::PyBitOrAssign(pos, self.get_position(), left, Box::new(symbol), right)))
            },
            Token::ShiftLeftAssign( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                let right = match self.get_symbol() {
                    Token::Yield( _ , _ ) => self.parse_yield_expr()?,
                    _ => self.parse_test_list()?
                };
                Ok(Box::new(ParseNode::PyShiftLeftAssign(pos, self.get_position(), left, Box::new(symbol), right)))
            },
            Token::ShiftRightAssign( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                let right = match self.get_symbol() {
                    Token::Yield( _ , _ ) => self.parse_yield_expr()?,
                    _ => self.parse_test_list()?
                };
                Ok(Box::new(ParseNode::PyShiftRightAssign(pos, self.get_position(), left, Box::new(symbol), right)))
            },
            Token::Assign( _ , _ ) => {
                let mut symbols : Vec<Box<Token>> = Vec::new();
                let mut nodes : Vec<Box<ParseNode>> = Vec::new();
                loop {
                    symbols.push(Box::new(self.get_symbol()));
                    self.advance();
                    match self.get_symbol() {
                        Token::Yield( _ ,_ ) => nodes.push(self.parse_yield_expr()?),
                        _ => nodes.push(self.parse_test_list_star_expr()?)
                    };
                    match self.get_symbol() {
                        Token::Assign( _ , _ ) => (),
                        _ => break
                    }
                };
                match self.get_symbol() {
                    Token::TypeComment( _ , _ , _ ) => {
                        let symbol = self.get_symbol();
                        self.advance();
                        Ok(Box::new(ParseNode::PyAssignment(pos, self.get_position(), left, Box::new(symbols), Box::new(nodes), Some(Box::new(symbol)))))
                    },
                    _ => Ok(Box::new(ParseNode::PyAssignment(pos, self.get_position(), left, Box::new(symbols), Box::new(nodes), None)))
                }
            },
            Token::Colon( _, _ ) => self.parse_annotate_assignment(pos, left),
            _ => Ok(left)
        }
    }

    /// Rule: annotade_assignment := ':' test ['=' (yield_expr|testlist_star_expr)]
    fn parse_annotate_assignment(&mut self, pos: u32, left : Box<ParseNode>) -> Result<Box<ParseNode>, SyntaxError> {
        let symbol = self.get_symbol();
        self.advance();
        let right = self.parse_test()?;
        match self.get_symbol() {
            Token::Assign( _ , _ ) => {
                let symbol2 = self.get_symbol();
                self.advance();
                let next = match self.get_symbol() {
                    Token::Yield( _ , _ ) => self.parse_yield_expr()?,
                    _ => self.parse_test_list()?
                };
                Ok(Box::new(ParseNode::PyAnnotatedAssign(pos, self.get_position(), left, Box::new(symbol), right, Some(Box::new(symbol2)),Some(next))))
            },
            _ => Ok(Box::new(ParseNode::PyAnnotatedAssign(pos, self.get_position(), left, Box::new(symbol), right, None,None)))
        }
    }

    /// Rule: del_stmt := 'del' exprlist
    fn parse_del_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        let pos = self.get_position();
        match self.get_symbol() {
            Token::Del( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                let node = self.parse_expr_list()?;
                Ok(Box::new(ParseNode::PyDel(pos, self.get_position(), Box::new(symbol), node)))
            },
            _ => Err(SyntaxError::new("Missing 'del' in del statement!".to_string(), pos))
        }
    }

    /// Rule: pass_stmt := 'pass'
    fn parse_pass_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        let pos = self.get_position();
        match self.get_symbol() {
            Token::Pass( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                Ok(Box::new(ParseNode::PyPass(pos, self.get_position(), Box::new(symbol))))
            },
            _ => Err(SyntaxError::new("Missing <NewLine> in simple statement!".to_string(), pos))
        }
    }

    /// flow_stmt := break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
    fn parse_flow_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        match self.get_symbol() {
            Token::Break( _ , _ ) => {

                self.parse_break_stmt()
            },
            Token::Continue( _ , _ ) => {

                self.parse_continue_stmt()
            },
            Token::Return( _ , _ ) => {

                self.parse_return_stmt()
            },
            Token::Raise( _ , _ ) => {

                self.parse_raise_stmt()
            },
            Token::Yield( _ , _ ) => {

                self.parse_yield_stmt()
            },
            _ => {
                Err(SyntaxError::new("Expecting 'break', 'continue', 'return', 'raise' or 'yield' in flow statement!".to_string(), self.get_position()))
            }
        }
    }

    /// Rule: break_stmt := 'break'
    fn parse_break_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        let pos = self.get_position();
        match self.get_symbol() {
            Token::Break( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                Ok(Box::new(ParseNode::PyBreak(pos, self.get_position(), Box::new(symbol))))
            },
            _ => Err(SyntaxError::new("Expecting 'break' in break statement!".to_string(), pos))
        }
    }

    /// Rule: continue_stmt := 'continue'
    fn parse_continue_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        let pos = self.get_position();
        match self.get_symbol() {
            Token::Continue( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                Ok(Box::new(ParseNode::PyContinue(pos, self.get_position(), Box::new(symbol))))
            },
            _ => Err(SyntaxError::new("Expecting 'continue' in continue statement!".to_string(), pos))
        }
    }

    /// Rule: return_stmt := 'return' [testlist_star_expr]
    fn parse_return_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        let pos = self.get_position();
        match self.get_symbol() {
            Token::Return( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                match self.get_symbol() {
                    Token::Newline( _ , _ , _ , _ ) |
                    Token::SemiColon( _ , _ ) => {
                        Ok(Box::new(ParseNode::PyReturn(pos, self.get_position(), Box::new(symbol), None)))
                    },
                    _ => {
                        let right = self.parse_test_list_star_expr()?;
                        Ok(Box::new(ParseNode::PyReturn(pos, self.get_position(), Box::new(symbol), Some(right))))
                    }
                }
            },
            _ => Err(SyntaxError::new("Expecting 'return' in return statement!".to_string(), pos))
        }
    }

    /// Rule: yield_stmt := [yield_expr]
    fn parse_yield_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        self.parse_yield_expr()
    }

    /// Rule: raise_stmt := 'raise' [test ['from' test]]
    fn parse_raise_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        let pos = self.get_position();
        match self.get_symbol() {
            Token::Raise( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                let left = self.parse_test()?;
                match self.get_symbol() {
                    Token::From( _ , _ ) => {
                        let symbol2 = self.get_symbol();
                        self.advance();
                        let right = self.parse_test()?;
                        Ok(Box::new(ParseNode::PyRaise(pos, self.get_position(), Box::new(symbol), left, Some(Box::new(symbol2)), Some(right))))
                    },
                    _ => Ok(Box::new(ParseNode::PyRaise(pos, self.get_position(), Box::new(symbol), left, None, None)))
                }
            },
            _ => Err(SyntaxError::new("Expecting 'raise' in raise statement!".to_string(), pos))
        }
    }

    /// Rule: import_stmt := import_name | import_from
    fn parse_import_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        match self.get_symbol() {
            Token::Import( _ , _ ) => self.parse_import_name_stmt(),
            _ => self.parse_import_from_stmt()
        }
    }

    /// Rule:: import_name := 'import' dotted_as_names
    fn parse_import_name_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        let pos = self.get_position();
        match self.get_symbol() {
            Token::Import( _ ,  _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                let right = self.parse_dotted_as_names_stmt()?;
                Ok(Box::new(ParseNode::PyImport(pos, self.get_position(), Box::new(symbol), right)))
            },
            _ => Err(SyntaxError::new("Missing 'import' in import statement!".to_string(), pos))
        }
    }

    fn parse_import_from_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_import_as_name_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    /// Rule: dotted_as_name := dotted_name ['as' NAME]
    fn parse_dotted_name_as_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        let pos = self.get_position();
        let left = self.parse_dotted_name_stmt()?;
        match self.get_symbol() {
            Token::As( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                match self.get_symbol() {
                    Token::Name( _ , _ , _ ) => {
                        let right = self.get_symbol();
                        self.advance();
                        Ok(Box::new(ParseNode::PyDottedAsName(pos, self.get_position(), left, Box::new(symbol), Box::new(right))))
                    },
                    _ => Err(SyntaxError::new("Missing Name literal after 'as' in dotted as name statement!".to_string(), pos))
                }
            },
            _ => Ok(left)
        }
    }

    fn parse_import_as_names_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    /// Rule: dotted_as_names := dotted_as_name (',' dotted_as_name)*
    fn parse_dotted_as_names_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        let pos = self.get_position();
        let mut nodes : Vec<Box<ParseNode>> = Vec::new();
        let mut separators : Vec<Box<Token>> = Vec::new();
        nodes.push(self.parse_dotted_name_as_stmt()?);
        loop {
            match self.get_symbol() {
                Token::Comma( _ , _ ) => {
                    separators.push(Box::new(self.get_symbol()));
                    self.advance();
                    nodes.push(self.parse_dotted_name_as_stmt()?)
                },
                _ => break
            }
        }
        Ok(Box::new(ParseNode::PyDottedAsNames(pos, self.get_position(), Box::new(nodes), Box::new(separators))))
    }

    /// Rule: dotted_name_stmt := NAME ('.' NAME)*
    fn parse_dotted_name_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        let pos = self.get_position();
        let mut names_node : Vec<Box<Token>> = Vec::new();
        let mut separators : Vec<Box<Token>> = Vec::new();
        match self.get_symbol() {
            Token::Name( _ , _ , _ ) => {
                names_node.push(Box::new(self.get_symbol()));
                self.advance();
                loop {
                    match self.get_symbol() {
                        Token::Dot( _ , _ ) => {
                            separators.push(Box::new(self.get_symbol()));
                            self.advance();
                            match self.get_symbol() {
                                Token::Name( _ , _ , _ ) => {
                                    names_node.push(Box::new(self.get_symbol()));
                                    self.advance()
                                },
                                _ => break
                            }
                        },
                        _ => break
                    }
                }
            },
            _ => return Err(SyntaxError::new("Missing Name literal in dotted name statement!".to_string(), pos))
        }
        Ok(Box::new(ParseNode::PyDottedName(pos, self.get_position(), Box::new(names_node), Box::new(separators))))
    }

    /// Rule: global_stmt := global' NAME (',' NAME)*
    fn parse_global_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        let pos = self.get_position();
        match self.get_symbol() {
            Token::Global( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                match self.get_symbol() {
                    Token::Name( _ , _ , _ ) => {
                        let mut names = Vec::<Box<Token>>::new();
                        let mut separators = Vec::<Box<Token>>::new();
                        names.push(Box::new(self.get_symbol()));
                        self.advance();
                        loop {
                            match self.get_symbol() {
                                Token::Comma( _ , _ ) => {
                                    separators.push(Box::new(self.get_symbol()));
                                    self.advance();
                                    match self.get_symbol() {
                                        Token::Name( _ , _ , _ ) => {
                                            names.push(Box::new(self.get_symbol()));
                                            self.advance()
                                        }
                                        _ => return Err(SyntaxError::new("Missing <NAME> literal after ',' in global statement!".to_string(), pos))
                                    }
                                },
                                _ => break
                            }
                        }
                        Ok(Box::new(ParseNode::PyGlobal(pos, self.get_position(), Box::new(symbol), Box::new(names), Box::new(separators))))
                    },
                    _ => Err(SyntaxError::new("Missing <NAME> literal in global statement!".to_string(), pos))
                }
            },
            _ => Err(SyntaxError::new("Missing 'global' in global statement!".to_string(), pos))
        }
    }

    /// Rule: nonlocal := 'nonlocal' NAME (',' NAME)*
    fn parse_nonlocal_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        let pos = self.get_position();
        match self.get_symbol() {
            Token::Nonlocal( _ , _ ) => {
                let symbol = self.get_symbol();
                self.advance();
                match self.get_symbol() {
                    Token::Name( _ , _ , _ ) => {
                        let mut names = Vec::<Box<Token>>::new();
                        let mut separators = Vec::<Box<Token>>::new();
                        names.push(Box::new(self.get_symbol()));
                        self.advance();
                        loop {
                            match self.get_symbol() {
                                Token::Comma( _ , _ ) => {
                                    separators.push(Box::new(self.get_symbol()));
                                    self.advance();
                                    match self.get_symbol() {
                                        Token::Name( _ , _ , _ ) => {
                                            names.push(Box::new(self.get_symbol()));
                                            self.advance()
                                        }
                                        _ => return Err(SyntaxError::new("Missing <NAME> literal after ',' in nonlocal statement!".to_string(), pos))
                                    }
                                },
                                _ => break
                            }
                        }
                        Ok(Box::new(ParseNode::PyNonLocal(pos, self.get_position(), Box::new(symbol), Box::new(names), Box::new(separators))))
                    },
                    _ => Err(SyntaxError::new("Missing <NAME> literal in nonlocal statement!".to_string(), pos))
                }
            },
            _ => Err(SyntaxError::new("Missing 'nonlocal' in nonlocal statement!".to_string(), pos))
        }
    }

    /// Rule: assert := 'assert' test [',' test]
    fn parse_assert_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        let pos = self.get_position();
        match self.get_symbol() {
            Token::Assert( _ , _ ) => {
                let symbol1 = self.get_symbol();
                self.advance();
                let first = self.parse_test()?;
                match self.get_symbol() {
                    Token::Comma( _ , _ ) => {
                        let symbol2 = self.get_symbol();
                        self.advance();
                        let second = self.parse_test()?;
                        return Ok(Box::new(ParseNode::PyAssert(pos, self.get_position(), Box::new(symbol1), first, Some(Box::new(symbol2)), Some(second))))
                    }
                    _ => ()
                }
                Ok(Box::new(ParseNode::PyAssert(pos, self.get_position(), Box::new(symbol1), first, None, None)))
            },
            _ => Err(SyntaxError::new("Missing 'assert' in assert statement!".to_string(), pos))
        }
    }

    fn parse_compound_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }
}


// Unittests for statement parser rules ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use crate::parser::abstract_syntax_tree_nodes::ParseNode;
    use crate::parser::expressions::ExpressionMethods;
    use crate::parser::lexical_analyzer::Token;
    use crate::parser::parser::{Parser, ParserMethods};
    use crate::parser::source_buffer::{SourceBuffer, SourceBufferMethods};
    use crate::parser::statements::StatementMethods;

    #[test]
    fn parse_pass_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("pass\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_stmt();

        let mut nodes = Vec::<Box<ParseNode>>::new();
        nodes.push(Box::new(ParseNode::PyPass(0, 4, Box::new(Token::Pass(0, 4)))));
        let mut separators = Vec::<Box<Token>>::new();
        let newline = Box::new(Token::Newline(4, 6, '\r', '\n'));

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PySimpleStmt(0, 6, Box::new(nodes), Box::new(separators), newline)))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_del_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("del a\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_stmt();

        let mut nodes = Vec::<Box<ParseNode>>::new();
        nodes.push(Box::new(ParseNode::PyDel(0, 5, Box::new(Token::Del(0, 3)), Box::new(ParseNode::PyName(4, 5, Box::new(Token::Name(4, 5, "a".to_string())))))));
        let mut separators = Vec::<Box<Token>>::new();
        let newline = Box::new(Token::Newline(5, 7, '\r', '\n'));

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PySimpleStmt(0, 7, Box::new(nodes), Box::new(separators), newline)))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_global_statement_single()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("global a\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_stmt();

        let mut nodes = Vec::<Box<ParseNode>>::new();

        let mut nodes_global = Vec::<Box<Token>>::new();
        nodes_global.push(Box::new(Token::Name(7, 8, "a".to_string())));

        let mut separators = Vec::<Box<Token>>::new();

        nodes.push(Box::new(ParseNode::PyGlobal(0, 8, Box::new(Token::Global(0, 6)), Box::new(nodes_global), Box::new(separators))));

        let mut separators = Vec::<Box<Token>>::new();
        let newline = Box::new(Token::Newline(8, 10, '\r', '\n'));

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PySimpleStmt(0, 10, Box::new(nodes), Box::new(separators), newline)))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_global_statement_multiple()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("global a, b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_stmt();

        let mut nodes = Vec::<Box<ParseNode>>::new();

        let mut nodes_global = Vec::<Box<Token>>::new();
        nodes_global.push(Box::new(Token::Name(7, 8, "a".to_string())));
        nodes_global.push(Box::new(Token::Name(10, 11, "b".to_string())));

        let mut separators = Vec::<Box<Token>>::new();
        separators.push(Box::new(Token::Comma(8, 9)));

        nodes.push(Box::new(ParseNode::PyGlobal(0, 11, Box::new(Token::Global(0, 6)), Box::new(nodes_global), Box::new(separators))));

        let mut separators = Vec::<Box<Token>>::new();
        let newline = Box::new(Token::Newline(11, 13, '\r', '\n'));

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PySimpleStmt(0, 13, Box::new(nodes), Box::new(separators), newline)))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_nonlocal_statement_single()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("nonlocal a\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_stmt();

        let mut nodes = Vec::<Box<ParseNode>>::new();

        let mut nodes_nonlocal = Vec::<Box<Token>>::new();
        nodes_nonlocal.push(Box::new(Token::Name(9, 10, "a".to_string())));

        let mut separators = Vec::<Box<Token>>::new();

        nodes.push(Box::new(ParseNode::PyNonLocal(0, 10, Box::new(Token::Nonlocal(0, 8)), Box::new(nodes_nonlocal), Box::new(separators))));

        let mut separators = Vec::<Box<Token>>::new();
        let newline = Box::new(Token::Newline(10, 12, '\r', '\n'));

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PySimpleStmt(0, 12, Box::new(nodes), Box::new(separators), newline)))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_nonlocal_statement_multiple()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("nonlocal a, b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_stmt();

        let mut nodes = Vec::<Box<ParseNode>>::new();

        let mut nodes_nonlocal = Vec::<Box<Token>>::new();
        nodes_nonlocal.push(Box::new(Token::Name(9, 10, "a".to_string())));
        nodes_nonlocal.push(Box::new(Token::Name(12, 13, "b".to_string())));

        let mut separators = Vec::<Box<Token>>::new();
        separators.push(Box::new(Token::Comma(10, 11)));

        nodes.push(Box::new(ParseNode::PyNonLocal(0, 13, Box::new(Token::Nonlocal(0, 8)), Box::new(nodes_nonlocal), Box::new(separators))));

        let mut separators = Vec::<Box<Token>>::new();
        let newline = Box::new(Token::Newline(13, 15, '\r', '\n'));

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PySimpleStmt(0, 15, Box::new(nodes), Box::new(separators), newline)))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_assert_statement_single()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("assert a\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_stmt();

        let mut nodes = Vec::<Box<ParseNode>>::new();
        nodes.push(Box::new(ParseNode::PyAssert(0, 8, Box::new(Token::Assert(0, 6)), Box::new(ParseNode::PyName(7, 8, Box::new(Token::Name(7, 8, "a".to_string())))), None, None)));

        let mut separators = Vec::<Box<Token>>::new();
        let newline = Box::new(Token::Newline(8, 10, '\r', '\n'));

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PySimpleStmt(0, 10, Box::new(nodes), Box::new(separators), newline)))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_assert_statement_multiple()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("assert a, b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_stmt();

        let mut nodes = Vec::<Box<ParseNode>>::new();
        nodes.push(Box::new(ParseNode::PyAssert(0, 11, Box::new(Token::Assert(0, 6)), Box::new(ParseNode::PyName(7, 8, Box::new(Token::Name(7, 8, "a".to_string())))), Some(Box::new(Token::Comma(8, 9))), Some(Box::new(ParseNode::PyName(10, 11, Box::new(Token::Name(10, 11, "b".to_string()))))))));

        let mut separators = Vec::<Box<Token>>::new();
        let newline = Box::new(Token::Newline(11,13, '\r', '\n'));

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PySimpleStmt(0, 13, Box::new(nodes), Box::new(separators), newline)))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_simple_break_statement_without_flow_control()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("break\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_break_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyBreak(0, 5, Box::new(Token::Break(0, 5)))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_simple_continue_statement_without_flow_control()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("continue\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_continue_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyContinue(0, 8, Box::new(Token::Continue(0, 8)))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_simple_return_statement_without_flow_control()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("return\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_return_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyReturn(0, 6, Box::new(Token::Return(0, 6)), None)))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_simple_return_statement_with_list_without_flow_control()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("return a\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_return_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyReturn(0, 8, Box::new(Token::Return(0, 6)), Some(Box::new(ParseNode::PyName(7, 8, Box::new(Token::Name(7, 8, "a".to_string()))))))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_simple_raise_statement_with_single_argument_without_flow_control()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("raise a\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_raise_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyRaise(0, 7, Box::new(Token::Raise(0, 5)), Box::new(ParseNode::PyName(6, 7, Box::new(Token::Name(6, 7, "a".to_string())))), None, None)))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_simple_raise_statement_with_double_argument_without_flow_control()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("raise a from b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_raise_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyRaise(0, 14, Box::new(Token::Raise(0, 5)), Box::new(ParseNode::PyName(6, 8, Box::new(Token::Name(6, 7, "a".to_string())))), Some(Box::new(Token::From(8, 12))), Some(Box::new(ParseNode::PyName(13, 14, Box::new(Token::Name(13, 14, "b".to_string()))))))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_plus_assign_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a += b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyPlusAssign(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::PlusAssign(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_plus_assign_yield_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a += yield b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyPlusAssign(0, 12, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::PlusAssign(2, 4)), Box::new(ParseNode::PyYieldExpr(5, 12, Box::new(Token::Yield(5, 10)), Box::new(ParseNode::PyName(11, 12, Box::new(Token::Name(11, 12, "b".to_string())))))))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_minus_assign_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a -= b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyMinusAssign(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::MinusAssign(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_mul_assign_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a *= b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyMulAssign(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::MulAssign(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_div_assign_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a /= b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyDivAssign(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::DivAssign(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_modulo_assign_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a %= b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyModuloAssign(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::ModuloAssign(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_bit_and_assign_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a &= b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyBitAndAssign(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::BitAndAssign(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_bit_xor_assign_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a ^= b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyBitXorAssign(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::BitXorAssign(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_bit_or_assign_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a |= b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyBitOrAssign(0, 6, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::BitOrAssign(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_floor_div_assign_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a //= b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyFloorDivAssign(0, 7, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::DoubleDivAssign(2, 5)), Box::new(ParseNode::PyName(6, 7, Box::new(Token::Name(6, 7, "b".to_string())))))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_power_assign_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a **= b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyPowerAssign(0, 7, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::PowerAssign(2, 5)), Box::new(ParseNode::PyName(6, 7, Box::new(Token::Name(6, 7, "b".to_string())))))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_shift_left_assign_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a <<= b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyShiftLeftAssign(0, 7, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::ShiftLeftAssign(2, 5)), Box::new(ParseNode::PyName(6, 7, Box::new(Token::Name(6, 7, "b".to_string())))))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_shift_right_assign_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a >>= b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyShiftRightAssign(0, 7, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::ShiftRightAssign(2, 5)), Box::new(ParseNode::PyName(6, 7, Box::new(Token::Name(6, 7, "b".to_string())))))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_non_assign_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyName(0, 1, Box::new(Token::Name(0, 1, "a".to_string())))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_annotated_simple_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a : b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyAnnotatedAssign(0, 5, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Colon(2, 3)), Box::new(ParseNode::PyName(4, 5, Box::new(Token::Name(4, 5, "b".to_string())))), None, None)))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_annotated_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a : b = c\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyAnnotatedAssign(0, 9, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Colon(2, 3)), Box::new(ParseNode::PyName(4, 6, Box::new(Token::Name(4, 5, "b".to_string())))), Some(Box::new(Token::Assign(6, 7))), Some(Box::new(ParseNode::PyName(8, 9, Box::new(Token::Name(8, 9, "c".to_string()))))))))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_assign_single_no_type_comment_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a = b\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        let mut right_symbols : Vec<Box<Token>> = Vec::new();
        right_symbols.push(Box::new(Token::Assign(2, 3)));

        let mut right_nodes : Vec<Box<ParseNode>> = Vec::new();
        right_nodes.push(Box::new(ParseNode::PyName(4, 5, Box::new(Token::Name(4, 5, "b".to_string())))));

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyAssignment(0, 5, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(right_symbols), Box::new(right_nodes), None)))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_assign_multiple_no_type_comment_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a = b = c\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        let mut right_symbols : Vec<Box<Token>> = Vec::new();
        right_symbols.push(Box::new(Token::Assign(2, 3)));
        right_symbols.push(Box::new(Token::Assign(6, 7)));

        let mut right_nodes : Vec<Box<ParseNode>> = Vec::new();
        right_nodes.push(Box::new(ParseNode::PyName(4, 6, Box::new(Token::Name(4, 5, "b".to_string())))));
        right_nodes.push(Box::new(ParseNode::PyName(8, 9, Box::new(Token::Name(8, 9, "c".to_string())))));

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyAssignment(0, 9, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(right_symbols), Box::new(right_nodes), None)))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_assign_multiple_with_type_comment_statement()
    {
        let mut buffer = SourceBuffer::new();
        buffer.from_text("a = b = c # type: int\r\n\r\n");

        let mut parser = Parser::new(&mut buffer, 4);
        let res = parser.parse_expr_stmt();

        let mut right_symbols : Vec<Box<Token>> = Vec::new();
        right_symbols.push(Box::new(Token::Assign(2, 3)));
        right_symbols.push(Box::new(Token::Assign(6, 7)));

        let mut right_nodes : Vec<Box<ParseNode>> = Vec::new();
        right_nodes.push(Box::new(ParseNode::PyName(4, 6, Box::new(Token::Name(4, 5, "b".to_string())))));
        right_nodes.push(Box::new(ParseNode::PyName(8, 10, Box::new(Token::Name(8, 9, "c".to_string())))));

        match res {
            Ok(x) => {
                assert_eq!(x, Box::new(ParseNode::PyAssignment(0, 23, Box::new(ParseNode::PyName(0, 2, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(right_symbols), Box::new(right_nodes), Some(Box::new(Token::TypeComment(10, 23, "# type: int".to_string()))))))
            },
            _ => assert!(false)
        }
    }

    // #[test]
    // fn parse_plus_assign_statement()
    // {
    //     let mut buffer = SourceBuffer::new();
    //     buffer.from_text("a += b\r\n");
    //
    //     let mut parser = Parser::new(&mut buffer, 4);
    //     let res = parser.parse_stmt();
    //
    //     let mut nodes = Vec::<Box<ParseNode>>::new();
    //     nodes.push(Box::new(ParseNode::PyPlusAssign(0, 6, Box::new(ParseNode::PyName(0, 1, Box::new(Token::Name(0, 1, "a".to_string())))), Box::new(Token::Assign(2, 4)), Box::new(ParseNode::PyName(5, 6, Box::new(Token::Name(5, 6, "b".to_string())))))));
    //
    //     let mut separators = Vec::<Box<Token>>::new();
    //     let newline = Box::new(Token::Newline(11,13, '\r', '\n'));
    //
    //     match res {
    //         Ok(x) => {
    //             assert_eq!(x, Box::new(ParseNode::PySimpleStmt(0, 13, Box::new(nodes), Box::new(separators), newline)))
    //         },
    //         _ => assert!(false)
    //     }
    // }
}