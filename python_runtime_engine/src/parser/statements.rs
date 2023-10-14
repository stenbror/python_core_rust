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
    fn parse_annotate_assignment(&mut self) -> Result<Box<ParseNode>, SyntaxError>;
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
        let _pattern = "match".to_string();
        match self.get_symbol() {
            Token::Name( _ , _ , text ) => {
                match text {
                    _pattern => self.parse_compound_stmt(), // Positional keyword 'match'
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

    fn parse_expr_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_annotate_assignment(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
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

    fn parse_yield_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_raise_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_import_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_import_name_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_import_from_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_import_as_name_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_dotted_name_as_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_import_as_names_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_dotted_as_names_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_dotted_name_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
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

}