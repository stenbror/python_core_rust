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

    fn parse_del_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_pass_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_flow_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_break_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_continue_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_return_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
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

    fn parse_global_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_nonlocal_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_assert_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_compound_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }
}


// Unittests for statement parser rules ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {

    #[test]
    fn parse_dummy_statement()
    {
        assert_eq!(true, true)
    }

}