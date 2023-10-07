use crate::parser::abstract_syntax_tree_nodes::ParseNode;
use crate::parser::parser::Parser;
use crate::parser::syntax_error::SyntaxError;

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
    fn parse_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_simple_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
    }

    fn parse_small_stmt(&mut self) -> Result<Box<ParseNode>, SyntaxError> {
        todo!()
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