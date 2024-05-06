use sqlite::Value;
use std::iter::repeat;

/// Internal Boolean representation
enum BooleanCondition {
    /// Empty tree
    None,

    /// Single boolean expression
    Expression(String),

    /// And branch
    And(Box<BooleanCondition>, Box<BooleanCondition>),

    /// Or branch
    Or(Box<BooleanCondition>, Box<BooleanCondition>),
}

impl BooleanCondition {
    /// Turn a boolean expression to its string SQL representation.
    pub fn expand(&self) -> String {
        match self {
            Self::None => "true".to_string(),
            Self::Expression(expr) => expr.to_owned(),
            Self::And(lft, rgt) => match (lft.needs_precedence(), rgt.needs_precedence()) {
                (true, false) => format!("({}) and {}", lft.expand(), rgt.expand()),
                (false, true) => format!("{} and ({})", lft.expand(), rgt.expand()),
                (true, true) => format!("({}) and ({})", lft.expand(), rgt.expand()),
                (false, false) => format!("{} and {}", lft.expand(), rgt.expand()),
            },
            Self::Or(lft, rgt) => format!("{} or {}", lft.expand(), rgt.expand()),
        }
    }

    fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }

    fn needs_precedence(&self) -> bool {
        matches!(self, Self::Or(_, _))
    }
}

/// Where condition builder.
pub struct WhereCondition {
    /// Boolean condition internal tree
    condition: BooleanCondition,

    /// Parameters associated to the conditions
    parameters: Vec<Value>,
}

impl Default for WhereCondition {
    fn default() -> Self {
        Self {
            condition: BooleanCondition::None,
            parameters: Vec::new(),
        }
    }
}

impl WhereCondition {
    /// Instanciate a new condition from an expression.
    pub fn new(expression: &str, parameters: Vec<Value>) -> Self {
        Self {
            condition: BooleanCondition::Expression(expression.to_string()),
            parameters,
        }
    }

    /// Turn the condition into a SQL string representation.
    pub fn expand(self) -> (String, Vec<Value>) {
        let expression = self.condition.expand();
        let parameters = self.parameters;
        //
        // Replace parameters placeholders by numerated parameters.
        let mut final_expression = "".to_string();
        for (param_index, sql_part) in expression.split("?*").enumerate() {
            if param_index > 0 {
                final_expression.push('?');
                final_expression.push_str(&param_index.to_string());
            }
            final_expression.push_str(sql_part);
        }

        (final_expression, parameters)
    }

    /// Instanciate a condition with a `IN` statement.
    pub fn where_in(field: &str, parameters: Vec<Value>) -> Self {
        let params: Vec<&str> = repeat("?*").take(parameters.len()).collect();
        let expression = format!("{} in ({})", field, params.join(", "));

        Self {
            condition: BooleanCondition::Expression(expression),
            parameters,
        }
    }

    /// Add a new parameter using a AND operator
    pub fn and_where(mut self, mut condition: WhereCondition) -> Self {
        if condition.condition.is_none() {
            return self;
        }

        if self.condition.is_none() {
            self.condition = condition.condition;
            self.parameters = condition.parameters;
        } else {
            let temp = BooleanCondition::None;
            let my_condition = std::mem::replace(&mut self.condition, temp);
            self.condition =
                BooleanCondition::And(Box::new(my_condition), Box::new(condition.condition));
            self.parameters.append(&mut condition.parameters);
        }

        self
    }

    /// Add a new condition with a OR operator
    pub fn or_where(mut self, mut condition: WhereCondition) -> Self {
        if condition.condition.is_none() {
            return self;
        }
        if self.condition.is_none() {
            self.condition = condition.condition;
            self.parameters = condition.parameters;
        } else {
            let temp = BooleanCondition::None;
            let my_condition = std::mem::replace(&mut self.condition, temp);
            self.condition =
                BooleanCondition::Or(Box::new(my_condition), Box::new(condition.condition));
            self.parameters.append(&mut condition.parameters);
        }

        self
    }
}

/// Get all condition builder.
pub trait GetAllCondition {
    /// Get the condition for a get all query.
    fn get_all_condition() -> WhereCondition {
        WhereCondition::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn boolean_expand_none() {
        let condition = BooleanCondition::None;

        assert_eq!("true".to_string(), condition.expand());
    }

    #[test]
    fn boolean_expand_expression() {
        let condition = BooleanCondition::Expression("something".to_string());

        assert_eq!("something".to_string(), condition.expand());
    }

    #[test]
    fn boolean_expand_and() {
        let left = BooleanCondition::Expression("left".to_string());
        let right = BooleanCondition::Expression("right".to_string());
        let condition = BooleanCondition::And(Box::new(left), Box::new(right));

        assert_eq!("left and right".to_string(), condition.expand());
    }

    #[test]
    fn boolean_expand_or() {
        let left = BooleanCondition::Expression("left".to_string());
        let right = BooleanCondition::Expression("right".to_string());
        let condition = BooleanCondition::Or(Box::new(left), Box::new(right));

        assert_eq!("left or right".to_string(), condition.expand());
    }

    #[test]
    fn expression_default() {
        let expression = WhereCondition::default();
        let (sql, params) = expression.expand();

        assert_eq!("true".to_string(), sql);
        assert!(params.is_empty());
    }

    #[test]
    fn expression_sql() {
        let expression = WhereCondition::new("something is not null", Vec::new());
        let (sql, params) = expression.expand();

        assert_eq!("something is not null".to_string(), sql);
        assert!(params.is_empty());
    }

    #[test]
    fn expression_and() {
        let expression =
            WhereCondition::new("A", Vec::new()).and_where(WhereCondition::new("B", Vec::new()));
        let (sql, params) = expression.expand();

        assert_eq!("A and B", &sql);
        assert!(params.is_empty());
    }

    #[test]
    fn expression_and_none() {
        let expression = WhereCondition::new("A", Vec::new()).and_where(WhereCondition::default());
        let (sql, params) = expression.expand();

        assert_eq!("A", &sql);
        assert!(params.is_empty());
    }

    #[test]
    fn expression_none_and() {
        let expression = WhereCondition::default().and_where(WhereCondition::new("A", Vec::new()));
        let (sql, params) = expression.expand();

        assert_eq!("A", &sql);
        assert!(params.is_empty());
    }

    #[test]
    fn expression_or() {
        let expression =
            WhereCondition::new("A", Vec::new()).or_where(WhereCondition::new("B", Vec::new()));
        let (sql, params) = expression.expand();

        assert_eq!("A or B", &sql);
        assert!(params.is_empty());
    }

    #[test]
    fn expression_or_none() {
        let expression = WhereCondition::new("A", Vec::new()).or_where(WhereCondition::default());
        let (sql, params) = expression.expand();

        assert_eq!("A", &sql);
        assert!(params.is_empty());
    }

    #[test]
    fn expression_none_or() {
        let expression = WhereCondition::default().or_where(WhereCondition::new("A", Vec::new()));
        let (sql, params) = expression.expand();

        assert_eq!("A", &sql);
        assert!(params.is_empty());
    }

    #[test]
    fn expression_complex_no_precedence() {
        let expression = WhereCondition::new("A", Vec::new())
            .and_where(WhereCondition::new("B", Vec::new()))
            .or_where(WhereCondition::new("C", Vec::new()));
        let (sql, params) = expression.expand();

        assert_eq!("A and B or C", &sql);
        assert!(params.is_empty());
    }

    #[test]
    fn expression_complex_with_precedence() {
        let sub_expression =
            WhereCondition::new("A", Vec::new()).or_where(WhereCondition::new("B", Vec::new()));
        let expression = WhereCondition::new("C", Vec::new()).and_where(sub_expression);
        let (sql, params) = expression.expand();

        assert_eq!("C and (A or B)", &sql);
        assert!(params.is_empty());
    }

    #[test]
    fn expression_complex_with_self_precedence() {
        let sub_expression = WhereCondition::new("C", Vec::new());
        let expression = WhereCondition::new("A", Vec::new())
            .or_where(WhereCondition::new("B", Vec::new()))
            .and_where(sub_expression);
        let (sql, params) = expression.expand();

        assert_eq!("(A or B) and C", &sql);
        assert!(params.is_empty());
    }

    #[test]
    fn expression_complex_with_both_precedence() {
        let sub_expression =
            WhereCondition::new("C", Vec::new()).or_where(WhereCondition::new("D", Vec::new()));
        let expression = WhereCondition::new("A", Vec::new())
            .or_where(WhereCondition::new("B", Vec::new()))
            .and_where(sub_expression);
        let (sql, params) = expression.expand();

        assert_eq!("(A or B) and (C or D)", &sql);
        assert!(params.is_empty());
    }

    #[test]
    fn expression_sql_with_parameter() {
        let expression = WhereCondition::new("A > ?*::pg_type", vec![Value::Integer(0)]);
        let (sql, params) = expression.expand();

        assert_eq!("A > ?1::pg_type", &sql);
        assert_eq!(1, params.len());
    }

    #[test]
    fn expression_sql_with_multiple_parameters() {
        let expression = WhereCondition::new("A > ?*", vec![Value::Integer(0)])
            .and_where(WhereCondition::new("B = ?*", vec![Value::Integer(1)]));
        let (sql, params) = expression.expand();

        assert_eq!("A > ?1 and B = ?2", &sql);
        assert_eq!(2, params.len());
    }

    #[test]
    fn expression_where_in() {
        let expression = WhereCondition::where_in("A", vec![Value::Integer(0), Value::Integer(1)]);
        let (sql, params) = expression.expand();

        assert_eq!("A in (?1, ?2)".to_string(), sql);
        assert_eq!(2, params.len());
    }

    #[test]
    fn expression_sql_with_multiple_parameters_and_where_in() {
        let expression = WhereCondition::new("A > ?*", vec![Value::Integer(0)])
            .or_where(WhereCondition::new("B", Vec::new()))
            .and_where(WhereCondition::where_in(
                "C",
                vec![
                    Value::Integer(100),
                    Value::Integer(101),
                    Value::Integer(102),
                ],
            ));

        let (sql, params) = expression.expand();

        assert_eq!("(A > ?1 or B) and C in (?2, ?3, ?4)", &sql);
        assert_eq!(4, params.len());
    }

    #[test]
    fn parameters_tosql() {
        let expression = WhereCondition::new("a = ?*", vec![Value::String("whatever".into())]);
        let (sql, params) = expression.expand();

        assert_eq!("a = ?1", &sql);
        assert_eq!(1, params.len());
    }

    #[test]
    fn expression_get_all_default() {
        impl GetAllCondition for String {}

        let expression = String::get_all_condition();
        let (sql, params) = expression.expand();

        assert_eq!("true".to_string(), sql);
        assert!(params.is_empty());
    }
}
