use std::iter::repeat_n;
use std::ops::Range;

use sqlite::Value;

/// Represents data for INSERT/UPDATE operations
pub trait MutationPayload {
    /// Number of columns of each row.
    fn number_of_columns(&self) -> usize;

    /// Number of rows to insert or update.
    fn number_of_rows(&self) -> usize;

    /// Number of values to bind for this mutation.
    fn binding_size(&self) -> usize {
        self.number_of_columns() * self.number_of_rows()
    }

    /// Column names for the mutation.
    fn columns(&self) -> &[&str];

    /// Comma separated list of column names for the mutation
    fn joined_columns(&self) -> String {
        self.columns().join(", ")
    }

    /// Generate the binding columns for the mutation to use in the SQL statement.
    ///
    /// i.e. if the mutation has 3 columns and two rows, the returned string will be
    /// ```
    /// (?, ?, ?), (?, ?, ?)
    /// ```
    fn binding_columns(&self) -> String {
        let rows_binding = format!(
            "({})",
            repeat_n("?", self.number_of_columns())
                .collect::<Vec<&str>>()
                .join(", ")
        );

        repeat_n(rows_binding, self.number_of_rows())
            .collect::<Vec<String>>()
            .join(", ")
    }

    /// Rows of values to insert or update.
    fn rows(&self) -> &[Vec<Value>];

    fn bindable_values(&self, row_indexes: Range<usize>) -> Vec<Value>;
}

/// Represents data for INSERT/UPDATE operations with compile-time column consistency.
pub struct FixedColumnPayload<const N: usize> {
    /// Column names for the mutation.
    columns: [&'static str; N],
    /// Rows of values
    rows: Vec<Vec<Value>>,
}

impl<const N: usize> FixedColumnPayload<N> {
    /// Create a new [MutationPayload] with the given columns.
    pub fn new(columns: [&'static str; N]) -> Self {
        Self {
            columns,
            rows: Vec::new(),
        }
    }

    /// Set the rows of the mutation, overwriting any previous value.
    pub fn with_rows(mut self, rows: Vec<[Value; N]>) -> Self {
        self.rows = rows.into_iter().map(|row| row.to_vec()).collect();
        self
    }

    /// Add a new row to the mutation.
    pub fn add_row(&mut self, values: [Value; N]) {
        self.rows.push(values.to_vec());
    }
}

impl<const N: usize> MutationPayload for FixedColumnPayload<N> {
    fn number_of_columns(&self) -> usize {
        N
    }

    fn number_of_rows(&self) -> usize {
        self.rows.len()
    }

    fn columns(&self) -> &[&str] {
        &self.columns
    }

    fn rows(&self) -> &[Vec<Value>] {
        &self.rows
    }

    fn bindable_values(&self, row_indexes: Range<usize>) -> Vec<Value> {
        self.rows[row_indexes].iter().flatten().cloned().collect()
    }
}

// impl<T: MutationPayload> BindableWithIndex for T {
//     // Todo check update with a where clause to ensure that binding is correct
//     fn bind<I: ParameterIndex>(self, statement: &mut Statement, index: I) -> sqlite::Result<()> {
//         let start_index = index.index(statement)?;
//
//         statement.bind_iter(self.rows().enumerate().map(|(i, v)| (start_index + i, v)))
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fixed_payload() {
        let mut payload = FixedColumnPayload::new(["col1", "col2"]);
        payload.add_row([Value::Integer(1), Value::Integer(2)]);
        payload.add_row([Value::Integer(3), Value::Integer(4)]);

        assert_eq!(payload.number_of_columns(), 2);
        assert_eq!(payload.number_of_rows(), 2);
        assert_eq!(payload.columns(), &["col1", "col2"]);
        assert_eq!(
            payload.rows(),
            &[
                vec![Value::Integer(1), Value::Integer(2)],
                vec![Value::Integer(3), Value::Integer(4)]
            ]
        );
    }

    #[test]
    fn compute_binding_size() {
        let mut payload = FixedColumnPayload::new(["col1", "col2"]);
        assert_eq!(payload.binding_size(), 0);

        payload.add_row([Value::Integer(1), Value::Integer(2)]);
        payload.add_row([Value::Integer(3), Value::Integer(4)]);
        assert_eq!(payload.binding_size(), 4);
    }

    #[test]
    fn join_column_names() {
        let payload = FixedColumnPayload::new(["col1", "col2", "col3"]);
        assert_eq!(payload.joined_columns(), "col1, col2, col3");
    }

    #[test]
    fn generate_binding_columns() {
        // 1 column
        {
            let mut payload = FixedColumnPayload::new(["col1"]);
            assert_eq!(payload.binding_columns(), "");

            payload.add_row([Value::Integer(1)]);
            assert_eq!(payload.binding_columns(), "(?)");

            payload.add_row([Value::Integer(2)]);
            assert_eq!(payload.binding_columns(), "(?), (?)");
        }
        // 2 columns
        {
            let mut payload = FixedColumnPayload::new(["col1", "col2"]);
            assert_eq!(payload.binding_columns(), "");

            payload.add_row([Value::Integer(1), Value::Integer(2)]);
            assert_eq!(payload.binding_columns(), "(?, ?)");

            payload.add_row([Value::Integer(3), Value::Integer(4)]);
            assert_eq!(payload.binding_columns(), "(?, ?), (?, ?)");
        }
    }
}
