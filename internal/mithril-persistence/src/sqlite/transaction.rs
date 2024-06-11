use crate::sqlite::SqliteConnection;

/// Sqlite transaction wrapper.
///
/// Transactions are automatically rolled back if this struct object is dropped and
/// the transaction was not committed.
pub struct Transaction<'a> {
    connection: &'a SqliteConnection,
    // An active transaction is one that has yet to be committed or rolled back.
    is_active: bool,
}

impl<'a> Transaction<'a> {
    /// Begin a new transaction.
    pub fn begin(connection: &'a SqliteConnection) -> Result<Self, sqlite::Error> {
        connection.execute("BEGIN TRANSACTION")?;
        Ok(Self {
            connection,
            is_active: true,
        })
    }

    /// Commit the transaction.
    pub fn commit(mut self) -> Result<(), sqlite::Error> {
        self.is_active = false;
        self.connection.execute("COMMIT TRANSACTION")?;
        Ok(())
    }

    /// Rollback the transaction.
    pub fn rollback(mut self) -> Result<(), sqlite::Error> {
        self.is_active = false;
        self.connection.execute("ROLLBACK TRANSACTION")?;
        Ok(())
    }
}

impl Drop for Transaction<'_> {
    fn drop(&mut self) {
        if self.is_active {
            // Unwrap should not happen here, otherwise it would mean that we have not handled
            // correctly the transaction "active" state or that the connection was closed.
            self.connection.execute("ROLLBACK TRANSACTION").unwrap();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sqlite::ConnectionExtensions;
    use anyhow::anyhow;
    use mithril_common::StdResult;
    use sqlite::Connection;

    fn init_database() -> SqliteConnection {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        connection
            .execute("create table query_test(text_data text not null primary key);")
            .unwrap();

        connection
    }

    fn get_number_of_rows(connection: &SqliteConnection) -> i64 {
        connection
            .query_single_cell("select count(*) from query_test", &[])
            .unwrap()
    }

    #[test]
    fn test_commit() {
        let connection = init_database();

        assert_eq!(0, get_number_of_rows(&connection));
        {
            let transaction = Transaction::begin(&connection).unwrap();
            connection
                .execute("insert into query_test(text_data) values ('row 1')")
                .unwrap();
            transaction.commit().unwrap();
        }
        assert_eq!(1, get_number_of_rows(&connection));
    }

    #[test]
    fn test_rollback() {
        let connection = init_database();

        assert_eq!(0, get_number_of_rows(&connection));
        {
            let transaction = Transaction::begin(&connection).unwrap();
            connection
                .execute("insert into query_test(text_data) values ('row 1')")
                .unwrap();
            transaction.rollback().unwrap();
        }
        assert_eq!(0, get_number_of_rows(&connection));
    }

    #[test]
    fn test_auto_rollback_when_dropping() {
        let connection = init_database();

        assert_eq!(0, get_number_of_rows(&connection));
        {
            let _transaction = Transaction::begin(&connection).unwrap();
            connection
                .execute("insert into query_test(text_data) values ('row 1')")
                .unwrap();
        }
        assert_eq!(0, get_number_of_rows(&connection));
    }

    #[test]
    fn test_auto_rollback_when_dropping_because_of_an_error() {
        fn failing_function() -> StdResult<()> {
            Err(anyhow!("This is an error"))
        }
        fn failing_function_that_insert_data(connection: &SqliteConnection) -> StdResult<()> {
            let transaction = Transaction::begin(connection).unwrap();
            connection
                .execute("insert into query_test(text_data) values ('row 1')")
                .unwrap();
            failing_function()?;
            transaction.commit().unwrap();
            Ok(())
        }

        let connection = init_database();

        assert_eq!(0, get_number_of_rows(&connection));
        let _err = failing_function_that_insert_data(&connection).unwrap_err();
        assert_eq!(0, get_number_of_rows(&connection));
    }

    #[test]
    fn test_drop_dont_panic_if_previous_commit_failed() {
        let connection = init_database();

        {
            let transaction = Transaction::begin(&connection).unwrap();
            connection
                .execute("insert into query_test(text_data) values ('row 1')")
                .unwrap();

            // Commiting make the transaction inactive thus make next operation fail
            connection.execute("COMMIT TRANSACTION").unwrap();
            transaction.commit().expect_err("Commit should have fail");

            // When going out of scope, drop is called and should not panic
        }
    }

    #[test]
    fn test_drop_dont_panic_if_previous_rollback_failed() {
        let connection = init_database();

        {
            let transaction = Transaction::begin(&connection).unwrap();
            connection
                .execute("insert into query_test(text_data) values ('row 1')")
                .unwrap();

            // Commiting make the transaction inactive thus make next operation fail
            connection.execute("COMMIT TRANSACTION").unwrap();
            transaction
                .rollback()
                .expect_err("Rollback should have fail");

            // When going out of scope, drop is called and should not panic
        }
    }
}
