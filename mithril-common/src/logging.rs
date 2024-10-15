//! Logging utilities for the Mithril project.

use slog::Logger;

/// Extension trait for `slog::Logger`
pub trait LoggerExtensions {
    /// Create a new child logger with a `src` key containing the component name.
    fn new_with_component_name<T>(&self) -> Self;

    /// Create a new child logger with a `src` key containing the provided name.
    fn new_with_name(&self, name: &str) -> Self;
}

impl LoggerExtensions for Logger {
    fn new_with_component_name<T>(&self) -> Self {
        self.new_with_name(component_name::<T>())
    }

    fn new_with_name(&self, name: &str) -> Self {
        self.new(slog::o!("src" => name.to_owned()))
    }
}

fn component_name<T>() -> &'static str {
    let complete_name = std::any::type_name::<T>();
    let without_generic = {
        if complete_name.contains('<') {
            complete_name.split('<').next().unwrap_or("")
        } else {
            complete_name
        }
    };
    without_generic.split("::").last().unwrap_or(complete_name)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::{TempDir, TestLogger};
    use slog::info;

    struct TestStruct;
    // The `allow(dead_code)` is used because a field is needed to add the lifetime but is unused.
    #[allow(dead_code)]
    struct TestStructWithLifetime<'a>(&'a str);
    enum TestEnum {}

    struct TestStructWithGeneric<T> {
        _phantom: std::marker::PhantomData<T>,
    }

    mod test_mod {
        pub struct ScopedTestStruct;
        pub enum ScopedTestEnum {}
    }

    impl TestStruct {
        fn self_component_name() -> &'static str {
            component_name::<Self>()
        }
    }

    #[test]
    fn extract_component_name_remove_namespaces() {
        assert_eq!(component_name::<TestStruct>(), "TestStruct");
        assert_eq!(component_name::<TestEnum>(), "TestEnum");
        assert_eq!(
            component_name::<test_mod::ScopedTestStruct>(),
            "ScopedTestStruct"
        );
        assert_eq!(
            component_name::<test_mod::ScopedTestEnum>(),
            "ScopedTestEnum"
        );
        assert_eq!(TestStruct::self_component_name(), "TestStruct");
        assert_eq!(
            component_name::<TestStructWithLifetime>(),
            "TestStructWithLifetime"
        );
        assert_eq!(
            component_name::<TestStructWithGeneric<test_mod::ScopedTestStruct>>(),
            "TestStructWithGeneric"
        );
        assert_eq!(
            component_name::<TestStructWithGeneric<&str>>(),
            "TestStructWithGeneric"
        );
    }

    #[test]
    fn logger_extension_new_with_component_name() {
        let log_path =
            TempDir::create("common_logging", "logger_extension_new_with_component_name")
                .join("test.log");
        {
            let root_logger = TestLogger::file(&log_path);
            let child_logger = root_logger.new_with_component_name::<TestStruct>();
            info!(child_logger, "Child log");
        }

        let logs = std::fs::read_to_string(&log_path).unwrap();
        assert!(
            logs.contains("src") && logs.contains("TestStruct"),
            "log should contain `src` key for `TestStruct` as component name was provided, logs:\n{logs}"
        );
    }

    #[test]
    fn logger_extension_new_with_name() {
        let expected_name = "my name";
        let log_path =
            TempDir::create("common_logging", "logger_extension_new_with_name").join("test.log");
        {
            let root_logger = TestLogger::file(&log_path);
            let child_logger = root_logger.new_with_name(expected_name);
            info!(child_logger, "Child log");
        }

        let logs = std::fs::read_to_string(&log_path).unwrap();
        assert!(
            logs.contains("src") && logs.contains(expected_name),
            "log should contain `src` key for `{expected_name}` as a name was provided, logs:\n{logs}"
        );
    }
}
