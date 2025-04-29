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
    use crate::test_utils::TestLogger;
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
        let (root_logger, log_inspector) = TestLogger::memory();
        let child_logger = root_logger.new_with_component_name::<TestStruct>();
        info!(child_logger, "Child log");

        assert!(
            log_inspector.contains_log("src") && log_inspector.contains_log("TestStruct"),
            "log should contain `src` key for `TestStruct` as component name was provided, logs:\n{log_inspector}"
        );
    }

    #[test]
    fn logger_extension_new_with_name() {
        let expected_name = "my name";
        let (root_logger, log_inspector) = TestLogger::memory();
        let child_logger = root_logger.new_with_name(expected_name);
        info!(child_logger, "Child log");

        assert!(
            log_inspector.contains_log("src") && log_inspector.contains_log(expected_name),
            "log should contain `src` key for `{expected_name}` as a name was provided, logs:\n{log_inspector}"
        );
    }
}
