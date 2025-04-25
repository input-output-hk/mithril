use std::fmt;
use std::io;
use std::sync::{Arc, RwLock};

use slog::{Drain, OwnedKVList, Record, KV};

/// A testing infrastructure for logging that consists of two main components:
/// - [MemoryDrainForTest]: A slog Drain that stores records in memory
/// - [MemoryDrainForTestInspector]: A component that provides methods to analyze stored logs
///
/// Records are stored as formatted strings in a thread-safe vector using `Arc<RwLock>`.
/// Each log record follows the format:
///
/// `{LEVEL} {MESSAGE}; {KEY1}={VALUE1}, {KEY2}={VALUE2}, ...`
///
/// where:
/// - LEVEL: The log level (DEBUG, INFO, etc.)
/// - MESSAGE: The main log message
/// - KEY=VALUE pairs: Additional context values attached to the log
///
/// # Performance Considerations
///
/// This drain implementation is designed for testing purposes and is not optimized for performance.
/// It uses an RwLock for each log operation and string formatting, which may introduce significant overhead.
#[derive(Default, Clone)]
pub struct MemoryDrainForTest {
    records: Arc<RwLock<Vec<String>>>,
}

impl MemoryDrainForTest {
    /// Creates a new instance of `MemoryDrainForTest`
    pub fn new() -> (Self, MemoryDrainForTestInspector) {
        let drain = Self::default();
        let inspector = MemoryDrainForTestInspector::new(&drain);
        (drain, inspector)
    }
}

/// A component that provides methods to analyze logs stored by [MemoryDrainForTest].
pub struct MemoryDrainForTestInspector {
    records: Arc<RwLock<Vec<String>>>,
}

impl MemoryDrainForTestInspector {
    fn new(memory_drain: &MemoryDrainForTest) -> Self {
        Self {
            records: memory_drain.records.clone(),
        }
    }

    /// Returns all log records that contain the specified text
    ///
    /// This method performs a case-sensitive search through all stored log records
    /// and returns copies of the matching records.
    pub fn search_logs(&self, text: &str) -> Vec<String> {
        self.records
            .read()
            .unwrap()
            .iter()
            .filter(|record| record.contains(text))
            .cloned()
            .collect()
    }

    /// Checks if any log record contains the specified text
    ///
    /// This method performs a case-sensitive search and returns true if any log
    /// record contains the specified text.
    pub fn contains_log(&self, text: &str) -> bool {
        self.records
            .read()
            .unwrap()
            .iter()
            .any(|record| record.contains(text))
    }
}

impl fmt::Display for MemoryDrainForTestInspector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.records.read().unwrap().join("\n"))
    }
}

impl Drain for MemoryDrainForTest {
    type Ok = ();
    type Err = io::Error;

    fn log(&self, record: &Record, values: &OwnedKVList) -> Result<Self::Ok, Self::Err> {
        let mut kv_serializer = KVSerializer::default();
        record.kv().serialize(record, &mut kv_serializer)?;
        values.serialize(record, &mut kv_serializer)?;

        let msg = format!(
            "{} {}; {}",
            record.level().as_str(),
            record.msg(),
            kv_serializer.content
        );

        self.records.write().unwrap().push(msg);
        Ok(())
    }
}

#[derive(Default)]
struct KVSerializer {
    content: String,
}

impl slog::Serializer for KVSerializer {
    fn emit_arguments(&mut self, key: slog::Key, val: &std::fmt::Arguments) -> slog::Result {
        use std::fmt::Write;

        let prefix = if self.content.is_empty() { "" } else { ", " };
        write!(self.content, "{prefix}{key}={val:?}")
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "Failed to serialize log"))?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use slog::debug;

    use super::*;

    #[test]
    fn test_log_format() {
        let (drain, log_inspector) = MemoryDrainForTest::new();
        let logger = slog::Logger::root(drain.clone().fuse(), slog::o!("shared" => "shared"));

        // Note: keys seem to be logged in invert order
        debug!(logger, "test format"; "key_3" => "value three", "key_2" => "value two", "key_1" => "value one");

        let results = log_inspector.search_logs("test format");
        assert_eq!(
            "DEBUG test format; key_1=value one, key_2=value two, key_3=value three, shared=shared",
            results[0]
        );
    }

    #[test]
    fn displaying_inspector_returns_all_log_messages() {
        let (drain, log_inspector) = MemoryDrainForTest::new();
        let logger = slog::Logger::root(drain.fuse(), slog::o!());

        debug!(logger, "message one"; "key" => "value1");
        debug!(logger, "message two"; "key" => "value2");

        let display = format!("{log_inspector}");
        assert_eq!(
            display,
            "DEBUG message one; key=value1\nDEBUG message two; key=value2"
        );
    }

    #[test]
    fn can_search_for_log_messages() {
        let (drain, log_inspector) = MemoryDrainForTest::new();
        let logger = slog::Logger::root(drain.clone().fuse(), slog::o!());

        debug!(logger, "test message"; "key" => "value");
        debug!(logger, "another message"; "key2" => "value2");

        let results = log_inspector.search_logs("test");
        assert_eq!(results.len(), 1);
        assert!(results[0].contains("test message"));
        assert!(log_inspector.contains_log("test message"));
    }

    #[tokio::test]
    async fn test_concurrent_logging_from_two_tasks() {
        let (drain, log_inspector) = MemoryDrainForTest::new();
        let drain_clone1 = drain.clone();
        let drain_clone2 = drain.clone();

        let handle1 = tokio::spawn(async move {
            let logger = slog::Logger::root(drain_clone1.fuse(), slog::o!());
            debug!(logger, "async test 1"; "key" => "value");
        });
        let handle2 = tokio::spawn(async move {
            let logger = slog::Logger::root(drain_clone2.fuse(), slog::o!());
            debug!(logger, "async test 2"; "key" => "value");
        });

        handle1.await.unwrap();
        handle2.await.unwrap();

        let results = log_inspector.search_logs("async test");
        assert_eq!(results.len(), 2);
        assert!(results.iter().any(|r| r.contains("async test 1")));
        assert!(results.iter().any(|r| r.contains("async test 2")));
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn test_concurrent_logging_from_multiple_threads() {
        let (drain, log_inspector) = MemoryDrainForTest::new();
        let mut join_set = tokio::task::JoinSet::new();

        for i in 0..10 {
            let drain_clone = drain.clone();
            join_set.spawn(async move {
                let logger = slog::Logger::root(drain_clone.fuse(), slog::o!());
                debug!(logger, "multi thread test {i}"; "thread_id" => i);
            });
        }

        join_set.join_all().await;

        let results = log_inspector.search_logs("multi thread test");
        assert_eq!(results.len(), 10);
        for i in 0..10 {
            assert!(results
                .iter()
                .any(|r| r.contains(&format!("multi thread test {i}"))));
        }
    }
}
