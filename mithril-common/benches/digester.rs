use criterion::{criterion_group, criterion_main, Criterion};
use mithril_common::{
    digesters::cache::{ImmutableFileDigestCacheProvider, JsonImmutableFileDigestCacheProvider},
    digesters::{
        cache::MemoryImmutableFileDigestCacheProvider, CardanoImmutableDigester, ImmutableDigester,
    },
    entities::{Beacon, ImmutableFileNumber},
};
use slog::Drain;
use std::{
    fs,
    fs::File,
    io::prelude::Write,
    path::{Path, PathBuf},
    sync::Arc,
};

fn temp_dir() -> PathBuf {
    std::env::temp_dir()
        .join("mithril_benches")
        .join("digester")
}

fn db_dir() -> PathBuf {
    temp_dir().join("db").join("immutable")
}

fn create_db(dir: &Path, number_of_immutables: ImmutableFileNumber, file_size: u64) {
    if dir.exists() {
        fs::remove_dir_all(dir).unwrap_or_else(|e| panic!("Could not remove dir {dir:?}: {e}"));
    }
    fs::create_dir_all(dir).unwrap_or_else(|e| panic!("Could not create dir {dir:?}: {e}"));

    // + 1 to simulate "in-progress" immutable trio.
    for filename in (1..=(number_of_immutables + 1)).flat_map(|i| {
        [
            format!("{i:05}.chunk"),
            format!("{i:05}.primary"),
            format!("{i:05}.secondary"),
        ]
    }) {
        let file = dir.join(Path::new(&filename));
        let mut source_file = File::create(file).unwrap();

        write!(source_file, "This is a test file named '{filename}'").unwrap();
        writeln!(source_file).unwrap();
        source_file.set_len(file_size).unwrap();
    }
}

#[inline]
fn create_logger() -> slog::Logger {
    let drain = slog_async::Async::new(slog::Discard).build().fuse();
    slog::Logger::root(Arc::new(drain), slog::o!())
}

#[inline]
async fn compute_digest(
    cache_provider: Option<Arc<dyn ImmutableFileDigestCacheProvider>>,
    number_of_immutables: ImmutableFileNumber,
) {
    let digester = CardanoImmutableDigester::new(cache_provider, create_logger());
    digester
        .compute_digest(
            &db_dir(),
            &Beacon::new("devnet".to_string(), 1, number_of_immutables),
        )
        .await
        .expect("digest computation should not fail");
}

fn criterion_benchmark(c: &mut Criterion) {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let number_of_immutable = 25;
    create_db(&db_dir(), number_of_immutable, 65536 * 4);

    c.bench_function("digester no cache", |bencher| {
        bencher
            .to_async(&runtime)
            .iter(|| async { compute_digest(None, number_of_immutable).await })
    });
    c.bench_function("digester memory cache", |bencher| {
        let cache = Arc::new(MemoryImmutableFileDigestCacheProvider::default());

        bencher
            .to_async(&runtime)
            .iter(|| async { compute_digest(Some(cache.clone()), number_of_immutable).await })
    });
    c.bench_function("digester json cache", |bencher| {
        let cache = Arc::new(JsonImmutableFileDigestCacheProvider::new(
            &temp_dir().join("immutable-cache-store.json"),
        ));

        bencher
            .to_async(&runtime)
            .iter(|| async { compute_digest(Some(cache.clone()), number_of_immutable).await })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
