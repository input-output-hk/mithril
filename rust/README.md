Mithril
=======

Paper: https://eprint.iacr.org/2021/916.pdf

Running Mithril using the Dockerfile
------------------------------------

First, to build the Docker image:
```
docker build . -t mithril
```

Next, to build and run the tests:
```
docker run -it mithril /bin/bash -c "cd /mithril; /root/.cargo/bin/cargo test --release"
```

To build and run the benchmarks:
```
docker run -it mithril /bin/bash -c "cd /mithril; /root/.cargo/bin/cargo bench"
```
