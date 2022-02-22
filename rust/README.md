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

We have run the benchmarks on a 2,7 GHz Quad-Core Intel Core i7 machine with 16 GB of RAM, on macOS 12.1. 

```shell
+----------------+
| Size of proofs |
+----------------+
|----------------|
| Trivial proofs |
+----------------+
k = 8; 6968 bytes
k = 16; 13496 bytes
k = 32; 26552 bytes
k = 64; 52664 bytes
k = 128; 104888 bytes
k = 256; 209336 bytes
k = 512; 418232 bytes
```

```shell

```
