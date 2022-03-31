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
|Curve: Bls12_381|
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
STM/"Bls12_381"/Key registration/32                                                                            
                        time:   [295.76 ms 367.82 ms 295.76 ms]
STM/"Bls12_381"/Key registration/64                                                                            
                        time:   [565.63 ms 573.72 ms 589.60 ms]
STM/"Bls12_381"/Key registration/128                                                                            
                        time:   [1.1800 s 1.1922 s 1.2124 s]
STM/"Bls12_381"/Key registration/256                                                                            
                        time:   [2.4396 s 2.5564 s 2.7018 s]
STM/"Bls12_381"/Key registration/512                                                                            
                        time:   [4.2276 s 4.3262 s 4.4616 s]
STM/"Bls12_381"/Key registration/1024                                                                            
                        time:   [8.0138 s 8.2035 s 8.3750 s]
STM/"Bls12_381"/Key registration/2048                                                                            
                        time:   [17.938 s 18.145 s 18.448 s]
STM/"Bls12_381"/Key registration/4096                                                                            
                        time:   [32.553 s 32.997 s 33.457 s]
STM/"Bls12_381"/Play all lotteries/k: 8, m: 50                                                                            
                        time:   [40.488 ms 41.386 ms 42.959 ms]
STM/"Bls12_381"/Play all lotteries/k: 8, m: 100                                                                            
                        time:   [70.278 ms 72.137 ms 75.313 ms]
STM/"Bls12_381"/Play all lotteries/k: 8, m: 150                                                                            
                        time:   [95.440 ms 101.44 ms 102.21 ms]
STM/"Bls12_381"/Play all lotteries/k: 8, m: 200                                                                            
                        time:   [130.12 ms 134.24 ms 137.99 ms]
STM/"Bls12_381"/Play all lotteries/k: 8, m: 250                                                                            
                        time:   [175.21 ms 175.15 ms 176.48 ms]
STM/"Bls12_381"/Play all lotteries/k: 8, m: 300                                                                            
                        time:   [173.27 ms 176.25 ms 178.82 ms]
STM/"Bls12_381"/Play all lotteries/k: 8, m: 350                                                                            
                        time:   [211.78 ms 213.77 ms 215.16 ms]
STM/"Bls12_381"/Play all lotteries/k: 8, m: 400                                                                            
                        time:   [231.01 ms 239.26 ms 244.34 ms]
STM/"Bls12_381"/Aggregation/k: 8, m: 50                                                                            
                        time:   [28.582 ms 28.715 ms 28.778 ms]
STM/"Bls12_381"/Aggregation/k: 16, m: 50                                                                            
                        time:   [56.970 ms 57.116 ms 57.718 ms]
STM/"Bls12_381"/Aggregation/k: 32, m: 50                                                                            
                        time:   [111.46 ms 111.74 ms 111.92 ms]
```
