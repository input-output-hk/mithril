## Benchmark Results - Batch Compatible 

```shell
+-------------------+
|   Size of proofs  |
+-------------------+
|-------------------|
|   Trivial proofs  |
+-------------------+
| Results obtained by using the parameters suggested in paper.
+-------------------+
+-------------------+
| Hash: Blake2b 512 |
+-------------------+
k = 445 | m = 2728 | nr parties = 3000; 118760 bytes
+-------------------+
| Hash: Blake2b 256 |
+-------------------+
k = 445 | m = 2728 | nr parties = 3000; 99384 bytes
+-------------------+
| Hash: Blake2b 512 |
+-------------------+
k = 554 | m = 3597 | nr parties = 3000; 133936 bytes
+-------------------+
| Hash: Blake2b 256 |
+-------------------+
k = 554 | m = 3597 | nr parties = 3000; 113728 bytes
```

```shell
STM/Blake2b/Key registration/k: 25, m: 150, nr_parties: 300
                        time:   [388.95 ms 389.43 ms 389.90 ms]
Found 1 outliers among 100 measurements (1.00%)
  1 (1.00%) high mild
STM/Blake2b/Play all lotteries/k: 25, m: 150, nr_parties: 300
                        time:   [699.76 µs 701.42 µs 703.63 µs]
Found 6 outliers among 100 measurements (6.00%)
  2 (2.00%) high mild
  4 (4.00%) high severe
STM/Blake2b/Aggregation/k: 25, m: 150, nr_parties: 300
                        time:   [18.888 ms 18.903 ms 18.920 ms]
Found 9 outliers among 100 measurements (9.00%)
  3 (3.00%) high mild
  6 (6.00%) high severe
STM/Blake2b/Verification/k: 25, m: 150, nr_parties: 300
                        time:   [2.1547 ms 2.1609 ms 2.1686 ms]
Found 8 outliers among 100 measurements (8.00%)
  1 (1.00%) low mild
  1 (1.00%) high mild
  6 (6.00%) high severe

STM/Blake2b/Key registration/k: 250, m: 1523, nr_parties: 2000
                        time:   [2.5986 s 2.6042 s 2.6101 s]
Found 8 outliers among 100 measurements (8.00%)
  8 (8.00%) high mild
STM/Blake2b/Play all lotteries/k: 250, m: 1523, nr_parties: 2000
                        time:   [5.9141 ms 5.9346 ms 5.9641 ms]
Found 11 outliers among 100 measurements (11.00%)
  9 (9.00%) high mild
  2 (2.00%) high severe
STM/Blake2b/Aggregation/k: 250, m: 1523, nr_parties: 2000
                        time:   [189.63 ms 190.10 ms 190.69 ms]
Found 14 outliers among 100 measurements (14.00%)
  5 (5.00%) high mild
  9 (9.00%) high severe
STM/Blake2b/Verification/k: 250, m: 1523, nr_parties: 2000
                        time:   [14.040 ms 14.143 ms 14.271 ms]
Found 4 outliers among 100 measurements (4.00%)
  1 (1.00%) high mild
  3 (3.00%) high severe
```


## Benchmark Results - Single Signature with Path

```shell
+-------------------+
|   Size of proofs  |
+-------------------+
|-------------------|
|   Trivial proofs  |
+-------------------+
| Results obtained by using the parameters suggested in paper.
+-------------------+
+-------------------+
| Hash: Blake2b 512 |
+-------------------+
k = 445 | m = 2728 | nr parties = 3000; 356632 bytes
+-------------------+
| Hash: Blake2b 256 |
+-------------------+
k = 445 | m = 2728 | nr parties = 3000; 222536 bytes
+-------------------+
| Hash: Blake2b 512 |
+-------------------+
k = 554 | m = 3597 | nr parties = 3000; 419808 bytes
+-------------------+
| Hash: Blake2b 256 |
+-------------------+
k = 554 | m = 3597 | nr parties = 3000; 261488 bytes
```

```shell
STM/Blake2b/Key registration/k: 25, m: 150, nr_parties: 300
                        time:   [385.81 ms 386.20 ms 386.67 ms]
Found 6 outliers among 100 measurements (6.00%)
  3 (3.00%) high mild
  3 (3.00%) high severe
STM/Blake2b/Play all lotteries/k: 25, m: 150, nr_parties: 300
                        time:   [696.75 µs 697.61 µs 699.18 µs]
Found 5 outliers among 100 measurements (5.00%)
  2 (2.00%) high mild
  3 (3.00%) high severe
STM/Blake2b/Aggregation/k: 25, m: 150, nr_parties: 300
                        time:   [21.321 ms 22.943 ms 24.806 ms]
Found 21 outliers among 100 measurements (21.00%)
  2 (2.00%) high mild
  19 (19.00%) high severe
STM/Blake2b/Verification/k: 25, m: 150, nr_parties: 300
                        time:   [2.1854 ms 2.1963 ms 2.2096 ms]
Found 3 outliers among 100 measurements (3.00%)
  3 (3.00%) high mild

STM/Blake2b/Key registration/k: 250, m: 1523, nr_parties: 2000
                        time:   [2.5808 s 2.6032 s 2.6377 s]
Found 8 outliers among 100 measurements (8.00%)
  3 (3.00%) high mild
  5 (5.00%) high severe
STM/Blake2b/Play all lotteries/k: 250, m: 1523, nr_parties: 2000
                        time:   [5.8590 ms 5.8654 ms 5.8724 ms]
Found 4 outliers among 100 measurements (4.00%)
  3 (3.00%) high mild
  1 (1.00%) high severe
STM/Blake2b/Aggregation/k: 250, m: 1523, nr_parties: 2000
                        time:   [190.47 ms 190.70 ms 190.96 ms]
Found 10 outliers among 100 measurements (10.00%)
  7 (7.00%) high mild
  3 (3.00%) high severe
STM/Blake2b/Verification/k: 250, m: 1523, nr_parties: 2000
                        time:   [14.007 ms 14.048 ms 14.092 ms]
Found 5 outliers among 100 measurements (5.00%)
  4 (4.00%) high mild
  1 (1.00%) high severe
```