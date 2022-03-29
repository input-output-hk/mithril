# Mithril Snapshotter Demo

**This is a work in progress** :hammer_and_wrench:

This cli implements a very simple snapshotter of the Cardano Node database for proof of concept only:
* a **full** snapshot includes `immutable` and latest `legder` state
* a **light** snapshot includes only `immutable` (the associated ledger state will be recalculated by the Cardano Node at startup)

---
## Pre-requisites:

**Install OpenSSL**

- Install a [correctly configured](https://github.com/openssl/openssl) Open SSL tool (version 1.1.1l+). 


## Download source code:
```bash
# Download sources from github
git clone https://github.com/input-output-hk/mithril-proto/mithril-snapshotter-poc

# Go to sources directory
cd mithril-snapshotter-poc
```

## Run Snapshot and Restore:
```bash
# Run full snapshot of ./testnet.example folder 
# (will be restored to ./testnet.example.restore)
make snapshot-full src=./testnet.example

# Run full snapshot of ./testnet.example folder 
# (will be restored to ./custom.restore)
make snapshot-full src=./testnet.example dest=./custom.restore

# Run light snapshot of ./testnet.example folder 
# (will be restored to ./custom.restore)
make snapshot-light src=./testnet.example dest=./custom.restore

# Run the restored database of the Cardano Node
# Execute this command in the Cardona Node folder
./cardano-node run --database-path=/custom.restore
```

## Example output:
```
Mithril Snapshot in 'full' mode

>> SOURCE: ./testnet.example
>> SNAPSHOT: ./testnet.example.snapshot
>> RESTORE: ./testnet123

>> Input files list
./testnet.example/db/immutable/00004.primary
./testnet.example/db/immutable/00002.secondary
./testnet.example/db/immutable/00004.secondary
./testnet.example/db/immutable/00005.primary
./testnet.example/db/immutable/00003.secondary
./testnet.example/db/immutable/00005.secondary
./testnet.example/db/immutable/00000.secondary
./testnet.example/db/immutable/00004.chunk
./testnet.example/db/immutable/00003.primary
./testnet.example/db/immutable/00002.primary
./testnet.example/db/immutable/00000.chunk
./testnet.example/db/immutable/00002.chunk
./testnet.example/db/immutable/00000.primary
./testnet.example/db/immutable/00005.chunk
./testnet.example/db/immutable/00001.secondary
./testnet.example/db/immutable/00003.chunk
./testnet.example/db/immutable/00001.primary
./testnet.example/db/immutable/00001.chunk
./testnet.example/db/ledger/2178
./testnet.example/db/volatile/blocks-9.dat
./testnet.example/db/volatile/blocks-8.dat
./testnet.example/db/volatile/blocks-11.dat
./testnet.example/db/volatile/blocks-10.dat
./testnet.example/db/volatile/blocks-12.dat
./testnet.example/db/volatile/blocks-13.dat
./testnet.example/db/volatile/blocks-17.dat
./testnet.example/db/volatile/blocks-16.dat
./testnet.example/db/volatile/blocks-14.dat
./testnet.example/db/volatile/blocks-15.dat
./testnet.example/db/volatile/blocks-18.dat
./testnet.example/db/volatile/blocks-19.dat
./testnet.example/db/volatile/blocks-5.dat
./testnet.example/db/volatile/blocks-4.dat
./testnet.example/db/volatile/blocks-6.dat
./testnet.example/db/volatile/blocks-7.dat
./testnet.example/db/volatile/blocks-3.dat
./testnet.example/db/volatile/blocks-2.dat
./testnet.example/db/volatile/blocks-0.dat
./testnet.example/db/volatile/blocks-1.dat
./testnet.example/db/.DS_Store
./testnet.example/db/protocolMagicId
./testnet.example/db/lock
./testnet.example/db/clean

>> Input files size
101M    ./testnet.example/db/immutable
1.3M    ./testnet.example/db/ledger
 13M    ./testnet.example/db/volatile
115M    ./testnet.example/db
115M    ./testnet.example

>> Copy immutable files to ./testnet.example.snapshot/src/db/immutable
>> Duration: 0s

>> Copy protocolMagicId file to ./testnet.example.snapshot/src/db
>> Duration: 0s

>> Copy ledger state latest file to ./testnet.example.snapshot/src/db/ledger
>> Duration: 0s

>> Snapshot src files list
./testnet.example.snapshot/src/db/immutable/00004.primary
./testnet.example.snapshot/src/db/immutable/00002.secondary
./testnet.example.snapshot/src/db/immutable/00004.secondary
./testnet.example.snapshot/src/db/immutable/00005.primary
./testnet.example.snapshot/src/db/immutable/00003.secondary
./testnet.example.snapshot/src/db/immutable/00005.secondary
./testnet.example.snapshot/src/db/immutable/00000.secondary
./testnet.example.snapshot/src/db/immutable/00004.chunk
./testnet.example.snapshot/src/db/immutable/00003.primary
./testnet.example.snapshot/src/db/immutable/00002.primary
./testnet.example.snapshot/src/db/immutable/00000.chunk
./testnet.example.snapshot/src/db/immutable/00002.chunk
./testnet.example.snapshot/src/db/immutable/00000.primary
./testnet.example.snapshot/src/db/immutable/00005.chunk
./testnet.example.snapshot/src/db/immutable/00001.secondary
./testnet.example.snapshot/src/db/immutable/00003.chunk
./testnet.example.snapshot/src/db/immutable/00001.primary
./testnet.example.snapshot/src/db/immutable/00001.chunk
./testnet.example.snapshot/src/db/ledger/2178
./testnet.example.snapshot/src/db/protocolMagicId

>> Snapshot src files size
101M    ./testnet.example.snapshot/src/db/immutable
1.3M    ./testnet.example.snapshot/src/db/ledger
102M    ./testnet.example.snapshot/src/db

>> Create snapshot archive in ./testnet.example.snapshot/archive/snapshot.tar.gz
a .
a ./immutable
a ./ledger
a ./protocolMagicId
a ./ledger/2178
a ./immutable/00004.primary
a ./immutable/00002.secondary
a ./immutable/00004.secondary
a ./immutable/00005.primary
a ./immutable/00003.secondary
a ./immutable/00005.secondary
a ./immutable/00000.secondary
a ./immutable/00004.chunk
a ./immutable/00003.primary
a ./immutable/00002.primary
a ./immutable/00000.chunk
a ./immutable/00002.chunk
a ./immutable/00000.primary
a ./immutable/00005.chunk
a ./immutable/00001.secondary
a ./immutable/00003.chunk
a ./immutable/00001.primary
a ./immutable/00001.chunk
>> Duration: 3s

>> Snapshot archive files list
drwxr-xr-x  0 jp     staff       0 Mar 29 18:50 ./
drwxr-xr-x  0 jp     staff       0 Mar 29 18:50 ./immutable/
drwxr-xr-x  0 jp     staff       0 Mar 29 18:50 ./ledger/
-rw-r--r--  0 jp     staff       9 Mar 29 18:50 ./protocolMagicId
-rw-r--r--  0 jp     staff 1370662 Mar 29 18:50 ./ledger/2178
-rw-r--r--  0 jp     staff   86409 Mar 29 18:50 ./immutable/00004.primary
-rw-r--r--  0 jp     staff 1209544 Mar 29 18:50 ./immutable/00002.secondary
-rw-r--r--  0 jp     staff 1209656 Mar 29 18:50 ./immutable/00004.secondary
-rw-r--r--  0 jp     staff   86409 Mar 29 18:50 ./immutable/00005.primary
-rw-r--r--  0 jp     staff 1209488 Mar 29 18:50 ./immutable/00003.secondary
-rw-r--r--  0 jp     staff 1209544 Mar 29 18:50 ./immutable/00005.secondary
-rw-r--r--  0 jp     staff 1208872 Mar 29 18:50 ./immutable/00000.secondary
-rw-r--r--  0 jp     staff 16117106 Mar 29 18:50 ./immutable/00004.chunk
-rw-r--r--  0 jp     staff    86409 Mar 29 18:50 ./immutable/00003.primary
-rw-r--r--  0 jp     staff    86409 Mar 29 18:50 ./immutable/00002.primary
-rw-r--r--  0 jp     staff 14788562 Mar 29 18:50 ./immutable/00000.chunk
-rw-r--r--  0 jp     staff 16241653 Mar 29 18:50 ./immutable/00002.chunk
-rw-r--r--  0 jp     staff    86409 Mar 29 18:50 ./immutable/00000.primary
-rw-r--r--  0 jp     staff 15798399 Mar 29 18:50 ./immutable/00005.chunk
-rw-r--r--  0 jp     staff  1209040 Mar 29 18:50 ./immutable/00001.secondary
-rw-r--r--  0 jp     staff 15979970 Mar 29 18:50 ./immutable/00003.chunk
-rw-r--r--  0 jp     staff    86409 Mar 29 18:50 ./immutable/00001.primary
-rw-r--r--  0 jp     staff 18784127 Mar 29 18:50 ./immutable/00001.chunk

>> Snapshot archive file size
 30M    ./testnet.example.snapshot/archive

>> Restore snapshot archive to ./testnet123
x ./
x ./immutable/
x ./ledger/
x ./protocolMagicId
x ./ledger/2178
x ./immutable/00004.primary
x ./immutable/00002.secondary
x ./immutable/00004.secondary
x ./immutable/00005.primary
x ./immutable/00003.secondary
x ./immutable/00005.secondary
x ./immutable/00000.secondary
x ./immutable/00004.chunk
x ./immutable/00003.primary
x ./immutable/00002.primary
x ./immutable/00000.chunk
x ./immutable/00002.chunk
x ./immutable/00000.primary
x ./immutable/00005.chunk
x ./immutable/00001.secondary
x ./immutable/00003.chunk
x ./immutable/00001.primary
x ./immutable/00001.chunk
>> Duration: 1s

>> Restored files list
./testnet123/db/immutable/00004.primary
./testnet123/db/immutable/00002.secondary
./testnet123/db/immutable/00004.secondary
./testnet123/db/immutable/00005.primary
./testnet123/db/immutable/00003.secondary
./testnet123/db/immutable/00005.secondary
./testnet123/db/immutable/00000.secondary
./testnet123/db/immutable/00004.chunk
./testnet123/db/immutable/00003.primary
./testnet123/db/immutable/00002.primary
./testnet123/db/immutable/00000.chunk
./testnet123/db/immutable/00002.chunk
./testnet123/db/immutable/00000.primary
./testnet123/db/immutable/00005.chunk
./testnet123/db/immutable/00001.secondary
./testnet123/db/immutable/00003.chunk
./testnet123/db/immutable/00001.primary
./testnet123/db/immutable/00001.chunk
./testnet123/db/ledger/2178
./testnet123/db/protocolMagicId

>> Restored files size
101M    ./testnet123/db/immutable
1.3M    ./testnet123/db/ledger
102M    ./testnet123/db
102M    ./testnet123

>> Snapshot src files digest
>> Digest SHA256 (./testnet.example.snapshot/src/db): 3081aa8d66d66b48f315b72794db6f6d11569ff442762924acc8ab9bbf09d856
>> Duration: 2s

>> Restored files digest
>> Digest SHA256 (./testnet123): 3081aa8d66d66b48f315b72794db6f6d11569ff442762924acc8ab9bbf09d856
>> Duration: 1s
```