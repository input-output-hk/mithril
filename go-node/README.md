# Mithril Node POC

## Requirements

In order to run a mithril node you need a ready PostgreSQL with sync-db data.

## Building

Before compiling mithril node you need to build a rust lib. Just run the command below.

```shell
make mlib
```

## Running Node

```shell
cd go-node
POSTGRE_DSN="host=127.0.0.1 port=5432 user=cdb password=123456 dbname=testnet sslmode=disable" \
MITHRIL_PARTY_ID=<N> \
 go run cmd/node/main.go
```

where N is a ``party_id`` index from 0 to 4. By default, each node starts as a leader. If yoo want to run node just as
signing node then provide ``LEADER=false`` to environment variables list.


## Working Algorithm

Running node every 30 seconds fetches a UTxO and initiates a certificate sign process. If the certificate sign process
receives enough amount of valid signatures from peer nodes then the node will save a certificate to the DB.

## Playing Parameters

It is not recommended to touch mithril parameters. Default values are OK but if you want just follow instructions below.

First you need to change parameters in the configuration file ``configs/dev-config.yaml`` file. Then create initializer
instance using ``mknode`` tool:

```shell
$ go run cmd/mknode/main.go
Usage: mknode <k> <m> <phi> <party_id> <stake>

$ go run cmd/mknode/main.go 5 100 0.2 1 1
Input args: 5 100 0.2 1 1
Initializer: part_id=1, stake=1
Participant: 1 1
AQAAAAAAAAABAAAAAAAAAGQAAAAAAAAABQAAAAAAAACamZmZmZnJP9sS3FJEAsCiY2lsCT2KTBvJ9rxXFE16bK+kLIqrKxENiqJ206PKPEPn8TsQdtlGcobd2Tq8y9ViIN+feATWja6/BCHEC4IF4DYQPcd+I0cA8ogJ0hE7bFTAWM5VGqe9iltFqoF2/lThpMdMhr+nevZe6atCRwR9Mt7TtZstzMYAKmEB1hk9wJwIWEZcaPw/jLp1OYXaNbCMDY14rkdHMIJJm1uYcdu613H+Ab9phXkAkOx66bLiWv+699EndffLQbCutXbIOWItvZsQHSQ3be91Nb+iwnt58JrlFE+frgcArMsIG8b6cljAAszInsGnqiOOcpT+SvXjZDD31TK3qas9aOa6Ygx6PQgyGjTusm0BiRl/8kfvHhSRwdZSqQTxLZdWWt1E6TZPAGASyAgilL840bzdbditVI8vVt5mRxsA7ivoOzJMzcjVREGJYKWIhczrSkGY+Hl+ISgM6bGJFcHBqxni552Uw/szAC7x3awBfgWFYVfvWM5fhvKsGGNBURKCLsWooWSIdRzxb4BjB9nvbERvCgoaAgWwrUqp6nIA7b9z+gA+Egjnr+0JdtqcXhw/Smrw594wnMh+tnMMORZeA7cWL1JAJATRYKDnVYwBhHXiPoyqi8c0UxsbvhOMLSZCAzFqaSjW6z3iRPyJz2VXdX6zANJ9WhwPQCVQtq4ANmtmXb99I2pH2yo4sA+m9LI/vjy9cTasG/7VIW3dAJ6VICcwvKZaSgzOKxUp464AuYliXaBrMjJXmaVjoSQjTa00z6Qq0A2FbolBTY1zDwWg88uHypndLlzp8GM448YA
```

Copy the last line (base64) to the configuration file to correspond participant initializer.


## API SERVER

Following API endpoints are available:

* ``/certs`` - list currently signed certificates.
* ``/certs/{merkle_root}`` - list certificate UTxO addresses.
* ``/certs/{merkle_root}/{addr}`` - gets proofs and UTxO for a specific address.
