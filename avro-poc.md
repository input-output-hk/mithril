# Apache Avro

Official rust bin (still in beta): [https://crates.io/crates/apache-avro](https://crates.io/crates/apache-avro)


## Design choices

Two serialization encodings available: binary and Json. Binary is the default but json can be interesting for debugging & webapps.

The whole schema isn't encoded in binary encoded message:
> Binary encoded Avro data does not include type information or field names. The benefit is that the serialized data is small, but as a result a schema must always be used in order to read Avro data correctly.
> The best way to ensure that the schema is structurally identical to the one used to write the data is to use the exact same schema.
> 
> Therefore, files or systems that store Avro data should always include the writer’s schema for that data. Avro-based remote procedure call (RPC) systems must also guarantee that remote recipients of data have a copy of the schema used to write that data.

cf. [doc](https://avro.apache.org/docs/1.11.1/specification/#data-serialization-and-deserialization)


Reading and writting messages can be done using different schema, as long as the read schema is compatible with the read write schema.


## Types

[Primitives](https://avro.apache.org/docs/1.11.1/specification/#primitive-types-1):
* null
* bool
* int, long
* float, double
* bytes
* string (utf8)

Note that there's **no unsigned numeric types**.

[Complex](https://avro.apache.org/docs/1.11.1/specification/#complex-types-1):
* Records: similar to strucs.
* Enums: simple enums, should be mapped to rust enum with only unit values.
* Arrays
* Maps
* Unions
* Fixed: fixed size data (ie: a hash)

[Logical types](https://avro.apache.org/docs/1.11.1/specification/#logical-types) are types derived from primitives or complex types by adding extra attributes:
* Decimal
* UUID
* Date
* Time: two variants: millisecond precision or microsecond precision.
* Timestamp: two variants: millisecond precision or microsecond precision.
* Local Timestamp: two variants: millisecond precision or microsecond precision.
* Duration

Notes:
* Records fields are not nullable by default, if you want so you must specify it in its type using an union (ie: `"type": ["null", "string"]` instead of `"type": "string"`).
* default values are only used for reading, when writting you still MUST provide a value, it's utility lies in backward compatibility.

## Compression Codecs

_[doc](https://avro.apache.org/docs/1.11.1/specification/#required-codecs)_

* null: don't compress data
* deflate: follow RFC1951, does not include a checksum contrary to other compression codecs
* (optional) bzip2
* (optional) snappy: a google compression lib.
* (optional) xz
* (optional) zstandard: a facebook compression lib.

## Useful 3rd party articles
* [Avro: unions & default values (by Federico Ragona)](https://federico.is/posts/2020/07/30/avro-unions-and-default-values/)
