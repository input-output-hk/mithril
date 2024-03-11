# Test immutable files

> [!IMPORTANT]
> The directory name matters, it must be named `immutable` in order to be understood as an
> immutable db directory by our db parser (see ([immutable_file.rs](./../../mithril-common/src/digesters/immutable_file.rs))).

> [!NOTE]
> The `.primary` and `.secondary` files are not real data. They're empty files that are
> here in order to simulate a real cardano immutable db structure.

These files were generated manually from the blocks in the sibling folder:

| Imput Block file | Number of transactions | Output immutable file |
|------------------|------------------------|-----------------------|
| [byron2.block](./blocks/byron2.block) | `2` | `00000.chunk`         |
| [shelley1.block](./blocks/shelley1.block) | `4` | `00000.chunk`         |
| [mary1.block](./blocks/mary1.block) | `14` | `00000.chunk`         |
| [allegra1.block](./blocks/allegra1.block) | `3` | `00001.chunk`         |
| [alonzo1.block](./blocks/alonzo1.block) | `5` | `00001.chunk`         |
| [conway1.block](./blocks/conway1.block) | `1` | `00001.chunk`         |

The following code was used to generate the immutable files:
```rust
use std::io::Write;

let immutable_files = vec![
    (
        "./00000.chunk",
        vec![
            (
                include_str!("./blocks/byron2.block"),
                2usize,
            ),
            (
                include_str!("./blocks/shelley1.block"),
                4,
            ),
            (
                include_str!("./blocks/mary1.block"),
                14,
            ),
        ],
    ),
    (
        "./00001.chunk",
        vec![
            (
                include_str!("./blocks/allegra1.block"),
                3,
            ),
            (
                include_str!("./blocks/alonzo1.block"),
                5,
            ),
            (
                include_str!("./blocks/conway1.block"),
                5,
            ),
        ],
    ),
];

for immutable_file_definition in immutable_files {
    let immutable_file_path = immutable_file_definition.0;
    let mut immutable_file = fs::File::create(immutable_file_path).unwrap();

    for (block_str, _tx_count) in immutable_file_definition.1.into_iter() {
        let cbor = hex::decode(block_str).expect("invalid hex");
        immutable_file.write_all(&cbor).unwrap();
    }
}
```

