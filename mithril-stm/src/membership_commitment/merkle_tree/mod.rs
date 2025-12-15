//! Merkle tree implementation for STM

mod commitment;
mod error;
mod leaf;
mod path;
mod tree;

pub use commitment::*;
pub use error::*;
pub use leaf::*;
pub use path::*;
pub use tree::*;

// ---------------------------------------------------------------------
// Heap Helpers
// ---------------------------------------------------------------------
fn parent(i: usize) -> usize {
    assert!(i > 0, "The root node does not have a parent");
    (i - 1) / 2
}

fn left_child(i: usize) -> usize {
    (2 * i) + 1
}

fn right_child(i: usize) -> usize {
    (2 * i) + 2
}

fn sibling(i: usize) -> usize {
    assert!(i > 0, "The root node does not have a sibling");
    // In the heap representation, the left sibling is always odd
    // And the right sibling is the next node
    // We're assuming that the heap is complete
    if i % 2 == 1 { i + 1 } else { i - 1 }
}
