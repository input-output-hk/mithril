mod utils;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet(what: &str) {
    let what = format!("Hello, {what}!");
    alert(&what);
}
