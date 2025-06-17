#![warn(missing_docs)]
//! This crate provides a toolset to verify conformity of http routes against an Open Api specification.

mod apispec;

pub use apispec::*;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_openapi_examples_conformity() {
        let api_spec = APISpec::from_file(DEFAULT_SPEC_FILE);

        let errors: Vec<String> = api_spec.verify_examples();

        assert!(
            errors.is_empty(),
            "Errors in examples\n{}",
            errors.join("\n")
        );
    }
}
