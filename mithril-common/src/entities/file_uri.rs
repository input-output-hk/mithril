use std::collections::HashSet;

use anyhow::anyhow;
use serde::{Deserialize, Serialize};

use crate::StdResult;

/// FileUri represents a file URI used to identify the file's location
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct FileUri(pub String);

impl From<FileUri> for String {
    fn from(file_uri: FileUri) -> Self {
        file_uri.0
    }
}

/// [TemplateUri] represents an URI pattern used to build a file's location
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize)]
pub struct TemplateUri(pub String);

/// [MultiFilesUri] represents a unique location uri for multiple files
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize)]
pub enum MultiFilesUri {
    /// URI template representing several URI
    Template(TemplateUri),
}

impl MultiFilesUri {
    /// Extract a template from a list of URIs
    pub fn extract_template_from_uris(
        file_uris: Vec<String>,
        extractor: impl Fn(&str) -> StdResult<Option<String>>,
    ) -> StdResult<Option<TemplateUri>> {
        let mut templates = HashSet::new();
        for file_uri in file_uris {
            let template_uri = extractor(&file_uri)?;
            template_uri.map(|template| templates.insert(template));
        }

        if templates.len() > 1 {
            return Err(anyhow!("Multiple templates found in the file URIs"));
        }

        Ok(templates.into_iter().next().map(TemplateUri))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn returns_template() {
        let file_uris = vec![
            "http://whatever/00001.tar.gz".to_string(),
            "http://whatever/00002.tar.gz".to_string(),
        ];
        fn extractor_returning_same_uri(_file_uri: &str) -> StdResult<Option<String>> {
            Ok(Some(
                "http://whatever/{immutable_file_number}.tar.gz".to_string(),
            ))
        }

        let template =
            MultiFilesUri::extract_template_from_uris(file_uris, extractor_returning_same_uri)
                .unwrap();

        assert_eq!(
            template,
            Some(TemplateUri(
                "http://whatever/{immutable_file_number}.tar.gz".to_string()
            ))
        );
    }

    #[test]
    fn returns_error_with_multiple_templates() {
        let file_uris = vec![
            "http://whatever/00001.tar.gz".to_string(),
            "http://00002.tar.gz/whatever".to_string(),
        ];
        fn extractor_returning_different_uri(file_uri: &str) -> StdResult<Option<String>> {
            Ok(Some(file_uri.to_string()))
        }

        MultiFilesUri::extract_template_from_uris(file_uris, extractor_returning_different_uri)
            .expect_err(
                "Should return an error when multiple templates are found in the file URIs",
            );
    }
}
