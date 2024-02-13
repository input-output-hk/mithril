use anyhow::anyhow;
use futures::Future;
use mithril_common::StdResult;

/// Utilities to expand aliases into their associated ids.
pub struct ExpanderUtils;

impl ExpanderUtils {
    /// Expands an id alias to the latest id.
    /// If the provided id is "latest", retrieves the list of ids using the given future and returns the first one.
    /// Otherwise, returns the provided id as is.
    pub async fn expand_eventual_id_alias(
        id: &str,
        get_list_of_ids: impl Future<Output = StdResult<Vec<String>>>,
    ) -> StdResult<String> {
        if id.to_lowercase() == "latest" {
            let list = get_list_of_ids.await?;
            let last_element = list.first().ok_or_else(|| anyhow!("Entity not found"))?;
            Ok(last_element.to_string())
        } else {
            Ok(id.to_string())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    async fn get_list_of_ids(ids: Vec<&str>) -> StdResult<Vec<String>> {
        Ok(ids.iter().map(|h| h.to_string()).collect::<Vec<_>>())
    }

    #[tokio::test]
    async fn expand_eventual_id_alias_should_returns_id() {
        let hash = ExpanderUtils::expand_eventual_id_alias(
            "hash-234",
            get_list_of_ids(vec!["hash-123", "hash-234", "hash-345"]),
        )
        .await
        .expect("expand_eventual_id_alias should not error when hash-234 is passed as parameter.");

        assert_eq!("hash-234", hash);
    }

    #[tokio::test]
    async fn expand_eventual_id_alias_latest_lowercase() {
        let hash = ExpanderUtils::expand_eventual_id_alias(
            "latest",
            get_list_of_ids(vec!["hash-123", "hash-234", "hash-345"]),
        )
        .await
        .expect("expand_eventual_id_alias should not error when latest is passed as parameter.");

        assert_eq!("hash-123".to_string(), hash);
    }

    #[tokio::test]
    async fn expand_eventual_id_alias_latest_uppercase() {
        let hash = ExpanderUtils::expand_eventual_id_alias(
            "LATEST",
            get_list_of_ids(vec!["hash-123", "hash-234", "hash-345"]),
        )
        .await
        .expect("expand_eventual_id_alias should not error when latest is passed as parameter.");

        assert_eq!("hash-123".to_string(), hash);
    }

    #[tokio::test]
    async fn expand_eventual_id_alias_should_error_if_list_is_empty() {
        let _ = ExpanderUtils::expand_eventual_id_alias("LATEST", get_list_of_ids(vec![]))
            .await
            .expect_err(
                "expand_eventual_id_alias should returns an error if returned list is empty.",
            );
    }

    #[tokio::test]
    async fn expand_eventual_id_alias_should_return_id_even_if_not_in_list() {
        let hash = ExpanderUtils::expand_eventual_id_alias(
            "hash-whatever",
            get_list_of_ids(vec!["hash-123", "hash-234", "hash-345"]),
        )
        .await
        .expect(
            "expand_eventual_id_alias should not error when hash-whatever is passed as parameter.",
        );

        assert_eq!("hash-whatever".to_string(), hash);
    }
}
