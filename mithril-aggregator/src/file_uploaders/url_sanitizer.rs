use anyhow::{anyhow, Context};
use reqwest::Url;

use mithril_common::StdResult;

/// Sanitize URL path by removing empty segments and adding trailing slash
pub fn sanitize_url_path(url: &Url) -> StdResult<Url> {
    let segments_non_empty = url
        .path_segments()
        .map(|s| s.into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>())
        .unwrap_or_default();
    let mut url = url.clone();
    {
        let mut url_path_segments = url
            .path_segments_mut()
            .map_err(|e| anyhow!("error parsing URL: {e:?}"))
            .with_context(|| "while sanitizing URL path: {url}")?;
        let url_path_segments_cleared = url_path_segments.clear();
        for segment in segments_non_empty {
            url_path_segments_cleared.push(segment);
        }
        url_path_segments_cleared.push("");
    }

    Ok(url)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sanitize_url_path() {
        let url = Url::parse("http://example.com/a//b/c.ext?test=123").unwrap();
        assert_eq!(
            "http://example.com/a/b/c.ext/?test=123",
            sanitize_url_path(&url).unwrap().as_str()
        );

        let url = Url::parse("http://example.com/a//b/c.ext").unwrap();
        assert_eq!(
            "http://example.com/a/b/c.ext/",
            sanitize_url_path(&url).unwrap().as_str()
        );

        let url = Url::parse("http://example.com/a//b/c").unwrap();
        assert_eq!(
            "http://example.com/a/b/c/",
            sanitize_url_path(&url).unwrap().as_str()
        );

        let url = Url::parse("http://example.com/").unwrap();
        assert_eq!(
            "http://example.com/",
            sanitize_url_path(&url).unwrap().as_str()
        );

        let url = Url::parse("http://example.com").unwrap();
        assert_eq!(
            "http://example.com/",
            sanitize_url_path(&url).unwrap().as_str()
        );
    }
}
