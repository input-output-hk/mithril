use anyhow::anyhow;
use serde::Deserialize;

use mithril_client::MithrilResult;

pub const ASSET_PLATFORM_LINUX: &str = "linux";
pub const ASSET_PLATFORM_MACOS: &str = "macos";
pub const ASSET_PLATFORM_WINDOWS: &str = "win64";

#[derive(Debug, Clone, Deserialize, Eq, PartialEq)]
pub struct GitHubAsset {
    pub name: String,
    pub browser_download_url: String,
}

#[derive(Debug, Default, Clone, Deserialize)]
pub struct GitHubRelease {
    pub assets: Vec<GitHubAsset>,
    pub prerelease: bool,
}

impl GitHubRelease {
    pub fn get_asset_for_os(&self, target_os: &str) -> MithrilResult<Option<&GitHubAsset>> {
        let os_in_asset_name = match target_os {
            "linux" => ASSET_PLATFORM_LINUX,
            "macos" => ASSET_PLATFORM_MACOS,
            "windows" => ASSET_PLATFORM_WINDOWS,
            _ => return Err(anyhow!("Unsupported platform: {}", target_os)),
        };

        let asset = self.assets.iter().find(|asset| asset.name.contains(os_in_asset_name));

        Ok(asset)
    }

    #[cfg(test)]
    pub fn dummy_with_all_supported_assets() -> Self {
        GitHubRelease {
            assets: vec![
                GitHubAsset {
                    name: format!("asset-name-{ASSET_PLATFORM_LINUX}.tar.gz"),
                    browser_download_url: "https://release-assets.com/linux".to_string(),
                },
                GitHubAsset {
                    name: format!("asset-name-{ASSET_PLATFORM_MACOS}.tar.gz"),
                    browser_download_url: "https://release-assets.com/macos".to_string(),
                },
                GitHubAsset {
                    name: format!("asset-name-{ASSET_PLATFORM_WINDOWS}.zip"),
                    browser_download_url: "https://release-assets.com/windows".to_string(),
                },
            ],
            ..GitHubRelease::default()
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn dummy_asset(os: &str) -> GitHubAsset {
        GitHubAsset {
            name: format!("asset-name-{os}.whatever"),
            browser_download_url: format!("https://release-assets.com/{os}"),
        }
    }

    #[test]
    fn returns_expected_asset_for_each_supported_platform() {
        let release = GitHubRelease {
            assets: vec![
                dummy_asset(ASSET_PLATFORM_LINUX),
                dummy_asset(ASSET_PLATFORM_MACOS),
                dummy_asset(ASSET_PLATFORM_WINDOWS),
            ],
            ..GitHubRelease::default()
        };

        {
            let asset = release.get_asset_for_os("linux").unwrap();
            assert_eq!(asset, Some(&dummy_asset(ASSET_PLATFORM_LINUX)));
        }

        {
            let asset = release.get_asset_for_os("macos").unwrap();
            assert_eq!(asset, Some(&dummy_asset(ASSET_PLATFORM_MACOS)));
        }

        {
            let asset = release.get_asset_for_os("windows").unwrap();
            assert_eq!(asset, Some(&dummy_asset(ASSET_PLATFORM_WINDOWS)));
        }
    }

    #[test]
    fn returns_none_when_asset_is_missing() {
        let release = GitHubRelease {
            assets: vec![dummy_asset(ASSET_PLATFORM_LINUX)],
            ..GitHubRelease::default()
        };

        let asset = release.get_asset_for_os("macos").unwrap();

        assert!(asset.is_none());
    }

    #[test]
    fn fails_for_unsupported_platform() {
        let release = GitHubRelease {
            assets: vec![dummy_asset(ASSET_PLATFORM_LINUX)],
            ..GitHubRelease::default()
        };

        release
            .get_asset_for_os("unsupported")
            .expect_err("Should have failed for unsupported platform");
    }
}
