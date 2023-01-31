use std::{error::Error as StdError, str::FromStr};

use crate::entities::Epoch;
use async_trait::async_trait;
use thiserror::Error;

use super::{supported_era::UnsupportedEraError, SupportedEra};

/// Represents a tag of Era change.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EraMarker {
    name: String,
    epoch: Option<Epoch>,
}

/// Adapters are responsible of technically reading the information of
/// [EraMarker]s from a backend.
#[async_trait]
pub trait EraReaderAdapter {
    /// Read era markers from the underlying adapter.
    async fn read(&self) -> Result<Vec<EraMarker>, Box<dyn StdError>>;
}

/// This is a response from the [EraReader]. It contains [EraMarker]s read from
/// the adapter. It can try to cast the given markers to [SupportedEra]s.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EraEpochToken {
    current_epoch: Epoch,
    current_era: EraMarker,
    next_era: Option<EraMarker>,
}

impl EraEpochToken {
    /// Instanciate a new [EraMarker].
    pub fn new(current_epoch: Epoch, current_era: EraMarker, next_era: Option<EraMarker>) -> Self {
        Self {
            current_epoch,
            current_era,
            next_era,
        }
    }

    /// Try to cast the current [EraMarker] to a [SupportedEra]. If it fails,
    /// that means the current Era is not supported by this version of the
    /// software.
    pub fn get_current_supported_era(&self) -> Result<SupportedEra, UnsupportedEraError> {
        SupportedEra::from_str(&self.current_era.name)
    }

    /// Return the [EraMarker] of the current Era.
    pub fn get_current_era_marker(&self) -> &EraMarker {
        &self.current_era
    }

    /// Try to cast the next [EraMarker] to a [SupportedEra]. If it fails, that
    /// means the coming Era will not be supported by this version of the
    /// software. This mechanism is used to issue a warning to the user asking
    /// for upgrade.
    pub fn get_next_supported_era(&self) -> Result<Option<SupportedEra>, UnsupportedEraError> {
        match self.next_era.as_ref() {
            Some(marker) => Ok(Some(SupportedEra::from_str(&marker.name)?)),
            None => Ok(None),
        }
    }

    /// Return the [EraMarker] for the coming Era if any.
    pub fn get_next_era_marker(&self) -> Option<&EraMarker> {
        self.next_era.as_ref()
    }
}

/// The EraReader is responsible of giving the current Era and the Era to come.
/// Is uses an [EraReaderAdapter] to read data from a backend.
pub struct EraReader {
    adapter: Box<dyn EraReaderAdapter>,
}

/// Error type when [EraReader] fails to return a [EraEpochToken].
#[derive(Debug, Error)]
pub enum EraReaderError {
    /// Underlying adapter fails to return data.
    #[error("Adapter Error message: «{message}» caught error: {error:?}")]
    AdapterFailure {
        /// context message
        message: String,

        /// nested underlying adapter error
        error: Box<dyn StdError>,
    },

    /// Data returned from the adapter are inconsistent or incomplete preventing
    /// the determined the current [EraMarker].
    #[error("Current Era could not be determined with data from the adapter.")]
    CurrentEraNotFound,
}

impl EraReader {
    /// Instantiate the [EraReader] injecting the adapter.
    pub fn new(adapter: Box<dyn EraReaderAdapter>) -> Self {
        Self { adapter }
    }

    /// This methods triggers the adapter to read the markers from the backend.
    /// It tries to determine the current Era and the next Era if any from the
    /// data returned from the adapter.
    pub async fn read_era_epoch_token(
        &self,
        current_epoch: Epoch,
    ) -> Result<EraEpochToken, EraReaderError> {
        let eras = self
            .adapter
            .read()
            .await
            .map_err(|e| EraReaderError::AdapterFailure {
                message: format!("Reading from EraReader adapter raised an error: '{}'.", &e),
                error: e,
            })?;

        let get_epoch = |era_marker: Option<&EraMarker>| -> Epoch {
            if let Some(marker) = era_marker {
                marker.epoch.unwrap_or_default()
            } else {
                Epoch(0)
            }
        };
        let current_marker = eras.iter().fold(None, |acc, marker| {
            if get_epoch(Some(marker)) <= current_epoch && get_epoch(Some(marker)) > get_epoch(acc)
            {
                Some(marker)
            } else {
                acc
            }
        });
        let current_era_marker =
            current_marker.ok_or_else(|| EraReaderError::CurrentEraNotFound)?;

        let next_era_marker = eras.last().filter(|&marker| marker != current_era_marker);

        Ok(EraEpochToken::new(
            current_epoch,
            current_era_marker.to_owned(),
            next_era_marker.cloned(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Default)]
    struct DummyAdapter {
        markers: Vec<EraMarker>,
    }

    impl DummyAdapter {
        pub fn set_markers(&mut self, markers: Vec<EraMarker>) {
            self.markers = markers;
        }
    }

    #[async_trait]
    impl EraReaderAdapter for DummyAdapter {
        async fn read(&self) -> Result<Vec<EraMarker>, Box<dyn StdError>> {
            Ok(self.markers.clone())
        }
    }

    fn get_basic_marker_sample() -> Vec<EraMarker> {
        vec![
            EraMarker {
                name: "one".to_string(),
                epoch: Some(Epoch(1)),
            },
            EraMarker {
                name: "thales".to_string(),
                epoch: None,
            },
            EraMarker {
                name: "thales".to_string(),
                epoch: Some(Epoch(10)),
            },
        ]
    }

    #[tokio::test]
    async fn current_era_is_supported() {
        let markers: Vec<EraMarker> = get_basic_marker_sample();
        let mut adapter = DummyAdapter::default();
        adapter.set_markers(markers);

        let reader = EraReader::new(Box::new(adapter));
        let token = reader.read_era_epoch_token(Epoch(10)).await.unwrap();

        assert_eq!(
            EraEpochToken {
                current_epoch: Epoch(10),
                current_era: EraMarker {
                    name: "thales".to_string(),
                    epoch: Some(Epoch(10))
                },
                next_era: None,
            },
            token
        );
    }

    #[tokio::test]
    async fn era_epoch_token() {
        let markers: Vec<EraMarker> = get_basic_marker_sample();
        let mut adapter = DummyAdapter::default();
        adapter.set_markers(markers);

        let reader = EraReader::new(Box::new(adapter));
        let token = reader.read_era_epoch_token(Epoch(10)).await.unwrap();
        assert_eq!(
            SupportedEra::dummy(),
            token
                .get_current_supported_era()
                .expect("the given era is supported")
        );
        assert!(token.get_next_era_marker().is_none());
        assert!(token
            .get_next_supported_era()
            .expect("None era shall not fail when asked.")
            .is_none());
    }

    #[tokio::test]
    async fn previous_era_is_not_supported() {
        let markers: Vec<EraMarker> = get_basic_marker_sample();
        let mut adapter = DummyAdapter::default();
        adapter.set_markers(markers);

        let reader = EraReader::new(Box::new(adapter));
        let token = reader.read_era_epoch_token(Epoch(9)).await.unwrap();

        assert_eq!(
            EraEpochToken {
                current_epoch: Epoch(9),
                current_era: EraMarker {
                    name: "one".to_string(),
                    epoch: Some(Epoch(1))
                },
                next_era: Some(EraMarker {
                    name: "thales".to_string(),
                    epoch: Some(Epoch(10))
                }),
            },
            token
        );
    }

    #[tokio::test]
    async fn error_when_no_current_era() {
        let markers = vec![
            EraMarker {
                name: "one".to_string(),
                epoch: None,
            },
            EraMarker {
                name: "two".to_string(),
                epoch: None,
            },
            EraMarker {
                name: "three".to_string(),
                epoch: Some(Epoch(100)),
            },
        ];

        let mut adapter = DummyAdapter::default();
        adapter.set_markers(markers);

        let reader = EraReader::new(Box::new(adapter));
        let _ = reader
            .read_era_epoch_token(Epoch(9))
            .await
            .expect_err("No current era must make the reader to fail.");
    }

    #[tokio::test]
    async fn error_when_no_era() {
        let adapter = DummyAdapter::default();

        let reader = EraReader::new(Box::new(adapter));
        let _ = reader
            .read_era_epoch_token(Epoch(9))
            .await
            .expect_err("The adapter gave no result hence the reader should fail.");
    }

    #[tokio::test]
    async fn current_era_is_not_supported() {
        let markers: Vec<EraMarker> = get_basic_marker_sample();
        let mut adapter = DummyAdapter::default();
        adapter.set_markers(markers);

        let reader = EraReader::new(Box::new(adapter));
        let token = reader.read_era_epoch_token(Epoch(9)).await.unwrap();

        token
            .get_current_supported_era()
            .expect_err("The era 'one' is not supported hence the token must issue an error.");

        assert_eq!(
            &EraMarker {
                name: "one".to_string(),
                epoch: Some(Epoch(1))
            },
            token.get_current_era_marker()
        );
        token
            .get_next_supported_era()
            .expect("The next era is supported hence this shall not fail.");
    }
}
