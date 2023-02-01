use std::sync::RwLock;

use crate::entities::Epoch;

use super::SupportedEra;

struct SupportedEraStamp {
    era: SupportedEra,
    epoch: Epoch,
}

/// EraChecker allows the verification of the current era
pub struct EraChecker {
    current_era_stamp: RwLock<SupportedEraStamp>,
}

impl EraChecker {
    /// Era checker factory
    pub fn new(era: SupportedEra, epoch: Epoch) -> Self {
        Self {
            current_era_stamp: RwLock::new(SupportedEraStamp { era, epoch }),
        }
    }

    /// Retrieve the current era
    pub fn current_era(&self) -> SupportedEra {
        self.current_era_stamp.read().unwrap().era
    }

    /// Retrieve the current Epoch
    pub fn current_epoch(&self) -> Epoch {
        self.current_era_stamp.read().unwrap().epoch
    }

    /// Change the current era
    pub fn change_era(&self, new_era: SupportedEra, current_epoch: Epoch) {
        let new_stamp = SupportedEraStamp {
            era: new_era,
            epoch: current_epoch,
        };
        let mut stamp = self.current_era_stamp.write().unwrap();
        *stamp = new_stamp;
    }

    /// Check if an era is active
    pub fn is_era_active(&self, era: SupportedEra) -> bool {
        self.current_era() == era
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_change_era() {
        let expected_era = SupportedEra::dummy();
        let era_checker = EraChecker::new(expected_era, Epoch(1));
        era_checker.change_era(expected_era, Epoch(2));

        assert_eq!(expected_era, era_checker.current_era());
        assert_eq!(Epoch(2), era_checker.current_epoch());
        assert!(era_checker.is_era_active(expected_era));
    }
}
