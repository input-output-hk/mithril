use std::sync::RwLock;

use super::SupportedEra;

/// EraChecker allows the verification of the current era
pub struct EraChecker {
    current_era: RwLock<SupportedEra>,
}

impl EraChecker {
    /// Era checker factory
    pub fn new(era: SupportedEra) -> Self {
        Self {
            current_era: RwLock::new(era),
        }
    }

    /// Retrieve the current era
    pub fn current_era(&self) -> SupportedEra {
        self.current_era.read().unwrap().to_owned()
    }

    /// Change the current era
    pub fn change_era(&self, new_era: SupportedEra) {
        let mut era = self.current_era.write().unwrap();
        *era = new_era;
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
        let era_checker = EraChecker::new(expected_era);
        era_checker.change_era(expected_era);

        assert_eq!(expected_era, era_checker.current_era());
    }

    #[test]
    fn is_era_active_panics_when_current_era_not_set() {
        let expected_era = SupportedEra::dummy();
        let era_checker = EraChecker::new(expected_era);
        era_checker.is_era_active(expected_era);
    }
}
