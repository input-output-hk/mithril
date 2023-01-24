use std::sync::RwLock;

use super::SupportedEra;

static CURRENT_ERA: RwLock<Option<SupportedEra>> = RwLock::new(None);

/// EraChecker allows the verification of the current era
pub struct EraChecker;

impl EraChecker {
    /// Retrieve the current era
    pub fn current_era() -> Option<SupportedEra> {
        CURRENT_ERA.read().unwrap().to_owned()
    }

    /// Change the current era
    pub fn change_era(new_era: SupportedEra) {
        let mut era = CURRENT_ERA.write().unwrap();
        *era = Some(new_era);
    }

    /// Check if an era is active
    pub fn is_era_active(era: SupportedEra) -> bool {
        match Self::current_era() {
            None => {
                panic!("Current era is NOT set, 'is_era_active' can't be called yet.");
            }
            Some(current_era) => current_era == era,
        }
    }

    #[cfg(test)]
    fn reset_era() {
        let mut era = CURRENT_ERA.write().unwrap();
        *era = None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_change_era() {
        EraChecker::reset_era();

        let expected_era = SupportedEra::dummy();
        EraChecker::change_era(expected_era);

        assert_eq!(Some(expected_era), EraChecker::current_era());
    }

    #[test]
    #[should_panic]
    fn is_era_active_panics_when_current_era_not_set() {
        EraChecker::reset_era();

        EraChecker::is_era_active(SupportedEra::dummy());
    }
}
