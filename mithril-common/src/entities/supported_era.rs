use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter, EnumString, IntoEnumIterator};

/// The era that the software is running or will run
#[derive(
    Display, EnumString, EnumIter, Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize,
)]
#[serde(rename_all = "lowercase")]
#[strum(serialize_all = "lowercase")]
pub enum SupportedEra {
    /// Pythagoras era
    Pythagoras,
}

impl SupportedEra {
    /// Retrieve the list of supported eras
    pub fn eras() -> Vec<Self> {
        Self::iter().collect()
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::test::double::Dummy;

    use super::*;

    #[test]
    fn correct_number_of_eras() {
        let total_eras = SupportedEra::eras().len();

        assert!(total_eras > 0);
        assert!(total_eras <= 2);
    }

    #[test]
    fn from_str() {
        let supported_era = SupportedEra::from_str(&SupportedEra::dummy().to_string())
            .expect("This era name should be supported.");

        assert_eq!(SupportedEra::dummy(), supported_era);
    }
}
