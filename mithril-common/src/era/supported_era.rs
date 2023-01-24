/// The era that the software is running or will run
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SupportedEra {
    /// Thales era
    Thales,
    /// Pythagoras era
    Pythagoras,
}

impl SupportedEra {
    /// Retrieve a dummy era (for test only)
    #[cfg(any(test, feature = "test_only"))]
    pub fn dummy() -> Self {
        Self::Thales
    }
}
