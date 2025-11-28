/// Representation of a Mithril network
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MithrilNetwork(String);

impl MithrilNetwork {
    /// Create a new MithrilNetwork instance
    pub fn new(name: String) -> Self {
        Self(name)
    }

    /// Create a dummy MithrilNetwork instance for testing purposes
    pub fn dummy() -> Self {
        Self("dummy".to_string())
    }

    /// Retrieve the name of the Mithril network
    pub fn name(&self) -> &str {
        &self.0
    }
}

impl From<String> for MithrilNetwork {
    fn from(name: String) -> Self {
        MithrilNetwork::new(name)
    }
}

impl From<&str> for MithrilNetwork {
    fn from(name: &str) -> Self {
        MithrilNetwork::new(name.to_string())
    }
}
