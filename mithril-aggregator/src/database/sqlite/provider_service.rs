use sqlite::Connection;

use crate::{VersionProvider, VersionUpdatedProvider};

pub struct ProviderService {
    connection: Connection,
}

impl ProviderService {
    pub fn new(connection: Connection) -> Self {
        Self { connection }
    }

    pub fn getDbVersionProvider(&self) -> VersionProvider {
        VersionProvider::new(&self.connection)
    }

    pub fn getDbUpdateVersionProvider(&self) -> VersionUpdatedProvider {
        VersionUpdatedProvider::new(&self.connection)
    }
}
