use std::{fs::File, io::Write, path::Path};

use slog::{Logger, warn};

use mithril_common::CardanoNetwork;

use crate::MithrilResult;

const CLEAN_FILE_NAME: &str = "clean";
const PROTOCOL_MAGIC_ID_FILENAME: &str = "protocolMagicId";

pub fn create_bootstrap_node_files(
    logger: &Logger,
    db_dir: &Path,
    network: &str,
) -> MithrilResult<()> {
    if let Err(error) = File::create(db_dir.join(CLEAN_FILE_NAME)) {
        warn!(
            logger, "Could not create clean shutdown marker file in directory '{}'", db_dir.display();
            "error" => error.to_string()
        );
    };

    if let Ok(network) = CardanoNetwork::from_code(network.to_string(), None) {
        let mut file = File::create(db_dir.join(PROTOCOL_MAGIC_ID_FILENAME))?;
        file.write_all(format!("{}", network.magic_id()).as_bytes())?;
    };

    Ok(())
}

#[cfg(test)]
mod test {
    use mithril_common::{CardanoNetwork, temp_dir};
    use slog::o;

    use super::*;

    #[test]
    fn create_bootstrap_node_files_creates_protocol_magic_id_and_clean_files_for_mainnet() {
        let db_dir = temp_dir!().join("db");
        std::fs::create_dir_all(&db_dir).unwrap();
        let network = CardanoNetwork::from_code("mainnet".to_string(), None).unwrap();
        let logger = slog::Logger::root(slog::Discard, o!());

        create_bootstrap_node_files(&logger, &db_dir, &network.to_string()).unwrap();

        let file_content =
            std::fs::read_to_string(db_dir.join(PROTOCOL_MAGIC_ID_FILENAME)).unwrap();
        assert_eq!(file_content, network.magic_id().to_string());

        assert!(db_dir.join(CLEAN_FILE_NAME).exists());
    }

    #[test]
    fn create_bootstrap_node_files_creates_protocol_magic_id_and_clean_files_for_preprod() {
        let db_dir = temp_dir!().join("db");
        std::fs::create_dir_all(&db_dir).unwrap();
        let network = CardanoNetwork::from_code("preprod".to_string(), None).unwrap();
        let logger = slog::Logger::root(slog::Discard, o!());

        create_bootstrap_node_files(&logger, &db_dir, &network.to_string()).unwrap();

        let file_content =
            std::fs::read_to_string(db_dir.join(PROTOCOL_MAGIC_ID_FILENAME)).unwrap();
        assert_eq!(file_content, network.magic_id().to_string());

        assert!(db_dir.join(CLEAN_FILE_NAME).exists());
    }

    #[test]
    fn create_bootstrap_node_files_creates_protocol_magic_id_and_clean_files_for_preview() {
        let db_dir = temp_dir!().join("db");
        std::fs::create_dir_all(&db_dir).unwrap();
        let network = CardanoNetwork::from_code("preview".to_string(), None).unwrap();
        let logger = slog::Logger::root(slog::Discard, o!());

        create_bootstrap_node_files(&logger, &db_dir, &network.to_string()).unwrap();

        let file_content =
            std::fs::read_to_string(db_dir.join(PROTOCOL_MAGIC_ID_FILENAME)).unwrap();
        assert_eq!(file_content, network.magic_id().to_string());

        assert!(db_dir.join(CLEAN_FILE_NAME).exists());
    }

    #[test]
    fn create_bootstrap_node_files_creates_protocol_magic_id_file_and_create_clean_file_for_devnet()
    {
        let db_dir = temp_dir!().join("db");
        std::fs::create_dir_all(&db_dir).unwrap();
        let network = CardanoNetwork::from_code("devnet".to_string(), Some(123)).unwrap();
        let logger = slog::Logger::root(slog::Discard, o!());

        create_bootstrap_node_files(&logger, &db_dir, &network.to_string()).unwrap();

        let file_content =
            std::fs::read_to_string(db_dir.join(PROTOCOL_MAGIC_ID_FILENAME)).unwrap();
        assert_eq!(file_content, network.magic_id().to_string());

        assert!(db_dir.join(CLEAN_FILE_NAME).exists());
    }

    #[test]
    fn create_bootstrap_node_files_does_not_create_protocol_magic_id_file_and_create_clean_file_for_private_network()
     {
        let db_dir = temp_dir!().join("db");
        std::fs::create_dir_all(&db_dir).unwrap();
        let network = CardanoNetwork::from_code("private".to_string(), Some(123)).unwrap();
        let logger = slog::Logger::root(slog::Discard, o!());

        create_bootstrap_node_files(&logger, &db_dir, &network.to_string()).unwrap();

        assert!(!db_dir.join(PROTOCOL_MAGIC_ID_FILENAME).exists());

        assert!(db_dir.join(CLEAN_FILE_NAME).exists());
    }
}
