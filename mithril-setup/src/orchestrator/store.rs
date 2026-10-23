//! Store module

/// Type of a field
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FieldType {
    /// A string field
    String,
    /// A signed integer field
    Int,
    /// A boolean field
    Bool,
    /// Path to a file
    FilePath,
}

/// State of a field
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum State {
    /// The field is not set
    Unset,
    /// The field is discovered from the environment
    Discovered,
    /// The field is guessed from other fields
    Guessed,
    /// The field is filled by the user
    FilledByUser,
}

/// Representation of a field in the store
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    /// Name
    pub name: String,

    /// Description
    pub description: String,

    /// Type of the field, for verification
    pub kind: FieldType,

    /// Default value
    pub default: Option<String>,

    /// Value
    pub value: Option<String>,

    /// State
    pub state: State,
}

impl Field {
    /// Create a new field
    pub fn init(
        name: String,
        description: String,
        kind: FieldType,
        default: Option<String>,
    ) -> Self {
        Self {
            name,
            description,
            kind,
            default,
            value: None,
            state: State::Unset,
        }
    }

    /// Set the value of the field and update its state to discovered
    pub fn set_discovered_value(&mut self, value: String) {
        self.value = Some(value);
        self.state = State::Discovered;
    }
}

/// Representation of the store containing all fields
pub struct Store {
    fields: Vec<Field>,
}

impl Store {
    /// Create a new store with the given fields
    pub fn new(fields: Vec<Field>) -> Self {
        Self { fields }
    }

    /// Get a reference to a field by its name
    pub fn get_field(&self, name: &str) -> Option<&Field> {
        self.fields.iter().find(|f| f.name == name)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    mod field {
        use super::*;

        #[test]
        fn test_field_are_initialized_at_unset_state() {
            let field = Field::init(
                "network".to_string(),
                "The Cardano network to target".to_string(),
                FieldType::String,
                Some("preview".to_string()),
            );
            assert_eq!(field.state, State::Unset);
        }

        #[test]
        fn _test_set_discovered_value_sets_the_value_and_state() {
            let mut field = Field::init(
                "network".to_string(),
                "The Cardano network to target".to_string(),
                FieldType::String,
                Some("preview".to_string()),
            );
            field.set_discovered_value("mainnet".to_string());
            assert_eq!(field.value.as_deref(), Some("mainnet"));
            assert_eq!(field.state, State::Discovered);
        }
    }

    mod store {
        use super::*;

        #[test]
        fn test_store_can_get_field_by_name() {
            let field1 = Field::init(
                "network".to_string(),
                "The Cardano network to target".to_string(),
                FieldType::String,
                Some("preview".to_string()),
            );
            let field2 = Field::init(
                "magic_id".to_string(),
                "Magic id of the Cardano network".to_string(),
                FieldType::Int,
                None,
            );
            let store = Store::new(vec![field1, field2]);

            let retrieved_field = store.get_field("network");
            assert!(retrieved_field.is_some());
            assert_eq!(retrieved_field.unwrap().name, "network");

            let non_existent_field = store.get_field("non_existent");
            assert!(non_existent_field.is_none());
        }
    }
}
