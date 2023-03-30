use std::collections::{hash_map::Iter, HashMap};

/// Handful tool to store SQL source aliases.
/// ```
/// let aliases = SourceAlias::new(&[("first", "one"), ("second", "two")]);
/// ```
#[derive(Debug, Default, Clone)]
pub struct SourceAlias {
    /// Internal HashMap of source_name => source_alias
    aliases: HashMap<String, String>,
}

impl SourceAlias {
    /// Create a new alias from a `&[(name, alias)]` list
    pub fn new(aliases: &[(&str, &str)]) -> Self {
        Self {
            aliases: aliases
                .iter()
                .map(|(name, alias)| (name.to_string(), alias.to_string()))
                .collect(),
        }
    }

    /// get an iterator from the current alias map
    pub fn get_iterator(&self) -> Iter<'_, String, String> {
        self.aliases.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_source_alias() {
        let source_alias = SourceAlias::new(&[("first", "one"), ("second", "two")]);
        let mut fields = "first.one, second.two".to_string();

        for (alias, source) in source_alias.get_iterator() {
            fields = fields.replace(alias, source);
        }
        assert_eq!("one.one, two.two".to_string(), fields);
    }
}
