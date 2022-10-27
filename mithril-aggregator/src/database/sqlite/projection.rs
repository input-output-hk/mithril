use std::collections::HashMap;

pub struct ProjectionField {
    pub name: String,
    pub definition: String,
    pub output_type: String,
}

impl ProjectionField {
    pub fn new(name: &str, definition: &str, output_type: &str) -> Self {
        Self {
            name: name.to_string(),
            definition: definition.to_string(),
            output_type: output_type.to_string(),
        }
    }
}

pub trait Projection {
    fn add_field(&mut self, field_name: &str, definition: &str, output_type: &str) {
        self.set_field(ProjectionField {
            name: field_name.to_string(),
            definition: definition.to_string(),
            output_type: output_type.to_string(),
        })
    }

    fn set_field(&mut self, field: ProjectionField);

    fn get_fields(&self) -> &Vec<ProjectionField>;

    fn expand(&self, aliases: HashMap<String, String>) -> String {
        let mut fields: String = self
            .get_fields()
            .iter()
            .map(|field| format!("{} as {}", field.definition, field.name))
            .collect::<Vec<String>>()
            .join(", ");

        for (alias, source) in aliases {
            fields = fields.replace(&alias, &source);
        }

        fields
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestProjection {
        fields: Vec<ProjectionField>,
    }

    impl TestProjection {
        pub fn new() -> Self {
            let mut projection = Self { fields: Vec::new() };

            projection.add_field("test_id", "{:test:}.test_id", "integer");
            projection.add_field("name", "{:test:}.name", "text");
            projection.add_field("created_at", "{:test:}.created_at", "timestamp");
            projection.add_field("thing_count", "count({:thing:}.*)", "integer");

            projection
        }
    }

    impl Projection for TestProjection {
        fn get_fields(&self) -> &Vec<ProjectionField> {
            &self.fields
        }

        fn set_field(&mut self, field: ProjectionField) {
            let _ = self.fields.push(field);
        }
    }

    #[test]
    fn simple_projection() {
        let projection = TestProjection::new();
        let aliases: HashMap<String, String> = [("{:test:}", "pika"), ("{:thing:}", "thing_alias")]
            .into_iter()
            .map(|(a, b)| (a.to_string(), b.to_string()))
            .collect();

        assert_eq!(
            "pika.test_id as test_id, pika.name as name, pika.created_at as created_at, count(thing_alias.*) as thing_count".to_string(),
            projection.expand(aliases)
        )
    }
}
