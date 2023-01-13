use apache_avro::{Schema, Writer};
use serde::Serialize;
use std::error::Error;

pub struct MessageEncoder {
    schema: Schema,
}

impl MessageEncoder {
    pub fn new(schema_str: &str) -> Result<Self, Box<dyn Error>> {
        let schema = Schema::parse_str(schema_str)?;
        Ok(Self { schema })
    }

    pub fn encode<T>(&self, data: T) -> Result<Vec<u8>, Box<dyn Error>>
    where
        T: Serialize,
    {
        let mut writer = Writer::new(&self.schema, Vec::new());
        writer.append_ser(&data)?;

        Ok(writer.into_inner()?)
    }
}
