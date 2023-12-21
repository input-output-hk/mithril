use pallas_primitives::conway::{BigInt, Constr, PlutusData};
use serde::ser::{SerializeMap, SerializeStruct};
use serde::Serialize;

use super::model::Datum;

impl<'a, C> minicbor::Decode<'a, C> for Datum {
    fn decode(d: &mut minicbor::Decoder<'a>, ctx: &mut C) -> Result<Self, minicbor::decode::Error> {
        PlutusData::decode(d, ctx).map(Datum)
    }
}

impl Serialize for Datum {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match &self.0 {
            PlutusData::Constr(constr) => {
                let Constr { fields, .. }: Constr<Datum> = Constr {
                    tag: constr.tag,
                    any_constructor: constr.any_constructor,
                    fields: constr.fields.iter().cloned().map(Datum).collect(),
                };
                let mut state = serializer.serialize_struct("Datum", 2)?;
                state.serialize_field("constructor", &constr.any_constructor.unwrap_or(0))?;
                state.serialize_field("fields", &fields)?;
                state.end()
            }
            PlutusData::BigInt(big_int) => match big_int {
                BigInt::Int(int) => serializer.serialize_newtype_variant("BigInt", 0, "int", int),
                BigInt::BigUInt(bytes) | BigInt::BigNInt(bytes) => serializer
                    .serialize_newtype_variant(
                        "BigInt",
                        0,
                        "bytes",
                        &hex::encode(bytes.as_slice()),
                    ),
            },
            PlutusData::BoundedBytes(s) => {
                serializer.serialize_newtype_variant("PlutusData", 0, "bytes", s)
            }
            PlutusData::Array(list) => {
                let list: Vec<Datum> = list.iter().cloned().map(Datum).collect();
                let mut state = serializer.serialize_map(Some(1))?;
                state.serialize_entry("list", &list)?;
                state.end()
            }
            PlutusData::Map(map) => {
                let mut state = serializer.serialize_map(Some(1))?;
                state.serialize_entry("map", map)?;
                state.end()
            }
        }
    }
}
