use minicbor::data::Tag;
use pallas_codec::utils::Bytes;
use serde::ser::SerializeMap;
use serde::Serialize;

use super::model::{Constr, Metadatum};

impl<'b, C> minicbor::Decode<'b, C> for Metadatum {
    fn decode(d: &mut minicbor::Decoder<'b>, ctx: &mut C) -> Result<Self, minicbor::decode::Error> {
        match d.datatype()? {
            minicbor::data::Type::U8
            | minicbor::data::Type::U16
            | minicbor::data::Type::U32
            | minicbor::data::Type::U64
            | minicbor::data::Type::I8
            | minicbor::data::Type::I16
            | minicbor::data::Type::I32
            | minicbor::data::Type::I64
            | minicbor::data::Type::Int => {
                let i = d.decode()?;
                Ok(Metadatum::Int(i))
            }
            minicbor::data::Type::Bytes => {
                let b = d.decode_with(ctx)?;
                Ok(Metadatum::Bytes(Bytes::to_string(&b)))
            }
            minicbor::data::Type::String => Ok(Metadatum::Text(d.decode_with(ctx)?)),
            minicbor::data::Type::Array | minicbor::data::Type::ArrayIndef => {
                Ok(Metadatum::List(d.decode_with(ctx)?))
            }
            minicbor::data::Type::Map | minicbor::data::Type::MapIndef => {
                Ok(Metadatum::Map(d.decode_with(ctx)?))
            }
            minicbor::data::Type::Tag => {
                let mut probe = d.probe();
                let tag = probe.tag()?;

                match tag {
                    Tag::Unassigned(121) => Ok(Self::Datum(d.decode_with(ctx)?)),
                    _ => Err(minicbor::decode::Error::message(
                        "unknown tag for inline datum data tag",
                    )),
                }
            }
            _ => Err(minicbor::decode::Error::message(
                "Can't turn data type into metadatum",
            )),
        }
    }
}

impl Serialize for Metadatum {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Metadatum::Datum(constr) => {
                let Constr {
                    tag,
                    constructor,
                    fields,
                } = constr;
                let mut state = serializer.serialize_map(Some(3))?;
                state.serialize_entry("tag", tag)?;
                state.serialize_entry("constructor", constructor)?;
                state.serialize_entry("fields", fields)?;
                state.end()
            }
            Metadatum::Int(int) => {
                let mut state = serializer.serialize_map(Some(1))?;
                state.serialize_entry("int", int)?;
                state.end()
            }
            Metadatum::Bytes(s) => {
                let mut state = serializer.serialize_map(Some(1))?;
                state.serialize_entry("bytes", s)?;
                state.end()
            }
            Metadatum::Text(s) => {
                let mut state = serializer.serialize_map(Some(1))?;
                state.serialize_entry("text", s)?;
                state.end()
            }
            Metadatum::List(list) => {
                let mut state = serializer.serialize_map(Some(1))?;
                state.serialize_entry("list", list)?;
                state.end()
            }
            Metadatum::Map(map) => {
                let mut state = serializer.serialize_map(Some(1))?;
                state.serialize_entry("map", map)?;
                state.end()
            }
        }
    }
}

impl<'b, C, A> minicbor::decode::Decode<'b, C> for Constr<A>
where
    A: minicbor::decode::Decode<'b, C>,
{
    fn decode(d: &mut minicbor::Decoder<'b>, ctx: &mut C) -> Result<Self, minicbor::decode::Error> {
        let tag = d.tag()?;

        match tag {
            Tag::Unassigned(t) => match t {
                121 => Ok(Constr {
                    tag: t,
                    fields: d.decode_with(ctx)?,
                    constructor: Some(0),
                }),
                _ => Err(minicbor::decode::Error::message(
                    "bad tag code for inline datum data",
                )),
            },
            _ => Err(minicbor::decode::Error::message(
                "bad tag code for inline datum data",
            )),
        }
    }
}
