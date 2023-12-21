use pallas_primitives::conway::PlutusData;
use pallas_primitives::ToCanonicalJson;

use super::model::Datum;

impl<'a, C> minicbor::Decode<'a, C> for Datum {
    fn decode(d: &mut minicbor::Decoder<'a>, ctx: &mut C) -> Result<Self, minicbor::decode::Error> {
        PlutusData::decode(d, ctx).map(Datum)
    }
}

impl ToCanonicalJson for Datum {
    fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }
}

/// Inspects the given bytes and returns a decoded `R` instance.
pub fn inspect<R>(inner: Vec<u8>) -> R
where
    for<'b> R: pallas_codec::minicbor::Decode<'b, ()>,
{
    minicbor::decode(&inner).unwrap()
}
