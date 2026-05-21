use super::{Accumulator, C, F, Msm, S};
use midnight_curves::serde::SerdeObject;
use midnight_proofs::utils::{SerdeFormat, helpers::ProcessedSerdeObject};
use std::{collections::BTreeMap, io};

pub trait Write {
    fn write<W: io::Write>(&self, w: &mut W, format: SerdeFormat) -> io::Result<()>;
}
pub trait Read: Sized {
    fn read<R: io::Read>(r: &mut R, format: SerdeFormat) -> io::Result<Self>;
}

impl Write for Msm<S> {
    fn write<W: io::Write>(&self, writer: &mut W, format: SerdeFormat) -> io::Result<()> {
        let bases = self.bases();
        let scalars = self.scalars();
        let fixed_base_scalars = self.fixed_base_scalars();

        writer.write_all(&(bases.len() as u32).to_le_bytes())?;
        for base in &bases {
            base.write(writer, format)?;
        }

        writer.write_all(&(scalars.len() as u32).to_le_bytes())?;
        for scalar in &scalars {
            scalar.write_raw(writer)?;
        }

        writer.write_all(&(fixed_base_scalars.len() as u32).to_le_bytes())?;
        for (key, value) in &fixed_base_scalars {
            let key_bytes = key.as_bytes();
            writer.write_all(&(key_bytes.len() as u32).to_le_bytes())?;
            writer.write_all(key_bytes)?;
            value.write_raw(writer)?;
        }

        Ok(())
    }
}

impl Read for Msm<S> {
    fn read<R: io::Read>(reader: &mut R, format: SerdeFormat) -> io::Result<Msm<S>> {
        let mut num_bases = [0u8; 4];
        reader.read_exact(&mut num_bases)?;
        let num_bases = u32::from_le_bytes(num_bases);

        let bases: Vec<_> = (0..num_bases)
            .map(|_| C::read(reader, format))
            .collect::<Result<_, _>>()?;

        let mut num_scalars = [0u8; 4];
        reader.read_exact(&mut num_scalars)?;
        let num_scalars = u32::from_le_bytes(num_scalars);

        let scalars: Vec<_> = (0..num_scalars)
            .map(|_| F::read_raw(reader))
            .collect::<Result<_, _>>()?;

        let mut num_fixed_base_scalars = [0u8; 4];
        reader.read_exact(&mut num_fixed_base_scalars)?;
        let num_fixed_base_scalars = u32::from_le_bytes(num_fixed_base_scalars);

        let mut fixed_base_scalars = BTreeMap::new();
        for _ in 0..num_fixed_base_scalars {
            let mut key_len = [0u8; 4];
            reader.read_exact(&mut key_len)?;
            let key_len = u32::from_le_bytes(key_len);

            let mut key_bytes = vec![0u8; key_len as usize];
            reader.read_exact(&mut key_bytes)?;
            let key = String::from_utf8(key_bytes)
                .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "invalid UTF-8 key"))?;

            let value = F::read_raw(reader)?;

            fixed_base_scalars.insert(key, value);
        }

        Ok(Msm::new(&bases, &scalars, &fixed_base_scalars))
    }
}

impl Write for Accumulator<S> {
    fn write<W: io::Write>(&self, writer: &mut W, format: SerdeFormat) -> io::Result<()> {
        self.lhs().write(writer, format)?;
        self.rhs().write(writer, format)
    }
}

impl Read for Accumulator<S> {
    fn read<R: io::Read>(reader: &mut R, format: SerdeFormat) -> io::Result<Self> {
        let lhs = Msm::read(reader, format)?;
        let rhs = Msm::read(reader, format)?;
        Ok(Accumulator::<S>::new(lhs, rhs))
    }
}
