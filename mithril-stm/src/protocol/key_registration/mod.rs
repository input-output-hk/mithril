mod closed_registration_entry;
mod concatenation_registration_entry;
mod register;
mod registration_entry;

#[cfg(feature = "future_snark")]
mod snark_registration_entry;

pub use closed_registration_entry::ClosedRegistrationEntry;
pub use concatenation_registration_entry::RegistrationEntryForConcatenation;
pub use register::{ClosedKeyRegistration, KeyRegistration};
pub use registration_entry::RegistrationEntry;
