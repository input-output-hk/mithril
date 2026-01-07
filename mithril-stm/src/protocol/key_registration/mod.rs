mod concatenation_registration_entry;
mod register;
mod registration_entry;

pub use concatenation_registration_entry::RegistrationEntryForConcatenation;
pub use register::{ClosedKeyRegistration, KeyRegistration};
pub use registration_entry::RegistrationEntry;

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{Initializer, KeyRegistration, MithrilMembershipDigest, Parameters};

    #[test]
    fn new_key_registration() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let message = [0u8; 16];

        let parameters = Parameters {
            m: 10,
            k: 5,
            phi_f: 0.8,
        };

        let initializer1 = Initializer::new(parameters, 10, &mut rng);
        let initializer2 = Initializer::new(parameters, 20, &mut rng);

        let mut registration = KeyRegistration::initialize();

        registration
            .register(&initializer1.clone().into())
            .expect("Registration should succeed");
        registration
            .register(&initializer2.into())
            .expect("Registration should succeed");

        let closed_key_registration = registration.close_registration();

        let signer1 = initializer1
            .try_create_signer::<MithrilMembershipDigest>(&closed_key_registration)
            .expect("Signer should be created!");

        let signature1 = signer1
            .create_signle_signature(&message)
            .expect("signature should be created");

        println!("{:?}", signature1);
    }
}
