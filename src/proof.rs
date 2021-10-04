//! General API for producing proofs from statements and witnesses

pub trait ProverEnv {
    type ProvingKey;
    type VerificationKey;

    fn setup(&self) -> (Self::ProvingKey, Self::VerificationKey);
}

pub trait Provable<Env, Witness, Proof>
where
    Env: ProverEnv,
{
    fn prove(&self, env: &Env, pk: &Env::ProvingKey, witness: Witness) -> Proof;
    fn verify(&self, env: &Env, vk: &Env::VerificationKey, proof: &Proof) -> bool;
}
