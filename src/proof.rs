//! General API for producing proofs from statements and witnesses

pub trait ProverEnv {
    type ProvingKey;
    type VerificationKey;

    fn setup(&self) -> (Self::ProvingKey, Self::VerificationKey);
}

pub trait Proof<Env, Statement, Relation, Witness>
where
    Env: ProverEnv,
{
    fn prove(env: &Env, pk: &Env::ProvingKey, rel: &Relation, witness: Witness) -> Self;
    fn verify(
        &self,
        env: &Env,
        vk: &Env::VerificationKey,
        rel: &Relation,
        stmt: &Statement,
    ) -> bool;
}

pub mod trivial {
    use super::*;

    pub struct TrivialEnv;
    pub struct TrivialProof<W>(W);

    impl ProverEnv for TrivialEnv {
        type VerificationKey = ();
        type ProvingKey = ();
        fn setup(&self) -> ((), ()) {
            ((), ())
        }
    }

    impl<Stmt, R, Witness> Proof<TrivialEnv, Stmt, R, Witness> for TrivialProof<Witness>
    where
        R: Fn(&Stmt, &Witness) -> bool,
    {
        fn prove(env: &TrivialEnv, _pk: &(), rel: &R, witness: Witness) -> Self {
            TrivialProof(witness)
        }

        fn verify(&self, env: &TrivialEnv, vk: &(), rel: &R, statement: &Stmt) -> bool {
            rel(statement, &self.0)
        }
    }
}
