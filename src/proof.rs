//! General API for producing proofs from statements and witnesses

pub trait ProverEnv {
    type ProvingKey;
    type VerificationKey;

    fn setup(&self) -> (Self::ProvingKey, Self::VerificationKey);
}

pub trait Provable<Env, Relation, Witness, Proof>
where
    Env: ProverEnv,
{
    fn prove(&self, env: &Env, pk: &Env::ProvingKey, rel: &Relation, witness: Witness) -> Proof;
    fn verify(&self, env: &Env, vk: &Env::VerificationKey, rel: &Relation, proof: &Proof) -> bool;
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
    pub struct Trivial;
    pub struct TrivialStmt<S>(S);

    pub struct TrivialProof<W>(W);

    impl<Env, Stmt, R, Witness> Provable<Env, R, Witness, Witness> for Stmt
    where
        Env: ProverEnv,
        R: Fn(&Stmt, &Witness) -> bool,
    {
        fn prove(&self, _env: &Env, _pk: &Env::ProvingKey, _rel: &R, witness: Witness) -> Witness {
            witness
        }

        fn verify(&self, _env: &Env, _vk: &Env::VerificationKey, rel: &R, proof: &Witness) -> bool {
            rel(&self, proof)
        }
    }

    pub struct TrivialEnv;

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
