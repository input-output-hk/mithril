//! General API for producing proofs from statements and witnesses
use std::fmt::Debug;

/// An environment or context that can contain any long-lived information
/// relevant to the proof backend
pub trait ProverEnv {
    type ProvingKey;
    type VerificationKey;

    fn setup(&self) -> (Self::ProvingKey, Self::VerificationKey);
}

/// Implementors of `Proof` know how to prove that a relation of type `Relation`
/// holds between values of types `Statement` and `Witness` (generally the
/// proofs are knowledge of such a `Witness`)
pub trait Proof: Sized {
    type Env: ProverEnv;
    type Statement;
    type Relation;
    type Witness;
    type Error: Debug;

    fn prove(
        env: &Self::Env,
        pk: &<Self::Env as ProverEnv>::ProvingKey,
        rel: &Self::Relation,
        stmt: &Self::Statement,
        witness: Self::Witness,
    ) -> Result<Self, Self::Error>;

    fn verify(
        &self,
        env: &Self::Env,
        vk: &<Self::Env as ProverEnv>::VerificationKey,
        rel: &Self::Relation,
        stmt: &Self::Statement,
    ) -> Result<(), Self::Error>;
}

pub mod trivial {
    //! A trivial implementation of `Proof` where proofs of knowledge of
    //! witnesses are just the witnesses themselves.
    use super::*;
    use std::fmt::{Debug, Formatter, Result as FmtResult};
    use std::marker::PhantomData;

    #[derive(Debug, Clone)]
    pub struct TrivialEnv;

    #[derive(Clone)]
    pub struct TrivialProof<S, R, W> {
        pub witness: W,
        pr: PhantomData<R>,
        ps: PhantomData<S>,
    }

    impl<S, R, W> TrivialProof<S, R, W> {
        pub fn new(witness: W) -> Self {
            Self {
                witness,
                pr: PhantomData,
                ps: PhantomData,
            }
        }
    }

    impl<S, R, W: Debug> Debug for TrivialProof<S, R, W> {
        fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
            write!(f, "TrivialProof({:?})", self.witness)
        }
    }

    impl ProverEnv for TrivialEnv {
        type VerificationKey = ();
        type ProvingKey = ();
        fn setup(&self) -> ((), ()) {
            ((), ())
        }
    }

    #[derive(Debug)]
    pub enum TrivialErr {
        NotProvable,
    }

    impl<Stmt, R, Witness> Proof for TrivialProof<Stmt, R, Witness>
    where
        R: Fn(&Stmt, &Witness) -> bool,
    {
        type Env = TrivialEnv;
        type Statement = Stmt;
        type Relation = R;
        type Witness = Witness;
        type Error = TrivialErr;
        fn prove(
            env: &TrivialEnv,
            _pk: &(),
            rel: &Self::Relation,
            stmt: &Stmt,
            witness: Self::Witness,
        ) -> Result<Self, TrivialErr> {
            if rel(stmt, &witness) {
                Ok(TrivialProof::new(witness))
            } else {
                Err(TrivialErr::NotProvable)
            }
        }

        fn verify(
            &self,
            env: &TrivialEnv,
            vk: &(),
            rel: &R,
            statement: &Stmt,
        ) -> Result<(), TrivialErr> {
            if rel(statement, &self.witness) {
                Ok(())
            } else {
                Err(TrivialErr::NotProvable)
            }
        }
    }
}
