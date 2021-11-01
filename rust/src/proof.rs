//! General API for producing proofs from statements and witnesses
use std::fmt::Debug;

/// An environment or context that can contain any long-lived information
/// relevant to the proof backend
pub trait ProverEnv {
    /// The secret key used to create proofs.
    type ProvingKey;
    /// The public key used to verify proofs.
    type VerificationKey;
    /// Create a new key pair.
    fn setup(&self) -> (Self::ProvingKey, Self::VerificationKey);
}

/// Implementors of `Proof` know how to prove that a relation of type `Relation`
/// holds between values of types `Statement` and `Witness` (generally the
/// proofs are knowledge of such a `Witness`)
pub trait Proof: Sized {
    /// Context for this proof system.
    type Env: ProverEnv;
    /// Statement to be proven.
    type Statement;
    /// Relation between values of type Statement and type Witness.
    type Relation;
    /// Witness of the validity of Statement.
    type Witness;
    /// Type of errors which can be output by the proof system.
    /// Into<i64> allows passing the error to the C API.
    type Error: Debug + Into<i64>;

    /// Construct a proof.
    fn prove(
        env: &Self::Env,
        pk: &<Self::Env as ProverEnv>::ProvingKey,
        rel: &Self::Relation,
        stmt: &Self::Statement,
        witness: Self::Witness,
    ) -> Result<Self, Self::Error>;

    /// Verify a proof.
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

    /// Trivial environment which contains nothing.
    #[derive(Debug, Clone)]
    pub struct TrivialEnv;

    /// TrivialProof simply contains the Witness directly.
    #[derive(Clone)]
    pub struct TrivialProof<S, R, W> {
        /// The witness itself.
        pub witness: W,
        pr: PhantomData<R>,
        ps: PhantomData<S>,
    }

    impl<S, R, W> TrivialProof<S, R, W> {
        /// Create a new TrivialProof from the witness.
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

    /// Error wrapper for TrivialProof.
    #[derive(Debug)]
    pub struct TrivialError<Error: Debug + Into<i64>>(Error);

    #[allow(clippy::from_over_into)]
    impl<Error: Debug + Into<i64>> Into<i64> for TrivialError<Error> {
        fn into(self) -> i64 {
            self.0.into()
        }
    }

    impl<Stmt, R, Witness, UnderlyingError> Proof for TrivialProof<Stmt, R, Witness>
    where
        R: Fn(&Stmt, &Witness) -> Result<(), UnderlyingError>,
        UnderlyingError: Debug + Into<i64>,
    {
        type Env = TrivialEnv;
        type Statement = Stmt;
        type Relation = R;
        type Witness = Witness;
        type Error = TrivialError<UnderlyingError>;
        fn prove(
            _env: &TrivialEnv,
            _pk: &(),
            rel: &Self::Relation,
            stmt: &Stmt,
            witness: Self::Witness,
        ) -> Result<Self, Self::Error> {
            if let Err(e) = rel(stmt, &witness) {
                Err(TrivialError(e))
            } else {
                Ok(TrivialProof::new(witness))
            }
        }

        fn verify(
            &self,
            _env: &TrivialEnv,
            _vk: &(),
            rel: &R,
            statement: &Stmt,
        ) -> Result<(), Self::Error> {
            if let Err(e) = rel(statement, &self.witness) {
                Err(TrivialError(e))
            } else {
                Ok(())
            }
        }
    }
}
