use crate::crypto_helper::ProtocolParameters;

/// Extension trait adding test utilities to [ProtocolInitializer][crate::crypto_helper::ProtocolInitializer]
pub trait ProtocolInitializerTestExtension {
    /// `TEST ONLY` - Override the protocol parameters of the `Initializer`
    fn override_protocol_parameters(&mut self, protocol_parameters: &ProtocolParameters);
}
