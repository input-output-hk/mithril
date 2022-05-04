#![allow(dead_code)]

use serde::{Deserialize, Serialize};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::entities;

/// Fake Beacon
pub fn beacon() -> entities::Beacon {
    let network = "testnet".to_string();
    let seconds_since_unix_epoch = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();
    let epoch = seconds_since_unix_epoch / 86400_u64; // 1 epoch every day
    let block = 100 * (seconds_since_unix_epoch / (5 * 100)) as u64; // 1 block every 5s and 1 snapshot every 100 blocks
    entities::Beacon::new(network, epoch, block)
}

/// Fake Digest
pub fn digest() -> Vec<u8> {
    let beacon = beacon();
    format!(
        "digest-{}-{}-{}",
        beacon.network, beacon.epoch, beacon.block
    )
    .as_bytes()
    .to_vec()
}

/// Fake ProtocolParameters
pub fn protocol_parameters() -> entities::ProtocolParameters {
    let k = 5;
    let m = 100;
    let phi_f = 0.65;
    entities::ProtocolParameters::new(k, m, phi_f)
}

/// Fake CertificatePending
pub fn certificate_pending() -> entities::CertificatePending {
    // Beacon
    let beacon = beacon();

    // Protocol parameters
    let protocol_parameters = protocol_parameters();

    // Previous hash
    let previous_hash = "123".to_string();

    // Signers with stakes
    let signers = signers_with_stakes(5);

    // Certificate pending
    entities::CertificatePending::new(beacon, protocol_parameters, previous_hash, signers)
}

/// Fake Certificate
pub fn certificate(certificate_hash: String) -> entities::Certificate {
    // Beacon
    let beacon = beacon();

    // Protocol parameters
    let protocol_parameters = protocol_parameters();

    // Signers with stakes
    let signers = signers_with_stakes(5);

    // Certificate
    let previous_hash = format!("{}0", certificate_hash);
    let block = beacon.block;
    let digest = format!("1{}", block).repeat(20);
    let started_at = "2006-01-02T15:04:05Z".to_string();
    let completed_at = "2006-01-02T15:04:05Z".to_string();
    let multisignature = format!("ABC{}", block).repeat(200);
    entities::Certificate::new(
        certificate_hash,
        previous_hash,
        block,
        protocol_parameters,
        digest,
        started_at,
        completed_at,
        signers,
        multisignature,
    )
}

/// Fake SignersWithStake
pub fn signers_with_stakes(total: u64) -> Vec<entities::SignerWithStake> {
    let total = total as usize;
    let signers_with_stakes =
        serde_json::from_str::<Vec<entities::SignerWithStake>>(SIGNERS_WITH_STAKE_JSON).unwrap();
    assert!(signers_with_stakes.len() >= total);
    signers_with_stakes[0..total].to_vec()
}

/// Fake Signers
pub fn signers(total: u64) -> Vec<entities::Signer> {
    signers_with_stakes(total)
        .iter()
        .map(|signer| entities::Signer::new(signer.party_id, signer.verification_key.clone()))
        .collect::<Vec<entities::Signer>>()
}

/// SignerWithSecretKeys represents a signer with verification/secret keys
// TODO: To delete once key registration is implemented
#[derive(Serialize, Deserialize, Clone)]
pub struct SignerWithSecretKeys {
    pub party_id: u64,
    pub stake: u64,
    pub verification_key: String,
    pub secret_key: String,
}

/// Fake SignerKeys returns Verification/Secret keys for a party_id
// TODO: To delete once key registration is implemented
pub fn signer_keys(party_id: u64) -> Option<SignerWithSecretKeys> {
    let signers_with_secret_keys =
        serde_json::from_str::<Vec<SignerWithSecretKeys>>(SIGNERS_WITH_STAKE_JSON).unwrap();
    let signer_keys = signers_with_secret_keys
        .iter()
        .filter(|signer| signer.party_id == party_id)
        .collect::<Vec<&SignerWithSecretKeys>>();
    Some((*(signer_keys.first()?)).clone())
}

/// Fake SingleSignatures
pub fn single_signatures(total: u64) -> Vec<entities::SingleSignature> {
    (1..total + 1)
        .map(|signature_id| {
            let party_id = signature_id;
            let index = signature_id + 100;
            let signature = "341f48eff51b822177a33de435b71cb8526b279f6f0030a39fec56869a15e5483301c51d04938fe8d9bff350863e6100d4aa24a5fe9f9a9b787dae8aaf8ee1d3d7a32a0f78030050f1fde1a681bdc2267a89a2d45b4d5b77f0cc124c5c790f005d76b3050d0515feb32ffeb52437273038cdc32fa22fd707f47f667225e1413738b048edf914736048b63a64cb0c64004a3a70ba31a53473160070a0b0abb90449316f1ce7d91cbea45b8b9e85e282ec52e88ec00b059fff544a1b784eca99003f3a7f7fb5d89884c664a5d37184fdbd72fbce96110743fa5895d656df8c7bb1c9f3e836438322040f65f7dc441a5e0040a30189a54a8911f50e2cc99ad7fb47dc115831dff4a289e4fa69d28884cfa8cb96fee1a8816912e04642c829116c012b1f253e0a3fa2c9eaf5655ed2c5cb56a83cbbcc7f9514ba4f4b1bd216f6ea3c5d5842207fbf2597864969f65f529c01711832d836cbda38f62e158d8b6f11c5aa4c2ea37f8184925b82561f77d250eb381e4fc2ea2d27071a9943df152791019a1715d0b82a3d8ae9130f445f2027612c91a8a0f66eabcc5d7ca511d1322649a6dffc524ec246fd7e68bb3d4edc43005329993dc654fe710df179c9be6da708da90a97622422b03bfd00ec2be2bba0be3442469837056e4c646d2bcf5707e003ea148bed7ad18e807031b9c96b64e9d9a2e9ef0086fbfddc4ff486164e95f004a381bd9e772c75433838d4f2d07a4011fb06cabb781166aaabb9afbc881cbfbda00b9081792fa0c1d9937476de1758f98d194cb3b912118d6eb9fd3e1bf210030a04132bf2c1ae7948fa0fc05677b5284eb513f42ba114bc7f822f4315f219d80c18c4b61a7112fa3be828a99ae1e01f161ed6af2ef4d3aea01c207d935efc1b736bfae68078e94818419943ed4bf6c3434f94c6280eb0f30e57859f43b7900936047e88bece6e42fbbb8053156c1106af7b927b28b2e14f2c7053be695ab19f6bb0656d54f5782fa6c7020aa36d20000000000000000003a03000000000000030000000000000028010000000000007179564d6c001d0a2f4cc240c25786807c668db8540d30984286df64e85bf5292ee4e549a4ef6bbc536b4cf395d74e019fc81092e350c3e2544044f21895be8707e383739bb004fd444a98e44085d762402f75b9f17e1ac3d213ca819fd57800c80e452c079d67594f0966c2f24498989ef0c24942a639ff1822f7304483294b658de590347dc7756f7c1f0a7c7ccf00246998e5a236d973de2000c3309cb1ab59daca91b89af59ef58435ce4991b7c87736613b29e2f56d51ca55c51478620030e6595fef39fd57d6d2a512254389e23aaa948798f34eab39f6816513b195263007ec12e4082a5775bbbd1ea65b300027030928835692d94772a40ef7d3f043d705608fa01a7381a5dd69b2ca8db00437066e14e8ea70471c0cd11917889d00e5020000000000004000000000000000a8e14b8b78cf6f689c2de87db8d0ca3d910e5ac86a9816f48840b265a7b0aaf7421c971adaab4a59fcbaa435515c0bcbb71f199d54c809811dea4f340a394ac84000000000000000addf62d30bc3d5acb8c65e75005f3a905d2731ae26e7fbee23426445ddc6221bba1492c125f8f144cfbe7314c9245fa871732fad3ed669b01368480cb53bc581".to_string();
            entities::SingleSignature::new(party_id, index, signature)
        })
        .collect::<Vec<entities::SingleSignature>>()
}

/// Fake Snapshots
pub fn snapshots(total: u64) -> Vec<entities::Snapshot> {
    (1..total + 1)
        .map(|snapshot_id| {
            let digest = format!("1{}", snapshot_id).repeat(20);
            let certificate_hash = "123".to_string();
            let size = snapshot_id * 100000;
            let created_at = "2006-01-02T15:04:05Z".to_string();
            let mut locations = Vec::new();
            locations.push(format!("http://{}", certificate_hash));
            locations.push(format!("http2://{}", certificate_hash));
            entities::Snapshot::new(digest, certificate_hash, size, created_at, locations)
        })
        .collect::<Vec<entities::Snapshot>>()
}

const SIGNERS_WITH_STAKE_JSON: &str = r###"
[
  {
    "party_id": 0,
    "stake": 826,
    "verification_key": "4a3a70ba31a53473160070a0b0abb90449316f1ce7d91cbea45b8b9e85e282ec52e88ec00b059fff544a1b784eca99003f3a7f7fb5d89884c664a5d37184fdbd72fbce96110743fa5895d656df8c7bb1c9f3e836438322040f65f7dc441a5e0040a30189a54a8911f50e2cc99ad7fb47dc115831dff4a289e4fa69d28884cfa8cb96fee1a8816912e04642c829116c012b1f253e0a3fa2c9eaf5655ed2c5cb56a83cbbcc7f9514ba4f4b1bd216f6ea3c5d5842207fbf2597864969f65f529c01711832d836cbda38f62e158d8b6f11c5aa4c2ea37f8184925b82561f77d250eb381e4fc2ea2d27071a9943df152791019a1715d0b82a3d8ae9130f445f2027612c91a8a0f66eabcc5d7ca511d1322649a6dffc524ec246fd7e68bb3d4edc43005329993dc654fe710df179c9be6da708da90a97622422b03bfd00ec2be2bba0be3442469837056e4c646d2bcf5707e003ea148bed7ad18e807031b9c96b64e9d9a2e9ef0086fbfddc4ff486164e95f004a381bd9e772c75433838d4f2d07a4011fb06cabb781166aaabb9afbc881cbfbda00b9081792fa0c1d9937476de1758f98d194cb3b912118d6eb9fd3e1bf210030a04132bf2c1ae7948fa0fc05677b5284eb513f42ba114bc7f822f4315f219d80c18c4b61a7112fa3be828a99ae1e01f161ed6af2ef4d3aea01c207d935efc1b736bfae68078e94818419943ed4bf6c3434f94c6280eb0f30e57859f43b7900936047e88bece6e42fbbb8053156c1106af7b927b28b2e14f2c7053be695ab19f6bb0656d54f5782fa6c7020aa36d200",
    "secret_key": "cb34961d524b9bb0988e343370c6462b6f265bca5e61b83d25c8f41efc8a9f02"
  },
  {
    "party_id": 1,
    "stake": 741,
    "verification_key": "7179564d6c001d0a2f4cc240c25786807c668db8540d30984286df64e85bf5292ee4e549a4ef6bbc536b4cf395d74e019fc81092e350c3e2544044f21895be8707e383739bb004fd444a98e44085d762402f75b9f17e1ac3d213ca819fd57800c80e452c079d67594f0966c2f24498989ef0c24942a639ff1822f7304483294b658de590347dc7756f7c1f0a7c7ccf00246998e5a236d973de2000c3309cb1ab59daca91b89af59ef58435ce4991b7c87736613b29e2f56d51ca55c51478620030e6595fef39fd57d6d2a512254389e23aaa948798f34eab39f6816513b195263007ec12e4082a5775bbbd1ea65b300027030928835692d94772a40ef7d3f043d705608fa01a7381a5dd69b2ca8db00437066e14e8ea70471c0cd11917889d00ae6568a9d8ede8d0dd1241c7e260a8dac1c4aeed0de9e124dc54f6ba415b5eaf8a0d67ab32277071004a9e56f9ea6e01fb4d7cb7ff1e2cf521cedad717761b6e4fae40ef6a9cbb86f3fd1f5e3306c55348773235901d7a057a910a0174345101578859c5d035b319e955f6dd6d163c5a33344a4dd3be18b220cb06e4bd944a109a54e355d729517a8ae40f6f53fd5101675acb1e6e6ebd762854864c859f76e3626c56c67b7d015c038b1a29f82ae78a4fb6f5fd13a22be60d3b6028ae439001caa1b47d51216c94bfad253d4605dac5e948d370acf63cb6351f4b94e875abd3585204368b31ae9634369477f22b76000cb6554aa7b6b32ef8c3562874268068ab8422f6aa1ea8a0c67374cdb5f58b1a4131bd666754ae1680c3a08352947101",
    "secret_key": "a7f804600ddb5e1b5877533610fcc0138266f231883d7ad4e9e4e902adf27912"
  },
  {
    "party_id": 2,
    "stake": 144,
    "verification_key": "cbba6330cfae423b573a502b3c96c7f8b8479e998e16be8ab731c46c2d32529a6f381e39586bf44596670fbe172c9301de39b691aea91c2cdf2f0fad63ef621651aa9bb70e9d5376bb00fc98d1b5f4dd39a99f8fcf83f591aa447d1a54935a005203800f70dcacd2bc8a708c03281e674e911cbfbe26bd572bd95900b5e25172ca0eae1cf9003e198fa91cbf49261a00feaf97b6c92571d4b7e9da3e9ba3f27ee2a84c2eca0da76ecf4be65a5bce7033cb15e63396898d2cee5df1fc97447c005e87d824d961c9db5d65bdd97a70343dada57b5501d408a4f650c812718cb79ffc24a6b8c613b7d5915898111e1fa701ccaeaf54b0b7d24c593012fea7e3e2d4170886d80b71af58c1b575d11f7e77dd3734c6aa5e897462ed8ffcf237d97d00db0a4e7071fb5ee74d711e68c8a8342b7b0ebef9ff055d1cd6b73e179db067ad306898657f7293e137d6178c33303e00afd87346a8f6e3127f9c5fe552d3e52a8efd1e24fd8cf7b8b5174e92d4438bbe62f5435501a727826789b1ee77230e0086d4fb2e0b75cce5ce8689f74de7afa2e48c82d695d3426dbaf7e044245ee1e6500320f66e383fced97d2ea07f9b13000f9ce3659ff99f34399c7671484c9cdbc608c73992b9712ab501aaa997c7e09fcb2e4fb820ea6ad0796a97eea7d0af0077b4d4de979d4c28ca51f07c28f22df97d01d581c3ab321fe98e72d8b8786f08f4c45cd244fae864d4ee75f08d029d01c78d3ebc12a5a3027b14d93815f155b7871248772b7816d58798db2561bad223d38033cf9ea829a2f39baf08b0407600",
    "secret_key": "6380b5fdbcd1903a103f4e246e8e186b8e1a7802b1dd43d5de2ad1f590bf4d00"
  },
  {
    "party_id": 3,
    "stake": 734,
    "verification_key": "de6786178a21dcd6ec7ef3fd2144dd5d2020b3963b9c9b0c8da411f226087f1dfe914dc998ceac8838ad254b2895ce001c91544e1ee4ed2fc3fff54aa480b82dd1e1fbc049930518c5dc378d53c1c146082789fccf9ce80605a79a0b95961b008ca19caa688ecdebc7cf1a49400cd28cb414f125f242817730382cfc0e0c0ce75706c618fc0bb3ad31e5450e3d5ace0056d4fa7a942e377bad6acfc8c2e1fe4f2d95a596ca8dd6ad2eb2d815c643c31f526b6da32f6b5ac6717db881649ac600c79d0e408125e77e9ced43543cf4eef5bea23565c342e567e72f35b9fab5985231436566a7370c8dcebfb25244314100086c8c4983e22629fb0f1a10fadffeeb0abe935891d0f9f3dc65317d2f1b33082052af79fd5c7d24ccdf7f28ae70790060d4dc43776825dca39449ec73804679b5253267cfbd1aaa4ec0ccdbf422a4716e2d7420bf0b125b54e0ff6f34afa900504308abca027a34fbc7667a456b6fa7fcac72291f6616907a16ae3579677438cbb484827116260e067a3beba982df000fb18f2cc499c941bbba1c2c5aa8ed67776df58807ac01ab9b3ea7341246eebec4faf30350482c856b0ecd46cbff4601d4f0b875941b314c10f57bc382c037fc6d82a31ac4ccd6279b1d5b1ba7547368430a1ad500e65d347ed00761ae245b000e484b37567c2af53a73a6ec217294ff06bba1340ddeacca30b8cadc24b11de38d58b826037bbb14df9641d903dc37007057ee6a83f4e21b918381bee298b1a29db22a46d3c37f13f076d0be8c69d3bcb83d778f4a2b6fcb486131e3213e9600",
    "secret_key": "41aea751b95b4d116ba0b3edabc6dc63efc2485c6ee807c494e608d9198c9003"
  },
  {
    "party_id": 4,
    "stake": 41,
    "verification_key": "8629eaf86e956db0b96f721ebfdddc8fb7c1d2d57873373552823bfc4dee8903e65afe75dc17cc02aa377add28f8d00047fccd405e2fa761da7ba38e3f96287f14bc18db7704551ae83d9283ae17fe6fdd1fc6b09d24779f4cf06b8fcc4ada00994c960e8ecc92457fd8aca47c0f6e599e7bea43b64a0fd65817f7ed52661d5dbce53050ad098a82552243bd91468000c1b2d07f23eec1fe95f5d164d0e0d3e453cf03280572611d7109de32bc88e8ee4f8ea828aced266e0c6c865afb3ead019f5acca3c53ace7b0346d80e59e493b0a8dc3062130fbfabd0745089b452c35c0e07dcf081c96c1c9c84370f839f1a00e5845c3d15403976048e3c155921afd54598d9f267f7172bfcfd6ae4c68fd9368e1f601a7993e89ac60d67fbd777890150c2b27315c5ab6b697c6ea0e91173ea18efd7de693f0fc7d2c1f5884df90f04b4a915af50745765de3f76de896a8801d515e08ba1b2fbfa157abd0b918165420a5832d54943b1823f6901beed36680ec249c555aee5264c37699e5354db5e00a3586076e38d5f6716c11fd640e3f3043af2dfb98f96b14277de3dc59b8ec15fc3a7c0b4b998ae9309f88927d0bb9d01180c7dde67993477e7e5584316fe1725e897f7ad92b138beebb944f3e44a98a20bac2e738e39c8d954373413094909008dc1582263cbafd827410373c7ea01f5a3335e3de9429301e206529e8cc3f4a25340cc61ce0f552a57b9d7065bc71f00e1a524ca8a239169a632dd7e4addc47939d68cf77806322c7dad74f423c80461ed6899867f9e8573c45aa45f61e6b800",
    "secret_key": "b607d04c458f601dbb1d30aa5002375a679b7a60faa49aced01b967cc2013402"
  }
]
"###;
