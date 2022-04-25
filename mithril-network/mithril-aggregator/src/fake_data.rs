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
    party_id: u64,
    verification_key: String,
    secret_key: String,
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
            let signature = format!("SIG{}", signature_id).repeat(15);
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
  },
  {
    "party_id": 5,
    "stake": 522,
    "verification_key": "41184c652984afcf04fb62e7f7f5cc0e2e83b32e352d46a6242a9b508414f012097a1a2b47484a6700e13ba81c11da0059758c000803d5dee8e457001a132305869d0152ab6d4a3db58f44d84ee66a06577a5629630812cbb19b558d1714e100d04ab66c6de14c42abac8e870f2422592772b582f7ed1cf13dda06f1343cb5f2a7cfb6f56b402456c687a8781b47de00955cf6121b39757d5fc18316a5fee32c222f4bc7bdf082f72ce20f0356eeea8b117a083c55c956d3c63bdec07a77f900e2bb1d7d5d9503ce4cb00c3913dc6052132574dbad4006d5a44379659f36486fcf205b1d028a7297de6551e9a6534201fd3449ebe0812b03b3a2265b241a28d273698034f672c0cea1e074f47a15c1d5f84d05393ea5c4b9232744274abf35003bd8dacb0eda64bb6bd99516aed7ef144c2a42096b7c7f9d789a654965489cb66d6483bfb95e7cbde7e7606444764f008c92fb07be80daebe41afc37919e058fab66b3151e692c7de7a4834de37955fef78f880edfebdd9b3cc5bb27adbcac007adbbf80587b87ec2ac410c811efdf08b3420d7232cb362e1ecee2e21074a4eff294ba0c0c663aa2e6665dce4a22d500d93f933df2c46cbd0d9d66215a9c3b93922bdf9e8838eaa781cdc74ab5507199a245dc4261ced1b387e4054791f0370042d0504d1922175cecad60abb259a436c196b1de7b9607e5c1b330f0e98689cd20cf0d7f1acee0dc27d754bae7c63000483dc650b214634a9d8a409ff7024bb27ec99311ce50c80269a6986ec3764f349c13d8cd51f3cc2fea6a81a9ed694801",
    "secret_key": "ab6c5aebb2a95c378a59421349d61511a9e16a70f5173e7d380271a77b7efa03"
  },
  {
    "party_id": 6,
    "stake": 245,
    "verification_key": "960d8fb1cedfd9e464a821fab5f9e0dd850a5eb6c45fd7e2938d31a3a6cc8a9bd79ad19739df0e16557de538c266850134132ccd39f275ac4f0e57b40db9ecaf13f983d0f1e3c3b2401e71230e683e46a724f9502dd190a83c4ff95c7b7fee004fe3dbd9d15c0457e301641317c1a2468d5fcff193e9dfb54e03c6f0705fead32bb2abcc57934861edc26b63886e1c013cd1dfc07bb11314d564b2923c3f1523818de829fb64c6581291d0f9fbb7ce08f498dd870fdbd3b6bdfcf58c334aa300f283b25bbfae0d7f616befbe09a2134e6021a1f468fa46cb27c4f0646237d3b59ede0e24790ae0b25f899e3ee0865b01c1bc68a985a42fc5e86238fd266cce2ae10ee5df68cf3f3a9ee4154d63f546610563ecd8dfeb04a50cbe5ff4ee01620030626e15f19e733671981607ca907b6e2a3913dd1ef287fd32467d96c23ed0c994c790c467b5cc4f1f9d0fb826b982007c07e21e7a9d438a1d077b39c79f9aeea91735313c9e9152781faab75a510b902a3050df44420af658a11b720b256a01e8ae06e73052aed0b0662271e27cc1138e2cdc7fd5e3332d37b046c28af149ebe9c66e8dbf554d2c40cf72c6962f1100a315a8ec8d46730d08d892365086d1d65b43343e701e6a018e81d1b52192d9fc218eae728745e129d846281b03481e018918de28889a43ac28afb1b28b10fb48c2827bf0d97ba24eb4e946abc78192eb40f0423715310bf5c656c29454ddc90028b84f7585f8f51e58fbb1821d08fd2741b935570d18fc1296aa68e5d7eb6a9ced57fa0362ed03b785b814a461e9e400",
    "secret_key": "53859e0dce0fdf9b4329a4383d49b005bc38e3806ae5a5844dc9fa4641a8470d"
  },
  {
    "party_id": 7,
    "stake": 225,
    "verification_key": "c722ee0ba7cc4394e730826eee38ddfb102118a743f7c6582770340ba3591d6c0cd9010b513f477d83db926ff0475400847fe9836f2f8f9f883b45c928197b552f4a72291fbded850b5ed0a82eebc864419e7bf9a8f678f66242a957e104fa00dafd8e8e6d99971e5bc8ec1b1dc4ab584aa6126d16a1c32d1f00e705985314d22729f9b19a89cca04cd7edb9f8046d00912d34fa14bfd5b3c1992a803bb3f1904cc24c76617bcdad057107580a8a46348f21786806a51ffd7be482c09b443b015c2e02d547ea6c8a9924b080d9e10ca5c0f1fe06028ca97cc193ebfda63a79c852ac5da5ab6cddf15ba242a5fbc96b01446697dad659771338cea089c5e6417e059da7659d3506ab8f32921cbbaa3b068e9643da70f130273e71786af0387200005a61fc49bb8ae8dea4c372558a20da466c429e350c9a44e5d3dc90a97316a37d7b52a066a6cbd7a674e1cf6f373301508dd1535cb19a2baedaa3b826e0be879c52f503ffc828a2c77226711c8adebfa8398d625010b9a18ef0f393b9b1d5002a58d2a16f1f7475f9e9000959ab249adb13061b1d39a5fd396be9832372da7768ecdcf9718786a08c1cf9da1894d2002742a69ccd910b4c0bfd1513236108d45e5d48d12a6f1a506b785624ec39964349c396f4615239fbffc8262dfd74a00007fee67e2517fb22548d16cbffcded47222f9f1d04f878fcca976de4dcaf0b5c503d03845ace71772421c901915ae60036c36246e9bb5cced586627c90e000a35f4713388b3cb816098d303bd9d47bee415660526c599b022312726c5ba9d300",
    "secret_key": "410475a7911d9138d01ad662e2082a237c8777c91d3d6276cffff19817f56b10"
  },
  {
    "party_id": 8,
    "stake": 872,
    "verification_key": "1895529a815100ba2518ef3de25aa232c50b2cbb4d74242dc1b02bc37f31a811b984eae55e42150003d02856aef65000643abc2f88e195ffa4300e37b696dd0e76f7509548b38365d9d9050ad019ca8eb3699f1f9127233b80bb6c33910a55014443aafc2e625b4226017b7d0fc30fd0764a8b0e6fffffb41c7ce238b25afc8a7e436d758eb5ae1e7a867b28be759600017999118d3a0308142d9a2928d3ff199a9317f81d9e3c8740b515c19378a4c66ce9cc9d518cfa9baa22233fbb6714003f043df8fbe52bc737d2fe2f1695cc293a26b370285f9ce040a43a4a8a2cbd4844d96dd70e29cbab00644a0816106a017b16b2c51704267a871dd9289a4f93e43c1035afd70a8ea7ba5ffe8514504fde00b30f81ea35272aef52692e09949501eb0c069887102a7c19915bef9772cf1eed651e91afecb92a48a7c5277cbc138661a7cf239a1b03957827cdb199a68d01a29823b1b6097e6753e8d26eecac305d8aec19157eaee86f8807b5fa0fa0d9395820ff2d596710be106848ec5daa4100fc7484c1e86ef25b702cb0fc5506287abbfe04836117bf9861ec85f3247e2f2c8196faed8e2c6799790ac8e909e7a9019052c918bba7461db636434ee659622a9c486587207eb4edfe384bc32bbb2caa761b6ed422854d4d849b9f1cf7b65c01f4cae712371150356a216b8e4e148157023c2501b10b9a187f1f9eca9623cb50bc84f8459076f7caad58f26d5fa99200fd481beab9a19f5039b672a11f82cb8aa2e3c874be6327824bc9796616baae7e3e746bc23aa8f57d61b51193da8c1801",
    "secret_key": "c2734a816e10d04a24ee0dc55c88cca99acd626638b0e969d78a0f8458b6ab03"
  },
  {
    "party_id": 9,
    "stake": 249,
    "verification_key": "f4e74b638c661fa6271faff07df5e34179a407672a2cfd1b1c5dabe55a2c5956a535c7fe86a796b9ee8532166ac49a000fba5a5045e189ebbb5c959329267642eea69051c58f2844da23a869921060483586eabbe969a50d248f68bb71dad000c4dc7a21aa62f4ec4ada69049a8d7880b927d9c096c63a9b72d21c75f22034e01d88ed2169211990696833a9bd68e1009c5d2a64522ab26bb1edd157344b66adb80ae0aecd5f423d40c95ef385a8190007b8224400385a53a46cdc9ab69b8001a0775b98d0027a7ee0a42490a655325c5c03c7d94b5963efb66126f4716a593771b8745338f3c89934132970ebb583006b8d966392e44d1ee20b52af97983d84f29afc002d9e9bcb39336f97f7bd2a56c1553f35c641b417871d51b198ea0500f5495d8418d7eab15e4caa5f17bef9890dfdee835c8b11af3f5699de24f02c9ca997d581116eb9740a02dce3f52af200b70510b085b88bf5d1e800969f1a77e901e83803fadc0d3dae60bd488ab1d47b359cb142dcf2ab7a86c91b27eebade0085e0997e57ece989110db79b86f23404df0ed4db338f51e42b5caa7613befe32c2917a68e546b541ffe9c575c3a06c006d46b0c825672f1411ad84baed2d5803c0fd9e322941664fb1e1a45ea93edf23a472bf885f64609509dfc94872b49600032ce7b396775884006527a076d8b43a18d684637bb7ba1cb1e5ac361604a36e120fca32751406ce375bda6b20126b0082351bcd7baed7309ac6e7e4bcf62735238e00a804c8b8902adb92544b3dee348352f3da7a332e78bff53982d6334f00",
    "secret_key": "469c574ec1713acfef1d502b2e77a32df7ff275e2201b9384906794dd1232002"
  }
]
"###;
