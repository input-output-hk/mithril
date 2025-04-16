(function() {
    var type_impls = Object.fromEntries([["mithril_common",[["<details class=\"toggle implementors-toggle\" open><summary><section id=\"impl-Clone-for-Ed25519Signer\" class=\"impl\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#29\">Source</a><a href=\"#impl-Clone-for-Ed25519Signer\" class=\"anchor\">§</a><h3 class=\"code-header\">impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.86.0/core/clone/trait.Clone.html\" title=\"trait core::clone::Clone\">Clone</a> for <a class=\"struct\" href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html\" title=\"struct mithril_common::crypto_helper::ed25519::Ed25519Signer\">Ed25519Signer</a></h3></section></summary><div class=\"impl-items\"><details class=\"toggle method-toggle\" open><summary><section id=\"method.clone\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#29\">Source</a><a href=\"#method.clone\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"https://doc.rust-lang.org/1.86.0/core/clone/trait.Clone.html#tymethod.clone\" class=\"fn\">clone</a>(&amp;self) -&gt; <a class=\"struct\" href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html\" title=\"struct mithril_common::crypto_helper::ed25519::Ed25519Signer\">Ed25519Signer</a></h4></section></summary><div class='docblock'>Returns a copy of the value. <a href=\"https://doc.rust-lang.org/1.86.0/core/clone/trait.Clone.html#tymethod.clone\">Read more</a></div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.clone_from\" class=\"method trait-impl\"><span class=\"rightside\"><span class=\"since\" title=\"Stable since Rust version 1.0.0\">1.0.0</span> · <a class=\"src\" href=\"https://doc.rust-lang.org/1.86.0/src/core/clone.rs.html#174\">Source</a></span><a href=\"#method.clone_from\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"https://doc.rust-lang.org/1.86.0/core/clone/trait.Clone.html#method.clone_from\" class=\"fn\">clone_from</a>(&amp;mut self, source: &amp;Self)</h4></section></summary><div class='docblock'>Performs copy-assignment from <code>source</code>. <a href=\"https://doc.rust-lang.org/1.86.0/core/clone/trait.Clone.html#method.clone_from\">Read more</a></div></details></div></details>","Clone","mithril_common::crypto_helper::ed25519_alias::genesis::ProtocolGenesisSigner","mithril_common::crypto_helper::ed25519_alias::era::EraMarkersSigner","mithril_common::crypto_helper::ed25519_alias::manifest::ManifestSigner"],["<details class=\"toggle implementors-toggle\" open><summary><section id=\"impl-Debug-for-Ed25519Signer\" class=\"impl\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#29\">Source</a><a href=\"#impl-Debug-for-Ed25519Signer\" class=\"anchor\">§</a><h3 class=\"code-header\">impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.86.0/core/fmt/trait.Debug.html\" title=\"trait core::fmt::Debug\">Debug</a> for <a class=\"struct\" href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html\" title=\"struct mithril_common::crypto_helper::ed25519::Ed25519Signer\">Ed25519Signer</a></h3></section></summary><div class=\"impl-items\"><details class=\"toggle method-toggle\" open><summary><section id=\"method.fmt\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#29\">Source</a><a href=\"#method.fmt\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"https://doc.rust-lang.org/1.86.0/core/fmt/trait.Debug.html#tymethod.fmt\" class=\"fn\">fmt</a>(&amp;self, f: &amp;mut <a class=\"struct\" href=\"https://doc.rust-lang.org/1.86.0/core/fmt/struct.Formatter.html\" title=\"struct core::fmt::Formatter\">Formatter</a>&lt;'_&gt;) -&gt; <a class=\"type\" href=\"https://doc.rust-lang.org/1.86.0/core/fmt/type.Result.html\" title=\"type core::fmt::Result\">Result</a></h4></section></summary><div class='docblock'>Formats the value using the given formatter. <a href=\"https://doc.rust-lang.org/1.86.0/core/fmt/trait.Debug.html#tymethod.fmt\">Read more</a></div></details></div></details>","Debug","mithril_common::crypto_helper::ed25519_alias::genesis::ProtocolGenesisSigner","mithril_common::crypto_helper::ed25519_alias::era::EraMarkersSigner","mithril_common::crypto_helper::ed25519_alias::manifest::ManifestSigner"],["<details class=\"toggle implementors-toggle\" open><summary><section id=\"impl-Deserialize%3C'de%3E-for-Ed25519Signer\" class=\"impl\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#29\">Source</a><a href=\"#impl-Deserialize%3C'de%3E-for-Ed25519Signer\" class=\"anchor\">§</a><h3 class=\"code-header\">impl&lt;'de&gt; <a class=\"trait\" href=\"https://docs.rs/serde/1.0.219/serde/de/trait.Deserialize.html\" title=\"trait serde::de::Deserialize\">Deserialize</a>&lt;'de&gt; for <a class=\"struct\" href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html\" title=\"struct mithril_common::crypto_helper::ed25519::Ed25519Signer\">Ed25519Signer</a></h3></section></summary><div class=\"impl-items\"><details class=\"toggle method-toggle\" open><summary><section id=\"method.deserialize\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#29\">Source</a><a href=\"#method.deserialize\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"https://docs.rs/serde/1.0.219/serde/de/trait.Deserialize.html#tymethod.deserialize\" class=\"fn\">deserialize</a>&lt;__D&gt;(__deserializer: __D) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.86.0/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;Self, __D::<a class=\"associatedtype\" href=\"https://docs.rs/serde/1.0.219/serde/de/trait.Deserializer.html#associatedtype.Error\" title=\"type serde::de::Deserializer::Error\">Error</a>&gt;<div class=\"where\">where\n    __D: <a class=\"trait\" href=\"https://docs.rs/serde/1.0.219/serde/de/trait.Deserializer.html\" title=\"trait serde::de::Deserializer\">Deserializer</a>&lt;'de&gt;,</div></h4></section></summary><div class='docblock'>Deserialize this value from the given Serde deserializer. <a href=\"https://docs.rs/serde/1.0.219/serde/de/trait.Deserialize.html#tymethod.deserialize\">Read more</a></div></details></div></details>","Deserialize<'de>","mithril_common::crypto_helper::ed25519_alias::genesis::ProtocolGenesisSigner","mithril_common::crypto_helper::ed25519_alias::era::EraMarkersSigner","mithril_common::crypto_helper::ed25519_alias::manifest::ManifestSigner"],["<details class=\"toggle implementors-toggle\" open><summary><section id=\"impl-Ed25519Signer\" class=\"impl\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#34-80\">Source</a><a href=\"#impl-Ed25519Signer\" class=\"anchor\">§</a><h3 class=\"code-header\">impl <a class=\"struct\" href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html\" title=\"struct mithril_common::crypto_helper::ed25519::Ed25519Signer\">Ed25519Signer</a></h3></section></summary><div class=\"impl-items\"><details class=\"toggle method-toggle\" open><summary><section id=\"method.create_test_signer\" class=\"method\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#36-42\">Source</a><h4 class=\"code-header\">pub fn <a href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html#tymethod.create_test_signer\" class=\"fn\">create_test_signer</a>&lt;R&gt;(rng: R) -&gt; Self<div class=\"where\">where\n    R: <a class=\"trait\" href=\"https://rust-random.github.io/rand/rand_core/trait.CryptoRng.html\" title=\"trait rand_core::CryptoRng\">CryptoRng</a> + <a class=\"trait\" href=\"https://rust-random.github.io/rand/rand_core/trait.RngCore.html\" title=\"trait rand_core::RngCore\">RngCore</a>,</div></h4></section></summary><div class=\"docblock\"><p><a href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html\" title=\"struct mithril_common::crypto_helper::ed25519::Ed25519Signer\">Ed25519Signer</a> factory</p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.create_deterministic_signer\" class=\"method\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#45-48\">Source</a><h4 class=\"code-header\">pub fn <a href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html#tymethod.create_deterministic_signer\" class=\"fn\">create_deterministic_signer</a>() -&gt; Self</h4></section></summary><div class=\"docblock\"><p><a href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html\" title=\"struct mithril_common::crypto_helper::ed25519::Ed25519Signer\">Ed25519Signer</a> deterministic</p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.create_non_deterministic_signer\" class=\"method\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#51-54\">Source</a><h4 class=\"code-header\">pub fn <a href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html#tymethod.create_non_deterministic_signer\" class=\"fn\">create_non_deterministic_signer</a>() -&gt; Self</h4></section></summary><div class=\"docblock\"><p><a href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html\" title=\"struct mithril_common::crypto_helper::ed25519::Ed25519Signer\">Ed25519Signer</a> non deterministic</p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.secret_key\" class=\"method\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#57-59\">Source</a><h4 class=\"code-header\">pub fn <a href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html#tymethod.secret_key\" class=\"fn\">secret_key</a>(&amp;self) -&gt; <a class=\"type\" href=\"mithril_common/crypto_helper/ed25519/type.Ed25519SecretKey.html\" title=\"type mithril_common::crypto_helper::ed25519::Ed25519SecretKey\">Ed25519SecretKey</a></h4></section></summary><div class=\"docblock\"><p>Get the <a href=\"mithril_common/crypto_helper/ed25519/type.Ed25519SecretKey.html\" title=\"type mithril_common::crypto_helper::ed25519::Ed25519SecretKey\">Ed25519SecretKey</a></p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.verification_key\" class=\"method\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#62-64\">Source</a><h4 class=\"code-header\">pub fn <a href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html#tymethod.verification_key\" class=\"fn\">verification_key</a>(&amp;self) -&gt; <a class=\"type\" href=\"mithril_common/crypto_helper/ed25519/type.Ed25519VerificationKey.html\" title=\"type mithril_common::crypto_helper::ed25519::Ed25519VerificationKey\">Ed25519VerificationKey</a></h4></section></summary><div class=\"docblock\"><p>Get the <a href=\"mithril_common/crypto_helper/ed25519/type.Ed25519VerificationKey.html\" title=\"type mithril_common::crypto_helper::ed25519::Ed25519VerificationKey\">Ed25519VerificationKey</a></p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.from_secret_key\" class=\"method\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#67-69\">Source</a><h4 class=\"code-header\">pub fn <a href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html#tymethod.from_secret_key\" class=\"fn\">from_secret_key</a>(secret_key: <a class=\"type\" href=\"mithril_common/crypto_helper/ed25519/type.Ed25519SecretKey.html\" title=\"type mithril_common::crypto_helper::ed25519::Ed25519SecretKey\">Ed25519SecretKey</a>) -&gt; Self</h4></section></summary><div class=\"docblock\"><p><a href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html\" title=\"struct mithril_common::crypto_helper::ed25519::Ed25519Signer\">Ed25519Signer</a> from <a href=\"mithril_common/crypto_helper/ed25519/type.Ed25519SecretKey.html\" title=\"type mithril_common::crypto_helper::ed25519::Ed25519SecretKey\">Ed25519SecretKey</a></p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.create_verifier\" class=\"method\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#72-74\">Source</a><h4 class=\"code-header\">pub fn <a href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html#tymethod.create_verifier\" class=\"fn\">create_verifier</a>(&amp;self) -&gt; <a class=\"struct\" href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Verifier.html\" title=\"struct mithril_common::crypto_helper::ed25519::Ed25519Verifier\">Ed25519Verifier</a></h4></section></summary><div class=\"docblock\"><p>Create a <a href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Verifier.html\" title=\"struct mithril_common::crypto_helper::ed25519::Ed25519Verifier\">Ed25519Verifier</a></p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.sign\" class=\"method\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#77-79\">Source</a><h4 class=\"code-header\">pub fn <a href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html#tymethod.sign\" class=\"fn\">sign</a>(&amp;self, message: &amp;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.86.0/std/primitive.u8.html\">u8</a>]) -&gt; <a class=\"type\" href=\"mithril_common/crypto_helper/ed25519/type.Ed25519Signature.html\" title=\"type mithril_common::crypto_helper::ed25519::Ed25519Signature\">Ed25519Signature</a></h4></section></summary><div class=\"docblock\"><p>Signs a message and returns a <a href=\"mithril_common/crypto_helper/ed25519/type.Ed25519Signature.html\" title=\"type mithril_common::crypto_helper::ed25519::Ed25519Signature\">Ed25519Signature</a></p>\n</div></details></div></details>",0,"mithril_common::crypto_helper::ed25519_alias::genesis::ProtocolGenesisSigner","mithril_common::crypto_helper::ed25519_alias::era::EraMarkersSigner","mithril_common::crypto_helper::ed25519_alias::manifest::ManifestSigner"],["<details class=\"toggle implementors-toggle\" open><summary><section id=\"impl-Serialize-for-Ed25519Signer\" class=\"impl\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#29\">Source</a><a href=\"#impl-Serialize-for-Ed25519Signer\" class=\"anchor\">§</a><h3 class=\"code-header\">impl <a class=\"trait\" href=\"https://docs.rs/serde/1.0.219/serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"mithril_common/crypto_helper/ed25519/struct.Ed25519Signer.html\" title=\"struct mithril_common::crypto_helper::ed25519::Ed25519Signer\">Ed25519Signer</a></h3></section></summary><div class=\"impl-items\"><details class=\"toggle method-toggle\" open><summary><section id=\"method.serialize\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/mithril_common/crypto_helper/ed25519.rs.html#29\">Source</a><a href=\"#method.serialize\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"https://docs.rs/serde/1.0.219/serde/ser/trait.Serialize.html#tymethod.serialize\" class=\"fn\">serialize</a>&lt;__S&gt;(&amp;self, __serializer: __S) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.86.0/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;__S::<a class=\"associatedtype\" href=\"https://docs.rs/serde/1.0.219/serde/ser/trait.Serializer.html#associatedtype.Ok\" title=\"type serde::ser::Serializer::Ok\">Ok</a>, __S::<a class=\"associatedtype\" href=\"https://docs.rs/serde/1.0.219/serde/ser/trait.Serializer.html#associatedtype.Error\" title=\"type serde::ser::Serializer::Error\">Error</a>&gt;<div class=\"where\">where\n    __S: <a class=\"trait\" href=\"https://docs.rs/serde/1.0.219/serde/ser/trait.Serializer.html\" title=\"trait serde::ser::Serializer\">Serializer</a>,</div></h4></section></summary><div class='docblock'>Serialize this value into the given Serde serializer. <a href=\"https://docs.rs/serde/1.0.219/serde/ser/trait.Serialize.html#tymethod.serialize\">Read more</a></div></details></div></details>","Serialize","mithril_common::crypto_helper::ed25519_alias::genesis::ProtocolGenesisSigner","mithril_common::crypto_helper::ed25519_alias::era::EraMarkersSigner","mithril_common::crypto_helper::ed25519_alias::manifest::ManifestSigner"]]]]);
    if (window.register_type_impls) {
        window.register_type_impls(type_impls);
    } else {
        window.pending_type_impls = type_impls;
    }
})()
//{"start":55,"fragment_lengths":[16462]}