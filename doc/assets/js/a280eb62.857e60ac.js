"use strict";(self.webpackChunkmithril_doc=self.webpackChunkmithril_doc||[]).push([[4899],{98475:(e,t,i)=>{i.r(t),i.d(t,{assets:()=>c,contentTitle:()=>o,default:()=>d,frontMatter:()=>s,metadata:()=>n,toc:()=>h});const n=JSON.parse('{"id":"mithril/advanced/mithril-certification/cardano-transactions","title":"Cardano transactions","description":"The Mithril protocol supports the certification of the full Cardano transactions set (since genesis). This allows users to verify a transaction\'s authenticity without downloading the entire Cardano blockchain.","source":"@site/root/mithril/advanced/mithril-certification/cardano-transactions.md","sourceDirName":"mithril/advanced/mithril-certification","slug":"/mithril/advanced/mithril-certification/cardano-transactions","permalink":"/doc/next/mithril/advanced/mithril-certification/cardano-transactions","draft":false,"unlisted":false,"editUrl":"https://github.com/input-output-hk/mithril/edit/main/docs/website/root/mithril/advanced/mithril-certification/cardano-transactions.md","tags":[],"version":"current","sidebarPosition":1,"frontMatter":{"sidebar_position":1,"sidebar_label":"Cardano transactions"},"sidebar":"mithrilSideBar","previous":{"title":"Mithril certification","permalink":"/doc/next/mithril/advanced/mithril-certification/"},"next":{"title":"Cardano stake distribution","permalink":"/doc/next/mithril/advanced/mithril-certification/cardano-stake-distribution"}}');var a=i(74848),r=i(28453);const s={sidebar_position:1,sidebar_label:"Cardano transactions"},o="Cardano transactions",c={},h=[{value:"Mithril certification",id:"mithril-certification",level:2},{value:"Message computation",id:"message-computation",level:3},{value:"Authenticity verification",id:"authenticity-verification",level:3}];function l(e){const t={a:"a",admonition:"admonition",code:"code",em:"em",h1:"h1",h2:"h2",h3:"h3",header:"header",img:"img",li:"li",p:"p",strong:"strong",ul:"ul",...(0,r.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(t.header,{children:(0,a.jsx)(t.h1,{id:"cardano-transactions",children:"Cardano transactions"})}),"\n",(0,a.jsxs)(t.p,{children:["The Mithril protocol supports the certification of the ",(0,a.jsx)(t.strong,{children:"full Cardano transactions set (since genesis)"}),". This allows users to verify a transaction's authenticity without downloading the entire Cardano blockchain.\nThis is particularly useful for lightweight clients, such as mobile wallets, which may lack the resources to store the entire blockchain."]}),"\n",(0,a.jsxs)(t.p,{children:["To achieve this, Mithril signers and aggregators independently compute a message representing the ",(0,a.jsx)(t.strong,{children:"Cardano transaction set"})," and apply the Mithril protocol to jointly sign it. A proof of membership is then generated on demand for the subset of transactions a Mithril client attempts to verify. This proof can be validated against the signed message, which is included in the Mithril certificate."]}),"\n",(0,a.jsxs)(t.p,{children:["A natural structure for the message is a ",(0,a.jsx)(t.strong,{children:"Merkle tree"}),", which:"]}),"\n",(0,a.jsxs)(t.ul,{children:["\n",(0,a.jsxs)(t.li,{children:["Can be succinctly represented by its ",(0,a.jsx)(t.strong,{children:"Merkle root"})," (the signed message)"]}),"\n",(0,a.jsxs)(t.li,{children:["Allows for membership proof of a transaction in the set by providing the ",(0,a.jsx)(t.strong,{children:"Merkle path"})," from the transaction to the root."]}),"\n"]}),"\n",(0,a.jsx)(t.p,{children:"This certification is conducted under high constraints when operating on the Cardano mainnet:"}),"\n",(0,a.jsxs)(t.ul,{children:["\n",(0,a.jsxs)(t.li,{children:["The current Cardano transaction set exceeds ",(0,a.jsx)(t.code,{children:"100 million"})," transactions"]}),"\n",(0,a.jsx)(t.li,{children:"The Mithril signer footprint must remain minimal (low memory, low CPU, low disk space)"}),"\n",(0,a.jsx)(t.li,{children:"On-demand generation of the proof of membership must be fast and scalable to high throughput."}),"\n"]}),"\n",(0,a.jsxs)(t.admonition,{type:"info",children:[(0,a.jsxs)(t.p,{children:["Another consideration is the finality of the Cardano chain itself: the closer we examine the tip of the chain, the more likely blocks can be rolled back, potentially invalidating transactions. However, the Mithril protocol is designed to offer certification with high guarantees. As an initial implementation, we have chosen to certify the transaction set at a ",(0,a.jsx)(t.strong,{children:"fixed offset from the tip of the chain"})," (currently ",(0,a.jsx)(t.code,{children:"100"})," blocks on the Cardano mainnet). This balances finality guarantees and the latency required for Mithril to certify a transaction after submission."]}),(0,a.jsx)(t.p,{children:"The distance from the tip at which the transaction set is certified affects the Mithril protocol itself: the closer to the tip, the more likely some signers may operate on a temporary fork of the Cardano chain (which will later be rolled back), potentially preventing the quorum needed to create a valid multi-signature. The offset can be adjusted as a Mithril network parameter."}),(0,a.jsxs)(t.p,{children:["It is also worth noting that a new signature round is ",(0,a.jsx)(t.strong,{children:"triggered at a constant pace"})," (every ",(0,a.jsx)(t.code,{children:"30"})," blocks on the Cardano mainnet)."]})]}),"\n",(0,a.jsx)(t.h2,{id:"mithril-certification",children:"Mithril certification"}),"\n",(0,a.jsxs)(t.p,{children:[(0,a.jsx)(t.a,{target:"_blank","data-noBrokenLinkCheck":!0,href:i(95906).A+"",children:(0,a.jsx)(t.img,{alt:"Design of the certification of the Cardano transactions",src:i(61678).A+"",width:"1359",height:"735"})}),"\n",(0,a.jsx)("small",{children:(0,a.jsx)("center",{children:"End to end certification for Cardano transactions"})})]}),"\n",(0,a.jsx)(t.admonition,{type:"info",children:(0,a.jsxs)(t.p,{children:["Learn about the Mithril certification steps ",(0,a.jsx)(t.a,{href:"/doc/next/mithril/advanced/mithril-certification/",children:"here"}),"."]})}),"\n",(0,a.jsx)(t.h3,{id:"message-computation",children:"Message computation"}),"\n",(0,a.jsxs)(t.p,{children:["Creating a Merkle tree with ",(0,a.jsx)(t.code,{children:"100 million"})," leaves is impractical due to high memory usage and long computation times, which exceed the operational capacity of the signer. However, a ",(0,a.jsx)(t.strong,{children:"Merkle forest"})," offers a suitable solution. In this structure, the leaves of the signed Merkle tree are the roots of separate Merkle trees, each representing a contiguous block range. Each Merkle tree\u2019s leaves are the transaction hashes within those blocks."]}),"\n",(0,a.jsx)(t.p,{children:"This structure is nearly append-only for transactions, allowing for some stored data compression when not used to create a membership proof. As a result, the volumes of information stored on signers and aggregators differ."}),"\n",(0,a.jsxs)(t.p,{children:["The blocks are divided into ",(0,a.jsx)(t.strong,{children:"block ranges"})," of ",(0,a.jsx)(t.code,{children:"15"})," blocks. The leaves of the Merkle trees are the hashes of the transactions in the blocks within each range (",(0,a.jsx)(t.code,{children:"~150-1.5k"})," transactions per block range on the Cardano mainnet).\nThis reduces the number of leaves in the Merkle forest to approximately ",(0,a.jsx)(t.code,{children:"1 million"})," on the Cardano mainnet \u2013 about ",(0,a.jsx)(t.code,{children:"100"})," times fewer than the number of transactions in the blockchain.\nThis allows the creation of a Merkle forest with, on average, ",(0,a.jsx)(t.code,{children:"100"})," times fewer leaves than the number of transactions in the Cardano blockchain (",(0,a.jsx)(t.code,{children:"~1"})," million leaves on the Cardano mainnet)."]}),"\n",(0,a.jsxs)(t.p,{children:[(0,a.jsx)(t.a,{target:"_blank","data-noBrokenLinkCheck":!0,href:i(57292).A+"",children:(0,a.jsx)(t.img,{alt:"Design of the certification of the Cardano transactions",src:i(29624).A+"",width:"1395",height:"716"})}),"\n",(0,a.jsx)("small",{children:(0,a.jsx)("center",{children:"Message creation when aggregating on the aggregator"})})]}),"\n",(0,a.jsxs)(t.p,{children:["The process is almost the same on the signer, except that the transactions of the block ranges are ephemerally stored and only their compressed representation is kept in the long run (the Merkle root of the block range Merkle tree) once the blocks are final (older than ",(0,a.jsx)(t.code,{children:"k"})," blocks from the tip of the chain, ",(0,a.jsx)(t.code,{children:"2160"})," on the Cardano mainnet). This allows drastic compression of the storage on the signers."]}),"\n",(0,a.jsxs)(t.p,{children:[(0,a.jsx)(t.a,{target:"_blank","data-noBrokenLinkCheck":!0,href:i(14157).A+"",children:(0,a.jsx)(t.img,{alt:"Design of the certification of the Cardano transactions",src:i(2561).A+"",width:"1395",height:"716"})}),"\n",(0,a.jsx)("small",{children:(0,a.jsx)("center",{children:"Message creation when signing on the aggregator"})})]}),"\n",(0,a.jsx)(t.admonition,{type:"info",children:(0,a.jsxs)(t.p,{children:["The Merkle tree inner nodes are computed with the ",(0,a.jsx)(t.code,{children:"BLAKE2s-256"})," hash function: the child bytes are concatenated and hashed to compute the parent node."]})}),"\n",(0,a.jsx)(t.h3,{id:"authenticity-verification",children:"Authenticity verification"}),"\n",(0,a.jsx)(t.p,{children:"The verification process operates on a subset of the Cardano set that can be certified (fully or partially):"}),"\n",(0,a.jsxs)(t.ul,{children:["\n",(0,a.jsxs)(t.li,{children:["The client calls a prover route exposed by the aggregator, which computes a ",(0,a.jsx)(t.strong,{children:"Merkle proof of membership"})," for the transactions signed in the latest snapshot"]}),"\n",(0,a.jsx)(t.li,{children:"The client verifies that the proof of membership is valid and that its Merkle root (the message) is signed by a valid Mithril certificate."}),"\n"]}),"\n",(0,a.jsxs)(t.p,{children:[(0,a.jsx)(t.a,{target:"_blank","data-noBrokenLinkCheck":!0,href:i(71107).A+"",children:(0,a.jsx)(t.img,{alt:"Design of the certification of the Cardano transactions",src:i(52071).A+"",width:"1395",height:"716"})}),"\n",(0,a.jsx)("small",{children:(0,a.jsxs)("center",{children:["Proof creation done by the aggregator ",(0,a.jsx)(t.em,{children:"(to verify 'Tx4' and `Tx62')"})]})})]})]})}function d(e={}){const{wrapper:t}={...(0,r.R)(),...e.components};return t?(0,a.jsx)(t,{...e,children:(0,a.jsx)(l,{...e})}):l(e)}},95906:(e,t,i)=>{i.d(t,{A:()=>n});const n=i.p+"assets/files/end-to-end-process-56f109261814c9baa4ec98aff6fdc452.jpg"},57292:(e,t,i)=>{i.d(t,{A:()=>n});const n=i.p+"assets/files/message-aggregator-d5c8b53df889a44ac4b9400682fb4f09.jpg"},14157:(e,t,i)=>{i.d(t,{A:()=>n});const n=i.p+"assets/files/message-signer-4af016319ff363e62779a51dc6d07f2d.jpg"},71107:(e,t,i)=>{i.d(t,{A:()=>n});const n=i.p+"assets/files/proof-client-966d78865fe32b397d721af9fc933bb9.jpg"},61678:(e,t,i)=>{i.d(t,{A:()=>n});const n=i.p+"assets/images/end-to-end-process-56f109261814c9baa4ec98aff6fdc452.jpg"},29624:(e,t,i)=>{i.d(t,{A:()=>n});const n=i.p+"assets/images/message-aggregator-d5c8b53df889a44ac4b9400682fb4f09.jpg"},2561:(e,t,i)=>{i.d(t,{A:()=>n});const n=i.p+"assets/images/message-signer-4af016319ff363e62779a51dc6d07f2d.jpg"},52071:(e,t,i)=>{i.d(t,{A:()=>n});const n=i.p+"assets/images/proof-client-966d78865fe32b397d721af9fc933bb9.jpg"},28453:(e,t,i)=>{i.d(t,{R:()=>s,x:()=>o});var n=i(96540);const a={},r=n.createContext(a);function s(e){const t=n.useContext(r);return n.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function o(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:s(e.components),n.createElement(r.Provider,{value:t},e.children)}}}]);