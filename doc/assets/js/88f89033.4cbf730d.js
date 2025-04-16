"use strict";(self.webpackChunkmithril_doc=self.webpackChunkmithril_doc||[]).push([[2096],{27960:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>c,contentTitle:()=>l,default:()=>d,frontMatter:()=>s,metadata:()=>i,toc:()=>o});var i=n(66585),a=n(74848),r=n(28453);const s={title:"Mithril client WASM breaking change",authors:[{name:"Mithril Team"}],tags:["mithril client","cli","breaking-change"]},l=void 0,c={authorsImageUrls:[void 0]},o=[{value:"Breaking change introduced in the unstable features of the Mithril client WASM",id:"breaking-change-introduced-in-the-unstable-features-of-the-mithril-client-wasm",level:3}];function h(e){const t={a:"a",code:"code",h3:"h3",p:"p",pre:"pre",strong:"strong",...(0,r.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(t.h3,{id:"breaking-change-introduced-in-the-unstable-features-of-the-mithril-client-wasm",children:"Breaking change introduced in the unstable features of the Mithril client WASM"}),"\n",(0,a.jsxs)(t.p,{children:["With the release of ",(0,a.jsx)(t.a,{href:"https://github.com/input-output-hk/mithril/releases/tag/2437.1",children:"distribution 2437"}),", we introduced a breaking change to the ",(0,a.jsx)(t.strong,{children:"Mithril client WASM"})," version ",(0,a.jsx)(t.code,{children:"0.4.1"}),". Unstable features are now activated using a ",(0,a.jsx)(t.strong,{children:"configuration option"})," instead of the ",(0,a.jsx)(t.code,{children:".unstable"})," property."]}),"\n",(0,a.jsxs)(t.p,{children:["This change ensures a seamless transition when new ",(0,a.jsx)(t.code,{children:"unstable"})," features become ",(0,a.jsx)(t.code,{children:"stable"}),", eliminating breaking changes in developer code and enhancing the developer experience."]}),"\n",(0,a.jsxs)(t.p,{children:["To activate ",(0,a.jsx)(t.code,{children:"unstable"})," features, use the following code:"]}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-js",children:"let client = new MithrilClient(aggregator_endpoint, genesis_verification_key, {\n  // The following option activates the unstable features of the client.\n  // Unstable features will trigger an error if this option is not set.\n  unstable: true,\n});\n"})}),"\n",(0,a.jsxs)(t.p,{children:["The previous ",(0,a.jsx)(t.code,{children:"client.unstable"})," implementation is not supported anymore and must be replaced with ",(0,a.jsx)(t.code,{children:"client"}),":"]}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-js",children:"// Before\nlet mithril_stake_distributions_message =\n  await client.unstable.compute_mithril_stake_distribution_message(\n    last_stake_distribution,\n  );\n"})}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-js",children:"// After\nlet mithril_stake_distributions_message =\n  await client.compute_mithril_stake_distribution_message(\n    last_stake_distribution,\n  );\n"})}),"\n",(0,a.jsxs)(t.p,{children:["The Mithril client WASM documentation is available ",(0,a.jsx)(t.a,{href:"https://mithril.network/doc/manual/developer-docs/nodes/mithril-client-library-wasm",children:"here"}),"."]}),"\n",(0,a.jsxs)(t.p,{children:["For questions or assistance, contact the team on the ",(0,a.jsx)(t.a,{href:"https://discord.gg/5kaErDKDRq",children:"Discord channel"}),"."]})]})}function d(e={}){const{wrapper:t}={...(0,r.R)(),...e.components};return t?(0,a.jsx)(t,{...e,children:(0,a.jsx)(h,{...e})}):h(e)}},28453:(e,t,n)=>{n.d(t,{R:()=>s,x:()=>l});var i=n(96540);const a={},r=i.createContext(a);function s(e){const t=i.useContext(r);return i.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function l(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:s(e.components),i.createElement(r.Provider,{value:t},e.children)}},66585:e=>{e.exports=JSON.parse('{"permalink":"/doc/dev-blog/2024/09/24/client-wasm-unstable-breaking-change","source":"@site/blog/2024-09-24-client-wasm-unstable-breaking-change.md","title":"Mithril client WASM breaking change","description":"Breaking change introduced in the unstable features of the Mithril client WASM","date":"2024-09-24T00:00:00.000Z","tags":[{"inline":true,"label":"mithril client","permalink":"/doc/dev-blog/tags/mithril-client"},{"inline":true,"label":"cli","permalink":"/doc/dev-blog/tags/cli"},{"inline":true,"label":"breaking-change","permalink":"/doc/dev-blog/tags/breaking-change"}],"readingTime":0.835,"hasTruncateMarker":false,"authors":[{"name":"Mithril Team","socials":{},"key":null,"page":null}],"frontMatter":{"title":"Mithril client WASM breaking change","authors":[{"name":"Mithril Team"}],"tags":["mithril client","cli","breaking-change"]},"unlisted":false,"prevItem":{"title":"Certification of Cardano stake distribution","permalink":"/doc/dev-blog/2024/10/15/cardano-stake-distribution-certification"},"nextItem":{"title":"Certification of Cardano transactions","permalink":"/doc/dev-blog/2024/07/30/cardano-transaction-certification"}}')}}]);