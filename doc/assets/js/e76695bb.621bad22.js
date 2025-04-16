"use strict";(self.webpackChunkmithril_doc=self.webpackChunkmithril_doc||[]).push([[4049],{27534:(e,t,i)=>{i.r(t),i.d(t,{assets:()=>h,contentTitle:()=>a,default:()=>c,frontMatter:()=>s,metadata:()=>r,toc:()=>d});var r=i(22383),n=i(74848),o=i(28453);const s={title:"Stake Distribution retrieval fixed",authors:[{name:"Mithril Team"}],tags:["stake-distribution","certificate"]},a=void 0,h={authorsImageUrls:[void 0]},d=[{value:"The way the Mithril nodes retrieve the Stake Distribution is changing",id:"the-way-the-mithril-nodes-retrieve-the-stake-distribution-is-changing",level:3}];function l(e){const t={a:"a",code:"code",h3:"h3",p:"p",strong:"strong",...(0,o.R)(),...e.components};return(0,n.jsxs)(n.Fragment,{children:[(0,n.jsxs)(t.p,{children:[(0,n.jsx)(t.strong,{children:"Update"}),": The Stake Distribution computation is evolving with the release of Cardano node ",(0,n.jsx)(t.code,{children:"8.0.0"}),": the computation now relies on the new ",(0,n.jsx)(t.code,{children:"cardano-cli query stake-snapshot --all-stake-pools"})," command that retrieves the Stake Distribution all at once and that is way faster. Prior versions of the Cardano node ",(0,n.jsx)(t.code,{children:"1.35+"})," are backward compatible and will keep implementing the algorithm detailed below."]}),"\n",(0,n.jsx)(t.h3,{id:"the-way-the-mithril-nodes-retrieve-the-stake-distribution-is-changing",children:"The way the Mithril nodes retrieve the Stake Distribution is changing"}),"\n",(0,n.jsxs)(t.p,{children:[(0,n.jsx)(t.strong,{children:"PR"}),": ",(0,n.jsx)(t.code,{children:"Fix Stake Distribution retrieval"})," ",(0,n.jsx)(t.a,{href:"https://github.com/input-output-hk/mithril/pull/499",children:"#499"})]}),"\n",(0,n.jsxs)(t.p,{children:[(0,n.jsx)(t.strong,{children:"Issue"}),": ",(0,n.jsx)(t.code,{children:"Stake distribution discrepancy"})," ",(0,n.jsx)(t.a,{href:"https://github.com/input-output-hk/mithril/issues/497",children:"#497"})]}),"\n",(0,n.jsxs)(t.p,{children:["We have noticed that the way the Mithril nodes computed the ",(0,n.jsx)(t.code,{children:"Stake Distribution"})," was erroneous: the epoch that was used to make the computation was the ",(0,n.jsx)(t.strong,{children:"current epoch"})," instead of the ",(0,n.jsx)(t.strong,{children:"previous epoch"}),". This has lead to some de-synchronization between the Signers and the hosted GCP Aggregator for a few epochs."]}),"\n",(0,n.jsxs)(t.p,{children:["Indeed, the ",(0,n.jsx)(t.code,{children:"Stake Distribution"})," retrieved from the Cardano node depended on the time at which it was done: the nodes where having differents values that prevented them from being able to work together to produce valid multi-signatures. The problem is related to the epoch that is used (",(0,n.jsx)(t.strong,{children:"current epoch"}),") to make the computation of the ",(0,n.jsx)(t.code,{children:"Stake Distribution"})," when the ",(0,n.jsx)(t.code,{children:"cardano-cli query stake-distribution"})," command is ran, whereas the Mithril protocol needs to work with the ",(0,n.jsx)(t.strong,{children:"previous epoch"}),"."]}),"\n",(0,n.jsxs)(t.p,{children:["A workaround is being implemented in this fix that will compute differently the ",(0,n.jsx)(t.code,{children:"Stake Distribution"})," to target the ",(0,n.jsx)(t.strong,{children:"previous epoch"}),". To do so, the Stake value that is now retrieved sequentially for each pool available in the ",(0,n.jsx)(t.code,{children:"cardano-cli query stake-distribution"})," by using the command ",(0,n.jsx)(t.code,{children:"cardano-cli query stake-snapshot --stake-pool-id **pool-id*"}),". This guarantees that the ",(0,n.jsx)(t.code,{children:"Stake Distribution"})," is computed deterministically on all nodes of the Mithril Network."]}),"\n",(0,n.jsxs)(t.p,{children:["We will continue our efforts to enhance the way the ",(0,n.jsx)(t.code,{children:"Stake Distribution"})," is retrieved in the future, and so that it works smoothly on the ",(0,n.jsx)(t.code,{children:"mainnet"})," (where the numbers of pools is bigger ",(0,n.jsx)(t.code,{children:"~3,000"})," vs ",(0,n.jsx)(t.code,{children:"~100"})," on the ",(0,n.jsx)(t.code,{children:"preview"})," network)."]}),"\n",(0,n.jsxs)(t.p,{children:["The SPOs need to recompile their Signer node in order to compute correctly the ",(0,n.jsx)(t.code,{children:"Stake Distributions"})," on their node (as in this ",(0,n.jsx)(t.a,{href:"https://mithril.network/doc/manual/getting-started/run-signer-node",children:"guide"}),").\nIt should then take up to ",(0,n.jsx)(t.code,{children:"2"})," epochs before they are able to successfully register their individual signatures with the Aggregator."]}),"\n",(0,n.jsxs)(t.p,{children:["More information about the ",(0,n.jsx)(t.code,{children:"Certificate Chain"})," and the epochs retrieval requirements is available ",(0,n.jsx)(t.a,{href:"https://mithril.network/doc/mithril/mithril-protocol/certificates",children:"here"}),"."]}),"\n",(0,n.jsxs)(t.p,{children:["Feel free to reach out to us on the ",(0,n.jsx)(t.a,{href:"https://discord.gg/5kaErDKDRq",children:"Discord channel"})," for questions and/or help."]})]})}function c(e={}){const{wrapper:t}={...(0,o.R)(),...e.components};return t?(0,n.jsx)(t,{...e,children:(0,n.jsx)(l,{...e})}):l(e)}},28453:(e,t,i)=>{i.d(t,{R:()=>s,x:()=>a});var r=i(96540);const n={},o=r.createContext(n);function s(e){const t=r.useContext(o);return r.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function a(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(n):e.components||n:s(e.components),r.createElement(o.Provider,{value:t},e.children)}},22383:e=>{e.exports=JSON.parse('{"permalink":"/doc/dev-blog/2022/09/13/stake-distribution-retrieval","source":"@site/blog/2022-09-13-stake-distribution-retrieval.md","title":"Stake Distribution retrieval fixed","description":"Update the computation now relies on the new cardano-cli query stake-snapshot --all-stake-pools command that retrieves the Stake Distribution all at once and that is way faster. Prior versions of the Cardano node 1.35+ are backward compatible and will keep implementing the algorithm detailed below.","date":"2022-09-13T00:00:00.000Z","tags":[{"inline":true,"label":"stake-distribution","permalink":"/doc/dev-blog/tags/stake-distribution"},{"inline":true,"label":"certificate","permalink":"/doc/dev-blog/tags/certificate"}],"readingTime":1.925,"hasTruncateMarker":false,"authors":[{"name":"Mithril Team","socials":{},"key":null,"page":null}],"frontMatter":{"title":"Stake Distribution retrieval fixed","authors":[{"name":"Mithril Team"}],"tags":["stake-distribution","certificate"]},"unlisted":false,"prevItem":{"title":"Mithril internal stores switch to SQLite.","permalink":"/doc/dev-blog/2022/09/14/sqlite-stores"},"nextItem":{"title":"Signers list computation in Certificates","permalink":"/doc/dev-blog/2022/09/12/certificate-signers-list"}}')}}]);