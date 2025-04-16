"use strict";(self.webpackChunkmithril_doc=self.webpackChunkmithril_doc||[]).push([[9862],{87:(e,i,n)=>{n.r(i),n.d(i,{assets:()=>l,contentTitle:()=>a,default:()=>h,frontMatter:()=>o,metadata:()=>r,toc:()=>d});var r=n(4288),t=n(74848),s=n(28453);const o={title:"Distribution `2513` is now available",authors:[{name:"Mithril Team"}],tags:["release","distribution",2513,"security-advisory"]},a=void 0,l={authorsImageUrls:[void 0]},d=[{value:"Distribution <code>2513</code> is now available",id:"distribution-2513-is-now-available",level:3}];function c(e){const i={a:"a",code:"code",h3:"h3",li:"li",p:"p",pre:"pre",strong:"strong",ul:"ul",...(0,s.R)(),...e.components};return(0,t.jsxs)(t.Fragment,{children:[(0,t.jsxs)(i.h3,{id:"distribution-2513-is-now-available",children:["Distribution ",(0,t.jsx)(i.code,{children:"2513"})," is now available"]}),"\n",(0,t.jsxs)(i.p,{children:["The ",(0,t.jsx)(i.a,{href:"https://github.com/input-output-hk/mithril/releases/tag/2513.0",children:(0,t.jsx)(i.code,{children:"2513.0"})})," distribution has been released, introducing the following changes:"]}),"\n",(0,t.jsxs)(i.ul,{children:["\n",(0,t.jsxs)(i.li,{children:["\u26a0\ufe0f ",(0,t.jsx)(i.strong,{children:"Breaking"})," changes in Mithril nodes:","\n",(0,t.jsxs)(i.ul,{children:["\n",(0,t.jsxs)(i.li,{children:["The ",(0,t.jsx)(i.strong,{children:"minimum required"})," ",(0,t.jsx)(i.code,{children:"glibc"})," version for pre-built Linux binaries has been upgraded from ",(0,t.jsx)(i.code,{children:"2.31"})," to ",(0,t.jsx)(i.code,{children:"2.35"})]}),"\n",(0,t.jsxs)(i.li,{children:["Mithril signers running versions ",(0,t.jsx)(i.code,{children:"<=0.2.200"})," ",(0,t.jsx)(i.strong,{children:"must be updated"})," due to the removal of Thales era legacy code"]}),"\n",(0,t.jsxs)(i.li,{children:["The ",(0,t.jsx)(i.code,{children:"with_snapshot_uploader"})," function in the Mithril client library has been renamed to ",(0,t.jsx)(i.code,{children:"with_file_uploader"})]}),"\n"]}),"\n"]}),"\n",(0,t.jsxs)(i.li,{children:["Added support for Cardano node ",(0,t.jsx)(i.code,{children:"10.2.1"})," in the signer and aggregator"]}),"\n",(0,t.jsxs)(i.li,{children:["Ended support for ",(0,t.jsx)(i.strong,{children:"macOS x64 pre-built binaries"})," for the client CLI"]}),"\n",(0,t.jsx)(i.li,{children:"Bug fixes and performance improvements."}),"\n"]}),"\n",(0,t.jsxs)(i.p,{children:["This new distribution has been deployed to the ",(0,t.jsx)(i.strong,{children:"Mithril aggregator"})," on the ",(0,t.jsx)(i.code,{children:"release-mainnet"})," and ",(0,t.jsx)(i.code,{children:"release-preprod"})," networks."]}),"\n",(0,t.jsxs)(i.p,{children:["If you are running a ",(0,t.jsx)(i.strong,{children:"Mithril signer"}),":"]}),"\n",(0,t.jsxs)(i.ul,{children:["\n",(0,t.jsxs)(i.li,{children:[(0,t.jsx)(i.strong,{children:"pre-release-preview"})," network: no action is required at this time"]}),"\n",(0,t.jsxs)(i.li,{children:[(0,t.jsx)(i.strong,{children:"release-preprod"})," network: upgrade your signer node binary to version ",(0,t.jsx)(i.code,{children:"0.2.237"})," \u2013 no configuration updates are required"]}),"\n",(0,t.jsxs)(i.li,{children:[(0,t.jsx)(i.strong,{children:"release-mainnet"})," network: upgrade your signer node binary to version ",(0,t.jsx)(i.code,{children:"0.2.237"}),"\u2013 no configuration updates are required."]}),"\n"]}),"\n",(0,t.jsx)(i.p,{children:"You can update the Mithril signer using the one-line command below. It downloads to the current directory by default, but you can specify a custom folder using the -p option:"}),"\n",(0,t.jsx)(i.pre,{children:(0,t.jsx)(i.code,{className:"language-bash",children:"curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/input-output-hk/mithril/refs/heads/main/mithril-install.sh | sh -s -- -c mithril-signer -d 2513.0 -p $(pwd)\n"})}),"\n",(0,t.jsxs)(i.p,{children:["For any inquiries or assistance, contact the team on the ",(0,t.jsx)(i.a,{href:"https://discord.gg/5kaErDKDRq",children:"Discord channel"}),"."]})]})}function h(e={}){const{wrapper:i}={...(0,s.R)(),...e.components};return i?(0,t.jsx)(i,{...e,children:(0,t.jsx)(c,{...e})}):c(e)}},28453:(e,i,n)=>{n.d(i,{R:()=>o,x:()=>a});var r=n(96540);const t={},s=r.createContext(t);function o(e){const i=r.useContext(s);return r.useMemo((function(){return"function"==typeof e?e(i):{...i,...e}}),[i,e])}function a(e){let i;return i=e.disableParentContext?"function"==typeof e.components?e.components(t):e.components||t:o(e.components),r.createElement(s.Provider,{value:i},e.children)}},4288:e=>{e.exports=JSON.parse('{"permalink":"/doc/dev-blog/2025/03/28/distribution-2513","source":"@site/blog/2025-03-28-distribution-2513.md","title":"Distribution `2513` is now available","description":"Distribution 2513 is now available","date":"2025-03-28T00:00:00.000Z","tags":[{"inline":true,"label":"release","permalink":"/doc/dev-blog/tags/release"},{"inline":true,"label":"distribution","permalink":"/doc/dev-blog/tags/distribution"},{"inline":true,"label":"2513","permalink":"/doc/dev-blog/tags/2513"},{"inline":true,"label":"security-advisory","permalink":"/doc/dev-blog/tags/security-advisory"}],"readingTime":1.14,"hasTruncateMarker":false,"authors":[{"name":"Mithril Team","socials":{},"key":null,"page":null}],"frontMatter":{"title":"Distribution `2513` is now available","authors":[{"name":"Mithril Team"}],"tags":["release","distribution","2513","security-advisory"]},"unlisted":false,"prevItem":{"title":"Certification of Cardano node database v2","permalink":"/doc/dev-blog/2025/04/14/cardano-node-database-v2-certification"},"nextItem":{"title":"Ending support for macOS x64 pre-built binaries","permalink":"/doc/dev-blog/2025/02/18/end-of-support-macos-x64"}}')}}]);