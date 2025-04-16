"use strict";(self.webpackChunkmithril_doc=self.webpackChunkmithril_doc||[]).push([[5624],{758:(e,i,n)=>{n.r(i),n.d(i,{assets:()=>o,contentTitle:()=>a,default:()=>c,frontMatter:()=>s,metadata:()=>t,toc:()=>h});var t=n(74326),l=n(74848),r=n(28453);const s={title:"One line installer for Mithril binaries",authors:[{name:"Mithril Team"}],tags:["binaries","pre-built","install","update","nodes","command-line","binaries","installer"]},a=void 0,o={authorsImageUrls:[void 0]},h=[{value:"One line installer for Mithril binaries",id:"one-line-installer-for-mithril-binaries",level:3},{value:"Examples of the one line installer",id:"examples-of-the-one-line-installer",level:4},{value:"Installer usage",id:"installer-usage",level:4}];function d(e){const i={a:"a",code:"code",h3:"h3",h4:"h4",li:"li",p:"p",pre:"pre",strong:"strong",ul:"ul",...(0,r.R)(),...e.components};return(0,l.jsxs)(l.Fragment,{children:[(0,l.jsx)(i.h3,{id:"one-line-installer-for-mithril-binaries",children:"One line installer for Mithril binaries"}),"\n",(0,l.jsx)(i.p,{children:"To simplify the installation and updating of Mithril binaries, we have created a one line installer that downloads and installs the Mithril binaries for you. This installer is available for Linux and macOS and supports the Mithril signer, Mithril aggregator, and Mithril client CLI."}),"\n",(0,l.jsxs)(i.p,{children:["The one line command is also displayed in the various ",(0,l.jsx)(i.code,{children:"Download the pre-built binary"})," sections across the documentation."]}),"\n",(0,l.jsx)(i.h4,{id:"examples-of-the-one-line-installer",children:"Examples of the one line installer"}),"\n",(0,l.jsxs)(i.ul,{children:["\n",(0,l.jsxs)(i.li,{children:["Download the ",(0,l.jsx)(i.strong,{children:"latest Mithril signer"})," in the current directory:"]}),"\n"]}),"\n",(0,l.jsx)(i.pre,{children:(0,l.jsx)(i.code,{className:"language-bash",children:"curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/input-output-hk/mithril/refs/heads/main/mithril-install.sh | sh -s -- -c mithril-signer -d latest -p $(pwd)\n"})}),"\n",(0,l.jsxs)(i.ul,{children:["\n",(0,l.jsxs)(i.li,{children:["Download the ",(0,l.jsx)(i.strong,{children:"latest Mithril client CLI"})," in the current directory:"]}),"\n"]}),"\n",(0,l.jsx)(i.pre,{children:(0,l.jsx)(i.code,{className:"language-bash",children:"curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/input-output-hk/mithril/refs/heads/main/mithril-install.sh | sh -s -- -c mithril-client -d latest -p $(pwd)\n"})}),"\n",(0,l.jsxs)(i.ul,{children:["\n",(0,l.jsxs)(i.li,{children:["Download the ",(0,l.jsx)(i.strong,{children:"unstable Mithril aggregator"})," in the current directory:"]}),"\n"]}),"\n",(0,l.jsx)(i.pre,{children:(0,l.jsx)(i.code,{className:"language-bash",children:"curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/input-output-hk/mithril/refs/heads/main/mithril-install.sh | sh -s -- -c mithril-aggregator -d unstable -p $(pwd)\n"})}),"\n",(0,l.jsxs)(i.ul,{children:["\n",(0,l.jsxs)(i.li,{children:["Download the ",(0,l.jsxs)(i.strong,{children:["Mithril client of distribution ",(0,l.jsx)(i.code,{children:"2445.0"})]})," in the current directory:"]}),"\n"]}),"\n",(0,l.jsx)(i.pre,{children:(0,l.jsx)(i.code,{className:"language-bash",children:"curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/input-output-hk/mithril/refs/heads/main/mithril-install.sh | sh -s -- -c mithril-client -d 2445.0 -p $(pwd)\n"})}),"\n",(0,l.jsx)(i.h4,{id:"installer-usage",children:"Installer usage"}),"\n",(0,l.jsx)(i.pre,{children:(0,l.jsx)(i.code,{className:"language-bash",children:"curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/input-output-hk/mithril/refs/heads/main/mithril-install.sh | sh -s -- -h\n\nInstall or upgrade a Mithril node\nUsage: sh [-n node] [-v version] [-d distribution] [-p path]\n  -c node          : Mithril node to install or upgrade (mithril-signer, mithril-aggregator, mithril-client)\n  -d distribution  : Distribution to upgrade to (latest, unstable or distribution version e.g '2445.0')\n  -p path          : Path to install the component\n\n"})}),"\n",(0,l.jsxs)(i.p,{children:["For any inquiries or assistance, feel free to contact the team on the ",(0,l.jsx)(i.a,{href:"https://discord.gg/5kaErDKDRq",children:"Discord channel"}),"."]})]})}function c(e={}){const{wrapper:i}={...(0,r.R)(),...e.components};return i?(0,l.jsx)(i,{...e,children:(0,l.jsx)(d,{...e})}):d(e)}},28453:(e,i,n)=>{n.d(i,{R:()=>s,x:()=>a});var t=n(96540);const l={},r=t.createContext(l);function s(e){const i=t.useContext(r);return t.useMemo((function(){return"function"==typeof e?e(i):{...i,...e}}),[i,e])}function a(e){let i;return i=e.disableParentContext?"function"==typeof e.components?e.components(l):e.components||l:s(e.components),t.createElement(r.Provider,{value:i},e.children)}},74326:e=>{e.exports=JSON.parse('{"permalink":"/doc/dev-blog/2024/11/25/one-line-binaries-installer","source":"@site/blog/2024-11-25-one-line-binaries-installer.md","title":"One line installer for Mithril binaries","description":"One line installer for Mithril binaries","date":"2024-11-25T00:00:00.000Z","tags":[{"inline":true,"label":"binaries","permalink":"/doc/dev-blog/tags/binaries"},{"inline":true,"label":"pre-built","permalink":"/doc/dev-blog/tags/pre-built"},{"inline":true,"label":"install","permalink":"/doc/dev-blog/tags/install"},{"inline":true,"label":"update","permalink":"/doc/dev-blog/tags/update"},{"inline":true,"label":"nodes","permalink":"/doc/dev-blog/tags/nodes"},{"inline":true,"label":"command-line","permalink":"/doc/dev-blog/tags/command-line"},{"inline":true,"label":"binaries","permalink":"/doc/dev-blog/tags/binaries"},{"inline":true,"label":"installer","permalink":"/doc/dev-blog/tags/installer"}],"readingTime":1.36,"hasTruncateMarker":false,"authors":[{"name":"Mithril Team","socials":{},"key":null,"page":null}],"frontMatter":{"title":"One line installer for Mithril binaries","authors":[{"name":"Mithril Team"}],"tags":["binaries","pre-built","install","update","nodes","command-line","binaries","installer"]},"unlisted":false,"prevItem":{"title":"Era switch to Pythagoras","permalink":"/doc/dev-blog/2024/12/17/era-switch-pythagoras"},"nextItem":{"title":"New Protocol Insights Dashboard released","permalink":"/doc/dev-blog/2024/11/18/new-protocol-insights-dashboard"}}')}}]);