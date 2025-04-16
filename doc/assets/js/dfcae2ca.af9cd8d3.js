"use strict";(self.webpackChunkmithril_doc=self.webpackChunkmithril_doc||[]).push([[1940],{27785:(e,s,r)=>{r.r(s),r.d(s,{assets:()=>a,contentTitle:()=>l,default:()=>h,frontMatter:()=>o,metadata:()=>i,toc:()=>d});var i=r(83612),n=r(74848),t=r(28453);const o={title:"Mithril Release Process",authors:[{name:"Mithril Team"}],tags:["process"]},l=void 0,a={authorsImageUrls:[void 0]},d=[{value:"Mithril follows a defined release process",id:"mithril-follows-a-defined-release-process",level:3},{value:"Release Process",id:"release-process",level:3},{value:"Networks",id:"networks",level:3},{value:"Further Reading",id:"further-reading",level:3}];function c(e){const s={a:"a",code:"code",em:"em",h3:"h3",img:"img",li:"li",p:"p",strong:"strong",ul:"ul",...(0,t.R)(),...e.components};return(0,n.jsxs)(n.Fragment,{children:[(0,n.jsx)(s.h3,{id:"mithril-follows-a-defined-release-process",children:"Mithril follows a defined release process"}),"\n",(0,n.jsx)(s.p,{children:"As the Mithril project grew and more and more SPOs became involved in testing Mithril, it became obvious we need clearer identification of artifacts running on various parts of the network. Moreover, on our road towards mainnet availability we'll need to strengthen our testing process in order to validate Mithril network on more realistic environments."}),"\n",(0,n.jsx)(s.h3,{id:"release-process",children:"Release Process"}),"\n",(0,n.jsx)(s.p,{children:"We want our release process to follow some basic principles:"}),"\n",(0,n.jsxs)(s.ul,{children:["\n",(0,n.jsxs)(s.li,{children:[(0,n.jsx)(s.em,{children:"Continuous Integration"}),": New code is integrated into the ",(0,n.jsx)(s.code,{children:"main"})," codeline frequently which triggers automated build and test process."]}),"\n",(0,n.jsxs)(s.li,{children:[(0,n.jsx)(s.em,{children:"Continuous Deployment"}),": New artifacts produced by the build process are continuously deployed to a suitable ",(0,n.jsx)(s.em,{children:"environment"})," where it can be used and tested by an increasing number of parties."]}),"\n",(0,n.jsxs)(s.li,{children:[(0,n.jsx)(s.em,{children:"Deployment Pipeline"}),": The deployment process is embodied in a ",(0,n.jsx)(s.em,{children:"pipeline"})," that describes and implements all the necessary steps to release a new version of Mithril."]}),"\n",(0,n.jsxs)(s.li,{children:[(0,n.jsx)(s.em,{children:"Artifact Promotion"}),": An artifact is built ",(0,n.jsx)(s.em,{children:"once and only once"})," and is ",(0,n.jsx)(s.em,{children:"promoted"})," while travelling through the build pipeline."]}),"\n"]}),"\n",(0,n.jsx)(s.p,{children:"Here is a high-level picture of this process:"}),"\n",(0,n.jsx)(s.p,{children:(0,n.jsx)(s.a,{target:"_blank","data-noBrokenLinkCheck":!0,href:r(50965).A+"",children:(0,n.jsx)(s.img,{alt:"Release Process",src:r(4473).A+"",width:"1220",height:"819"})})}),"\n",(0,n.jsxs)(s.ul,{children:["\n",(0,n.jsxs)(s.li,{children:["We will use a custom version based on ",(0,n.jsx)(s.a,{href:"https://semver.org",children:"SemVer"})," for all the crates, binaries and containers of the repository and for the GitHub release."]}),"\n",(0,n.jsxs)(s.li,{children:["We release a new distribution every 2 weeks (this duration is subject to changes as the project matures)","\n",(0,n.jsxs)(s.ul,{children:["\n",(0,n.jsxs)(s.li,{children:["The released version is named after the year and its week number: ",(0,n.jsx)(s.strong,{children:"YYWW.patch"})," (e.g. ",(0,n.jsx)(s.code,{children:"2250.0"}),")."]}),"\n",(0,n.jsx)(s.li,{children:'In case of critical regressions happening in production, a patch version will be released in between "official" releases as a hotfix.'}),"\n"]}),"\n"]}),"\n",(0,n.jsxs)(s.li,{children:["A new version ",(0,n.jsx)(s.code,{children:"YYWW.0"})," will have the following life cycle:","\n",(0,n.jsxs)(s.ul,{children:["\n",(0,n.jsxs)(s.li,{children:["A commit ",(0,n.jsx)(s.code,{children:"abc123"})," merged on ",(0,n.jsx)(s.code,{children:"main"})," branch is deployed on the network named ",(0,n.jsx)(s.code,{children:"testing-preview"}),"."]}),"\n",(0,n.jsxs)(s.li,{children:["A commit ",(0,n.jsx)(s.code,{children:"def456"})," tagged with ",(0,n.jsx)(s.code,{children:"YYWW.0-prerelease"})," is deployed on the network named ",(0,n.jsx)(s.code,{children:"pre-release-preview"}),"."]}),"\n",(0,n.jsxs)(s.li,{children:["A GitHub release ",(0,n.jsx)(s.code,{children:"YYWW.0-prerelease"})," is created and linked with the ",(0,n.jsx)(s.code,{children:"YYWW.0-prerelease"})," tag and marked as ",(0,n.jsx)(s.code,{children:"pre-release"}),"."]}),"\n",(0,n.jsxs)(s.li,{children:["A tag ",(0,n.jsx)(s.code,{children:"YYWW.0-prerelease"})," is qualified and selected for release or rejected (and replaced by a ",(0,n.jsx)(s.code,{children:"YYWW.1-prerelease"})," tag if necessary on a ",(0,n.jsx)(s.code,{children:"fed789"}),")."]}),"\n",(0,n.jsxs)(s.li,{children:["If the tag ",(0,n.jsx)(s.code,{children:"YYWW.0-prerelease"})," is selected, a new tag is created and name ",(0,n.jsx)(s.code,{children:"YYWW.0"})," on the same commit ",(0,n.jsx)(s.code,{children:"def456"}),"."]}),"\n",(0,n.jsxs)(s.li,{children:["A GitHub release ",(0,n.jsx)(s.code,{children:"YYWW.0"})," is created and linked to the ",(0,n.jsx)(s.code,{children:"YYWW.0"})," tag and marked as ",(0,n.jsx)(s.code,{children:"release"}),"."]}),"\n",(0,n.jsxs)(s.li,{children:["The commit ",(0,n.jsx)(s.code,{children:"def456"})," with tag ",(0,n.jsx)(s.code,{children:"YYWW.0"})," is deployed to the network named ",(0,n.jsx)(s.code,{children:"release-preprod"}),"."]}),"\n"]}),"\n"]}),"\n",(0,n.jsxs)(s.li,{children:["The ",(0,n.jsx)(s.code,{children:"Cargo.toml"})," versions of the crates are updated (if required) just before creating the ",(0,n.jsx)(s.code,{children:"YYWW.0-prerelease"})," tag ."]}),"\n",(0,n.jsxs)(s.li,{children:["The documentation website is also updated at the same time where the ",(0,n.jsx)(s.code,{children:"next"})," version becomes the ",(0,n.jsx)(s.code,{children:"current"})," version, leaving future updates be appended to the ",(0,n.jsx)(s.code,{children:"next"})," version during the upcoming developments."]}),"\n",(0,n.jsxs)(s.li,{children:["In order to simplify the life of Mithril users, we have introduced a version of the ",(0,n.jsx)(s.code,{children:"Mithril API"})," used between client/signer and aggregators to check if the nodes are able to communicate together (following semver and considering the versions are compatible only if they share the same minor)."]}),"\n",(0,n.jsxs)(s.li,{children:["Our main distribution artifact is currently docker (OCI) images. We also provide more packages, eg. ",(0,n.jsx)(s.code,{children:".deb"})," packages or compiled binaries (some of them available on multiple platforms, e.g. Windows or macOS) to simplify users' life."]}),"\n",(0,n.jsxs)(s.li,{children:["We also publish some of our crates on the ",(0,n.jsx)(s.code,{children:"crates.io"})," registry whenever a new version is created (e.g. ",(0,n.jsx)(s.a,{href:"https://crates.io/crates/mithril-stm",children:(0,n.jsx)(s.code,{children:"mithril-stm"})}),")."]}),"\n"]}),"\n",(0,n.jsx)(s.h3,{id:"networks",children:"Networks"}),"\n",(0,n.jsxs)(s.ul,{children:["\n",(0,n.jsxs)(s.li,{children:["We maintain different Mithril networks (eg. servers, VMs, configurations...) to which artifacts are deployed at various stages of the process:","\n",(0,n.jsxs)(s.ul,{children:["\n",(0,n.jsxs)(s.li,{children:[(0,n.jsx)(s.code,{children:"testing-preview"}),": This is an internal environment based on the ",(0,n.jsx)(s.code,{children:"preview"})," cardano testnet where most of the automated tests happen. It is also used to test features as soon as they are merged on the ",(0,n.jsx)(s.code,{children:"main"})," branch."]}),"\n",(0,n.jsxs)(s.li,{children:[(0,n.jsx)(s.code,{children:"pre-release-preview"}),": This is a persistent environment based on the ",(0,n.jsx)(s.code,{children:"preview"})," cardano testnet. SPOs which are active on preview are welcomed to take part in the Mithril signing process and to test new ",(0,n.jsx)(s.code,{children:"pre-release"})," distributions deployed there."]}),"\n",(0,n.jsxs)(s.li,{children:[(0,n.jsx)(s.code,{children:"release-preprod"}),": Another persistent environment, based on the ",(0,n.jsx)(s.code,{children:"preprod"})," cardano testnet, where more SPOs are expected to join and test, updated less frequently (on actual ",(0,n.jsx)(s.code,{children:"release"})," distributions)."]}),"\n",(0,n.jsxs)(s.li,{children:["(",(0,n.jsx)(s.em,{children:"LATER"}),") ",(0,n.jsx)(s.code,{children:"mainnet"}),": Production environment where new releases are deployed once qualifed on ",(0,n.jsx)(s.code,{children:"release-preprod"}),"."]}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,n.jsx)(s.h3,{id:"further-reading",children:"Further Reading"}),"\n",(0,n.jsxs)(s.ul,{children:["\n",(0,n.jsxs)(s.li,{children:["The Mithril developers have redacted an ADR ",(0,n.jsx)(s.a,{href:"https://mithril.network/doc/adr/3/",children:"Release process and versioning"})," that also describes more technically this process."]}),"\n"]})]})}function h(e={}){const{wrapper:s}={...(0,t.R)(),...e.components};return s?(0,n.jsx)(s,{...e,children:(0,n.jsx)(c,{...e})}):c(e)}},50965:(e,s,r)=>{r.d(s,{A:()=>i});const i=r.p+"assets/files/release_process-a9ce55af510cd542b71e68a485251004.jpg"},4473:(e,s,r)=>{r.d(s,{A:()=>i});const i=r.p+"assets/images/release_process-a9ce55af510cd542b71e68a485251004.jpg"},28453:(e,s,r)=>{r.d(s,{R:()=>o,x:()=>l});var i=r(96540);const n={},t=i.createContext(n);function o(e){const s=i.useContext(t);return i.useMemo((function(){return"function"==typeof e?e(s):{...s,...e}}),[s,e])}function l(e){let s;return s=e.disableParentContext?"function"==typeof e.components?e.components(n):e.components||n:o(e.components),i.createElement(t.Provider,{value:s},e.children)}},83612:e=>{e.exports=JSON.parse('{"permalink":"/doc/dev-blog/2022/12/05/release-process","source":"@site/blog/2022-12-05-release-process/index.md","title":"Mithril Release Process","description":"Mithril follows a defined release process","date":"2022-12-05T00:00:00.000Z","tags":[{"inline":true,"label":"process","permalink":"/doc/dev-blog/tags/process"}],"readingTime":3.54,"hasTruncateMarker":false,"authors":[{"name":"Mithril Team","socials":{},"key":null,"page":null}],"frontMatter":{"title":"Mithril Release Process","authors":[{"name":"Mithril Team"}],"tags":["process"]},"unlisted":false,"prevItem":{"title":"Mithril Era Switch","permalink":"/doc/dev-blog/2023/03/02/era-switch-feature"},"nextItem":{"title":"Mithril environments are updated","permalink":"/doc/dev-blog/2022/10/28/updated-environments"}}')}}]);