"use strict";(self.webpackChunkmithril_doc=self.webpackChunkmithril_doc||[]).push([[6378],{62602:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>l,contentTitle:()=>s,default:()=>u,frontMatter:()=>o,metadata:()=>n,toc:()=>c});const n=JSON.parse('{"id":"manual/develop/protocol-simulation","title":"Protocol simulation","description":"- This demo will improve your understanding of the Mithril protocol. By engaging with it, you will gain insights into how participants interact to create a multi-signature and understand the impact of the protocol parameters","source":"@site/root/manual/develop/protocol-simulation.md","sourceDirName":"manual/develop","slug":"/manual/develop/protocol-simulation","permalink":"/doc/next/manual/develop/protocol-simulation","draft":false,"unlisted":false,"editUrl":"https://github.com/input-output-hk/mithril/edit/main/docs/website/root/manual/develop/protocol-simulation.md","tags":[],"version":"current","sidebarPosition":3,"frontMatter":{"sidebar_position":3},"sidebar":"manualSideBar","previous":{"title":"Run a private network","permalink":"/doc/next/manual/develop/run-mithril-devnet"},"next":{"title":"API references","permalink":"/doc/next/manual/develop/references"}}');var a=r(74848),i=r(28453);const o={sidebar_position:3},s="Protocol simulation",l={},c=[{value:"Prerequisites",id:"prerequisites",level:2},{value:"Download the source",id:"download-the-source",level:2},{value:"Build the Mithril protocol demo binary",id:"build-the-mithril-protocol-demo-binary",level:2},{value:"Verify the build",id:"verify-the-build",level:2},{value:"Run the simulation",id:"run-the-simulation",level:2},{value:"Case 1: produce a multi-signature",id:"case-1-produce-a-multi-signature",level:3},{value:"Case 2: does not produce a multi-signature",id:"case-2-does-not-produce-a-multi-signature",level:3}];function d(e){const t={a:"a",admonition:"admonition",code:"code",h1:"h1",h2:"h2",h3:"h3",header:"header",li:"li",p:"p",pre:"pre",strong:"strong",ul:"ul",...(0,i.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(t.header,{children:(0,a.jsx)(t.h1,{id:"protocol-simulation",children:"Protocol simulation"})}),"\n",(0,a.jsxs)(t.admonition,{type:"info",children:[(0,a.jsxs)(t.ul,{children:["\n",(0,a.jsxs)(t.li,{children:["\n",(0,a.jsxs)(t.p,{children:["This demo will improve your understanding of the ",(0,a.jsx)(t.strong,{children:"Mithril protocol"}),". By engaging with it, you will gain insights into how participants interact to create a multi-signature and understand the impact of the protocol parameters"]}),"\n"]}),"\n",(0,a.jsxs)(t.li,{children:["\n",(0,a.jsx)(t.p,{children:"This simulation is run by a CLI that you will build and run, ultimately generating real Mithril multi-signatures"}),"\n"]}),"\n",(0,a.jsxs)(t.li,{children:["\n",(0,a.jsx)(t.p,{children:"For reproducibility of the results, the simulation uses a deterministic source of randomness"}),"\n"]}),"\n"]}),(0,a.jsxs)(t.p,{children:["During the simulation, some artifacts, such as ",(0,a.jsx)(t.code,{children:"verification keys"}),", ",(0,a.jsx)(t.code,{children:"individual signatures"}),", and ",(0,a.jsx)(t.code,{children:"multi signatures"}),", will be written to an ",(0,a.jsx)(t.code,{children:"artifacts"})," folder."]})]}),"\n",(0,a.jsx)(t.h2,{id:"prerequisites",children:"Prerequisites"}),"\n",(0,a.jsx)(t.p,{children:"Ensure you have the following:"}),"\n",(0,a.jsxs)(t.ul,{children:["\n",(0,a.jsx)(t.li,{children:"A computer running Linux (preferred) or macOS"}),"\n",(0,a.jsxs)(t.li,{children:["The latest stable version of the correctly configured ",(0,a.jsx)(t.a,{href:"https://www.rust-lang.org/learn/get-started",children:"Rust toolchain"}),"."]}),"\n"]}),"\n",(0,a.jsx)(t.h2,{id:"download-the-source",children:"Download the source"}),"\n",(0,a.jsx)(t.p,{children:"You can download the source file from GitHub (HTTPS):"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-bash",children:"git clone https://github.com/input-output-hk/mithril.git\n"})}),"\n",(0,a.jsx)(t.p,{children:"Or (SSH):"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-bash",children:"git clone git@github.com:input-output-hk/mithril.git\n"})}),"\n",(0,a.jsx)(t.h2,{id:"build-the-mithril-protocol-demo-binary",children:"Build the Mithril protocol demo binary"}),"\n",(0,a.jsx)(t.p,{children:"Change the directory:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-bash",children:"cd mithril/demo/protocol-demo\n"})}),"\n",(0,a.jsx)(t.p,{children:"Run tests (optional):"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-bash",children:"make test\n"})}),"\n",(0,a.jsx)(t.p,{children:"Build the executable:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-bash",children:"make build\n"})}),"\n",(0,a.jsx)(t.h2,{id:"verify-the-build",children:"Verify the build"}),"\n",(0,a.jsx)(t.p,{children:"Check that the Mithril client binary is working correctly by running its help function:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-bash",children:"./mithrildemo -h\n"})}),"\n",(0,a.jsx)(t.p,{children:"You should see:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-bash",children:"mithrildemo\nSimple demonstration of the Mithril protocol\n\nUSAGE:\n    mithrildemo [OPTIONS]\n\nOPTIONS:\n    -h, --help                     Print help information\n    -k, --k <K>                    Quorum parameter [default: 5]\n    -m, --m <M>                    Security parameter, upper bound on indices [default: 200]\n        --nmessages <NMESSAGES>    Number of messages to sign [default: 1]\n        --nparties <NPARTIES>      Number of parties [default: 5]\n        --phi-f <PHI_F>            f in phi(w) = 1 - (1 - f)^w, where w is the stake of a\n                                   participant [default: 0.2]\n"})}),"\n",(0,a.jsx)(t.h2,{id:"run-the-simulation",children:"Run the simulation"}),"\n",(0,a.jsxs)(t.admonition,{type:"tip",children:[(0,a.jsx)(t.p,{children:"A friendly reminder about the protocol parameters:"}),(0,a.jsxs)(t.ul,{children:["\n",(0,a.jsxs)(t.li,{children:[(0,a.jsx)(t.code,{children:"k"}),": the ",(0,a.jsx)(t.code,{children:"quorum"})," parameter represents the minimum number of individual signatures (gathered from multiple participants) required to be aggregated in a multi-signature"]}),"\n",(0,a.jsxs)(t.li,{children:[(0,a.jsx)(t.code,{children:"m"}),": the ",(0,a.jsx)(t.code,{children:"security"})," parameter represents the total number of ",(0,a.jsx)(t.code,{children:"lotteries"})," in which each participant can participate to sign the message"]}),"\n",(0,a.jsxs)(t.li,{children:[(0,a.jsx)(t.code,{children:"phi-f"}),": the parameter that controls the probability of a participant winning a ",(0,a.jsx)(t.code,{children:"lottery"}),"; it varies between ",(0,a.jsx)(t.code,{children:"0.0"})," (less chance) and ",(0,a.jsx)(t.code,{children:"1.0"})," (more chance)."]}),"\n"]})]}),"\n",(0,a.jsxs)(t.admonition,{type:"danger",children:[(0,a.jsxs)(t.p,{children:["The ",(0,a.jsx)(t.code,{children:"security level"})," of the protocol is highly dependent on the value of the ",(0,a.jsx)(t.code,{children:"protocol parameters"}),"."]}),(0,a.jsx)(t.p,{children:"Therefore, these protocol parameters will be carefully selected by Mithril cryptographers and researchers to guarantee that only genuine stakeholders representing a sufficient threshold of the total stake can combine their individual signatures in a valid multi-signature."})]}),"\n",(0,a.jsx)(t.h3,{id:"case-1-produce-a-multi-signature",children:"Case 1: produce a multi-signature"}),"\n",(0,a.jsxs)(t.p,{children:["Run the simulation with ",(0,a.jsx)(t.code,{children:"5"})," participants:"]}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-bash",children:"./mithrildemo -k 5 -m 50 --phi-f 0.65 --nparties 5\n"})}),"\n",(0,a.jsx)(t.p,{children:"The simulation should succeed and produce (or aggregate) a multi-signature:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-bash",children:'>> Launch Mithril protocol demonstrator with configuration:\nConfig {\n    m: 50,\n    k: 5,\n    phi_f: 0.65,\n    nparties: 5,\n    nmessages: 1,\n}\n\n>> Protocol establishment phase\nParty #0: party created with 826 stakes\nParty #1: party created with 741 stakes\nParty #2: party created with 144 stakes\nParty #3: party created with 734 stakes\nParty #4: party created with 41 stakes\nProtocol established to StmParameters { m: 50, k: 5, phi_f: 0.65 }\n\n>> Protocol initialization phase:\nVerifier: verifier created\nVerifier: protocol params updated to StmParameters { m: 50, k: 5, phi_f: 0.65 }\nParty #0: protocol params updated to StmParameters { m: 50, k: 5, phi_f: 0.65 }\nParty #1: protocol params updated to StmParameters { m: 50, k: 5, phi_f: 0.65 }\nParty #2: protocol params updated to StmParameters { m: 50, k: 5, phi_f: 0.65 }\nParty #3: protocol params updated to StmParameters { m: 50, k: 5, phi_f: 0.65 }\nParty #4: protocol params updated to StmParameters { m: 50, k: 5, phi_f: 0.65 }\nVerifier: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]\nParty #0: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]\nParty #1: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]\nParty #2: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]\nParty #3: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]\nParty #4: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]\nArtifacts written to artifacts/parties-keys.json\n\n>> Protocol operations phase:\nMessage #0 to sign: [119, 36, 224, 63, 184, 216, 74, 55, 106, 67, 184, 244, 21, 24, 161, 28]\nParty #0: sign message 7724e03fb8d84a376a43b8f41518a11c\nParty #0: lottery #2 won\nParty #0: lottery #3 won\nParty #0: lottery #8 won\nParty #0: lottery #13 won\nParty #0: lottery #16 won\nParty #0: lottery #17 won\nParty #0: lottery #19 won\nParty #0: lottery #23 won\nParty #0: lottery #25 won\nParty #0: lottery #28 won\nParty #0: lottery #29 won\nParty #0: lottery #31 won\nParty #0: lottery #42 won\nParty #0: lottery #43 won\nParty #0: lottery #46 won\nParty #1: sign message 7724e03fb8d84a376a43b8f41518a11c\nParty #1: lottery #2 won\nParty #1: lottery #3 won\nParty #1: lottery #8 won\nParty #1: lottery #13 won\nParty #1: lottery #16 won\nParty #1: lottery #17 won\nParty #1: lottery #19 won\nParty #1: lottery #23 won\nParty #1: lottery #25 won\nParty #1: lottery #29 won\nParty #1: lottery #31 won\nParty #1: lottery #42 won\nParty #1: lottery #43 won\nParty #1: lottery #46 won\nParty #2: sign message 7724e03fb8d84a376a43b8f41518a11c\nParty #2: lottery #19 won\nParty #2: lottery #43 won\nParty #2: lottery #46 won\nParty #3: sign message 7724e03fb8d84a376a43b8f41518a11c\nParty #3: lottery #2 won\nParty #3: lottery #3 won\nParty #3: lottery #8 won\nParty #3: lottery #13 won\nParty #3: lottery #16 won\nParty #3: lottery #17 won\nParty #3: lottery #19 won\nParty #3: lottery #23 won\nParty #3: lottery #25 won\nParty #3: lottery #29 won\nParty #3: lottery #31 won\nParty #3: lottery #42 won\nParty #3: lottery #43 won\nParty #3: lottery #46 won\nParty #4: sign message 7724e03fb8d84a376a43b8f41518a11c\nParty #4: lottery #19 won\nParty #0: aggregate signature computed\nParty #1: aggregate signature computed\nParty #2: aggregate signature computed\nParty #3: aggregate signature computed\nParty #4: aggregate signature computed\nArtifacts written to artifacts/single-signatures.json\nArtifacts written to artifacts/multi-signatures.json\n\n>> Protocol certificate verification phase:\nMessage #0 to verify: 7724e03fb8d84a376a43b8f41518a11c\nParty #0: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!\nVerifier: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!\nParty #1: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!\nVerifier: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!\nParty #2: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!\nVerifier: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!\nParty #3: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!\nVerifier: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!\nParty #4: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!\nVerifier: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!\n\n>> Congratulations, the protocol terminated with success.\n'})}),"\n",(0,a.jsx)(t.h3,{id:"case-2-does-not-produce-a-multi-signature",children:"Case 2: does not produce a multi-signature"}),"\n",(0,a.jsxs)(t.p,{children:["Run the simulation with ",(0,a.jsx)(t.code,{children:"5"})," participants:"]}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-bash",children:"./mithrildemo -k 5 -m 5 --phi-f 0.25 --nparties 5\n"})}),"\n",(0,a.jsx)(t.p,{children:"The simulation should fail and not produce (or aggregate) any multi-signature:"}),"\n",(0,a.jsx)(t.pre,{children:(0,a.jsx)(t.code,{className:"language-bash",children:'>> Launch Mithril protocol demonstrator with configuration:\nConfig {\n    m: 5,\n    k: 5,\n    phi_f: 0.25,\n    nparties: 5,\n    nmessages: 1,\n}\n\n>> Protocol establishment phase:\nParty #0: party created with 826 stakes\nParty #1: party created with 741 stakes\nParty #2: party created with 144 stakes\nParty #3: party created with 734 stakes\nParty #4: party created with 41 stakes\nProtocol established to StmParameters { m: 5, k: 5, phi_f: 0.25 }\n\n>> Protocol initialization phase:\nVerifier: verifier created\nVerifier: protocol params updated to StmParameters { m: 5, k: 5, phi_f: 0.25 }\nParty #0: protocol params updated to StmParameters { m: 5, k: 5, phi_f: 0.25 }\nParty #1: protocol params updated to StmParameters { m: 5, k: 5, phi_f: 0.25 }\nParty #2: protocol params updated to StmParameters { m: 5, k: 5, phi_f: 0.25 }\nParty #3: protocol params updated to StmParameters { m: 5, k: 5, phi_f: 0.25 }\nParty #4: protocol params updated to StmParameters { m: 5, k: 5, phi_f: 0.25 }\nVerifier: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]\nParty #0: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]\nParty #1: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]\nParty #2: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]\nParty #3: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]\nParty #4: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]\nArtifacts written to artifacts/parties-keys.json\n\n>> Protocol operations phase:\nMessage #0 to sign: [119, 36, 224, 63, 184, 216, 74, 55, 106, 67, 184, 244, 21, 24, 161, 28]\nParty #0: sign message 7724e03fb8d84a376a43b8f41518a11c\nParty #1: sign message 7724e03fb8d84a376a43b8f41518a11c\nParty #2: sign message 7724e03fb8d84a376a43b8f41518a11c\nParty #3: sign message 7724e03fb8d84a376a43b8f41518a11c\nParty #4: sign message 7724e03fb8d84a376a43b8f41518a11c\nParty #0: not enough signatures to compute aggregate\nParty #1: not enough signatures to compute aggregate\nParty #2: not enough signatures to compute aggregate\nParty #3: not enough signatures to compute aggregate\nParty #4: not enough signatures to compute aggregate\nArtifacts written to artifacts/single-signatures.json\nArtifacts written to artifacts/multi-signatures.json\n\n>> Protocol certificate verification phase:\nMessage #0 to verify: 7724e03fb8d84a376a43b8f41518a11c\nParty #0: aggregate signature not found 7724e03fb8d84a376a43b8f41518a11c\n\n>> Certificate verification failed: aggregate signature not found.\n'})}),"\n",(0,a.jsx)(t.admonition,{type:"tip",children:(0,a.jsxs)(t.p,{children:["For more information about the Mithril protocol, refer to the ",(0,a.jsx)(t.a,{href:"/doc/next/mithril/advanced/mithril-protocol/protocol",children:"about Mithril"})," section."]})})]})}function u(e={}){const{wrapper:t}={...(0,i.R)(),...e.components};return t?(0,a.jsx)(t,{...e,children:(0,a.jsx)(d,{...e})}):d(e)}},28453:(e,t,r)=>{r.d(t,{R:()=>o,x:()=>s});var n=r(96540);const a={},i=n.createContext(a);function o(e){const t=n.useContext(i);return n.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function s(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:o(e.components),n.createElement(i.Provider,{value:t},e.children)}}}]);