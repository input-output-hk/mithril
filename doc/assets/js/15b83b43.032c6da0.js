"use strict";(self.webpackChunkmithril_doc=self.webpackChunkmithril_doc||[]).push([[3595],{11870:(e,t,i)=>{i.r(t),i.d(t,{assets:()=>l,contentTitle:()=>a,default:()=>c,frontMatter:()=>s,metadata:()=>n,toc:()=>h});var n=i(81823),r=i(74848),o=i(28453);const s={title:"Mithril internal stores switch to SQLite.",authors:[{name:"Mithril Team"}],tags:["store","sqlite","breaking-change"]},a=void 0,l={authorsImageUrls:[void 0]},h=[{value:"What is that?",id:"what-is-that",level:2},{value:"How to migrate data from old storage system to SQLite stores?",id:"how-to-migrate-data-from-old-storage-system-to-sqlite-stores",level:2},{value:"Downloading",id:"downloading",level:3},{value:"Compiling",id:"compiling",level:3},{value:"Running the migration",id:"running-the-migration",level:3},{value:"Manual migration process",id:"manual-migration-process",level:3}];function d(e){const t={a:"a",code:"code",em:"em",h2:"h2",h3:"h3",p:"p",pre:"pre",strong:"strong",...(0,o.R)(),...e.components};return(0,r.jsxs)(r.Fragment,{children:[(0,r.jsx)(t.h2,{id:"what-is-that",children:"What is that?"}),"\n",(0,r.jsx)(t.p,{children:"Since almost the beginning of the Mithril project, the software used to rely on a store mechanism to save its different states allowing Signers and Aggregators to resume on correct state when switched on and off. This internal store mechanism used to be a bunch of JSON files saved in a given directory. Even though this does the job it still presents flaws: data are hard to query when debugging especially when crossing data (which signers have participated in this multi-signature?). Also, data are stored in different places which can be a problem when moving these files from one place to another. We also had to imagine what would be a migration scenario in case of a structure change. Switching to a file based SQL database solves these issues."}),"\n",(0,r.jsx)(t.p,{children:"The new release now uses SQLite stores in place of JSON file storage. This means that to continue running a Signer or an Aggregator node it is necessary to migrate from the old storage system to SQLite. This release comes with a tool to perform the migration which should be as straightforward as launching a command line (read below). The migration tool will be available only for a limited time in order to make Mithril beta testers able to migrate their existing data."}),"\n",(0,r.jsx)(t.h2,{id:"how-to-migrate-data-from-old-storage-system-to-sqlite-stores",children:"How to migrate data from old storage system to SQLite stores?"}),"\n",(0,r.jsx)(t.p,{children:"There are 2 ways of getting the new version and the associated migration tool. Either downloading binaries from GitHub or compiling them yourself."}),"\n",(0,r.jsx)(t.h3,{id:"downloading",children:"Downloading"}),"\n",(0,r.jsxs)(t.p,{children:["Download the new ",(0,r.jsx)(t.code,{children:"mithril-signer"})," and ",(0,r.jsx)(t.code,{children:"mithril-signer-migrate"})," files from the ",(0,r.jsx)(t.a,{href:"https://github.com/input-output-hk/mithril/releases/tag/unstable",children:"nightly builds page"}),". Make them executable:"]}),"\n",(0,r.jsx)(t.pre,{children:(0,r.jsx)(t.code,{children:"$> chmod +x mithril-signer*\n$> ls -1F mithril-signer*\nmithril-signer*\nmithril-signer-migrate*\n"})}),"\n",(0,r.jsxs)(t.p,{children:[(0,r.jsx)(t.em,{children:"note"}),": the suffix ",(0,r.jsx)(t.code,{children:"*"})," appended to the the entries output above indicates the file is executable. If it is not present, ensure the ",(0,r.jsx)(t.code,{children:"chmod"})," command does not produce any error."]}),"\n",(0,r.jsx)(t.h3,{id:"compiling",children:"Compiling"}),"\n",(0,r.jsxs)(t.p,{children:["If you used to compile your node as stated in the ",(0,r.jsx)(t.a,{href:"https://mithril.network/doc/manual/getting-started/run-signer-node",children:"guide"}),", you have to compile the migration tool as well:"]}),"\n",(0,r.jsx)(t.pre,{children:(0,r.jsx)(t.code,{children:"$> cd mithril-signer\n$> cargo build --all-targets --release\n  Compiling mithril-signer v0.1.0 (/home/somebody/shared/mithril/mithril-signer)\n    Finished release [optimized] target(s) in 4.56s\n$> ls -1F ../target/release/mithril-signer*\n../target/release/mithril-signer*\n../target/release/mithril-signer.d\n../target/release/mithril-signer-migrate*\n../target/release/mithril-signer-migrate.d\n"})}),"\n",(0,r.jsx)(t.h3,{id:"running-the-migration",children:"Running the migration"}),"\n",(0,r.jsxs)(t.p,{children:["The first step is to stop the running Mithril node if any. The ",(0,r.jsx)(t.code,{children:"mithril-signer-migrate"})," executable can perform the migration automatically once you know where your actual JSON files are located. Have a look in your configuration file (default ",(0,r.jsx)(t.code,{children:"/opt/mithril/mithril-signer.env"}),"), check the value associated with the ",(0,r.jsx)(t.code,{children:"DATA_STORES_DIRECTORY"})," key (default to ",(0,r.jsx)(t.code,{children:"/opt/mithril/stores"}),") and copy the path indicated here. Copy this path after the ",(0,r.jsx)(t.code,{children:"--db-dir"})," option on the following command line:"]}),"\n",(0,r.jsx)(t.pre,{children:(0,r.jsx)(t.code,{children:"$> ./mithril-signer-migrate automatic --db-dir /paste/the/data/stores/directory/here\nMithril Aggregator JSON \u2192 SQLite migration tool.\nMigrating protocol_initializer_store data\u2026\nOK \u2713\nMigrating stake_store data\u2026\nOK \u2713\n"})}),"\n",(0,r.jsxs)(t.p,{children:["At the end of this command, a file ",(0,r.jsx)(t.code,{children:"signer.sqlite3"})," (or ",(0,r.jsx)(t.code,{children:"aggregator.sqlite3"})," if you run an Aggregator) should be present in the specified base directory."]}),"\n",(0,r.jsx)(t.p,{children:"That should be enough, launch your upgraded mithril node."}),"\n",(0,r.jsxs)(t.p,{children:[(0,r.jsx)(t.strong,{children:"Note:"})," The migration executable does not remove the old JSON files from the disk."]}),"\n",(0,r.jsx)(t.h3,{id:"manual-migration-process",children:"Manual migration process"}),"\n",(0,r.jsxs)(t.p,{children:["The executable also provides a ",(0,r.jsx)(t.code,{children:"manual"})," switch for migrating Mithril JSON store directories placed in custom directories. This is mainly intended for developers who work on tweaked environments. Each internal store has its own data structure. In order to correctly migrate and process data, the type of the store has to be given on the command line."]}),"\n",(0,r.jsx)(t.pre,{children:(0,r.jsx)(t.code,{children:"$> ./mithril-signer-migrate manual --help\n"})}),"\n",(0,r.jsx)(t.p,{children:"The command above should give you all informations needed to run a custom store migration."}),"\n",(0,r.jsxs)(t.p,{children:["Feel free to reach out to us on the ",(0,r.jsx)(t.a,{href:"https://discord.gg/5kaErDKDRq",children:"Discord channel"})," for questions and/or help."]})]})}function c(e={}){const{wrapper:t}={...(0,o.R)(),...e.components};return t?(0,r.jsx)(t,{...e,children:(0,r.jsx)(d,{...e})}):d(e)}},28453:(e,t,i)=>{i.d(t,{R:()=>s,x:()=>a});var n=i(96540);const r={},o=n.createContext(r);function s(e){const t=n.useContext(o);return n.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function a(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(r):e.components||r:s(e.components),n.createElement(o.Provider,{value:t},e.children)}},81823:e=>{e.exports=JSON.parse('{"permalink":"/doc/dev-blog/2022/09/14/sqlite-stores","source":"@site/blog/2022-09-14-sqlite-stores.md","title":"Mithril internal stores switch to SQLite.","description":"What is that?","date":"2022-09-14T00:00:00.000Z","tags":[{"inline":true,"label":"store","permalink":"/doc/dev-blog/tags/store"},{"inline":true,"label":"sqlite","permalink":"/doc/dev-blog/tags/sqlite"},{"inline":true,"label":"breaking-change","permalink":"/doc/dev-blog/tags/breaking-change"}],"readingTime":3.005,"hasTruncateMarker":false,"authors":[{"name":"Mithril Team","socials":{},"key":null,"page":null}],"frontMatter":{"title":"Mithril internal stores switch to SQLite.","authors":[{"name":"Mithril Team"}],"tags":["store","sqlite","breaking-change"]},"unlisted":false,"prevItem":{"title":"Mithril Keys Certification","permalink":"/doc/dev-blog/2022/10/11/keys-certification-badge"},"nextItem":{"title":"Stake Distribution retrieval fixed","permalink":"/doc/dev-blog/2022/09/13/stake-distribution-retrieval"}}')}}]);