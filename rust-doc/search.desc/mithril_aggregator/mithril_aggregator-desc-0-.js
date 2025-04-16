searchState.loadedDescShard("mithril_aggregator", 0, "Mithril aggregator The Aggregator is responsible for:\nConfiguration structure dedicated to the AggregatorRuntime.\nThe runner responsibility is to expose a code API for the …\nThis trait is intended to allow mocking the …\nThe AggregatorRuntime responsibility is to create a state …\nArtifactBuilder is trait for building an artifact\nA SignersImporterRetriever fetching signers data from …\nCommand that outputs some result after execution\nIdentifies the type of command\nAggregator configuration\nA Critical error means the Runtime stops and the software …\nDefault configuration with all the default values for …\nDummy uploader for test purposes.\nStore and get aggregator epoch settings for given epoch.\nDifferent kinds of execution environments\nFileUploader represents a file uploader interactor. It …\nAdapter to convert RegisterSignerMessage to Signer …\nUploader to GCP storage.\nA trait for mapping ImmutableFileNames to their digests.\nErrors that need the runtime to try again without changing …\nUploader to local storage.\nMithril Aggregator Node\nMultiSigner is the cryptographic engine in charge of …\nMultiSignerImpl is an implementation of the MultiSigner\nProduction environment, minimum logging, maximum …\nAn error that needs to re-initialize the state machine.\nError encountered or produced by the Runtime. This enum …\nCommand that runs a server\nTool that can import a list of signers\nTrait that define how a SignersImporter persist the …\nTrait that define how a SignersImporter retrieve the …\nAuthenticates single signatures against a signed message.\nUploader needed to copy the snapshot once computed.\nTest environment, maximum logging, memory stores etc.\nStore and get signers verification keys for given epoch.\nZstandard specific parameters\nCheck if the HTTP server can serve static directories.\nIf set no error is returned in case of unparsable block …\nIf set no error is returned in case of unparsable block …\nConfiguration of the ancillary files signer\nAuthenticates a single signature against a signed message.\nCardano CLI tool path\nPath of the socket used by the Cardano CLI tool to …\nCardano node version.\nThe maximum number of roll forwards during a poll of the …\nThe maximum number of roll forwards during a poll of the …\nCardano transactions database connection pool size\nCardano transactions database connection pool size\nCardano transactions prover cache pool size\nCardano transactions prover cache pool size\nMaximum number of transactions hashes allowed by request …\nMaximum number of transactions hashes allowed by request …\nCardano transactions signing configuration\nCardano transactions signing configuration\nUrl to CExplorer list of pools to import as signer in the …\nCardano chain observer type\nChain observer type\nClose the signer registration round of an epoch.\napplication main command\nCompute the list of signed entity discriminants that are …\nCompute an artifact\nWhite list for origin client request.\nCompute the protocol message\nDirectory where configuration file is located\nCreate an artifact and persist it.\nTell the certifier to try to create a new certificate.\nCreates a multi signature from single signatures\nCreates a multi signature from single signatures\nCreate new open message\nCreate a new Critical error\nCustom origin tag of client request added to the whitelist …\nPerform one tick of the state machine.\nDirectory to store aggregator data (Certificates, …\ndatabase module. This module contains the entities …\nDirectory of the Cardano node files\nDirectory of the Cardano node store.\nDirectory of the Cardano node database\nDependency injection module. This module provides tools to …\nUse the digest caching strategy\nImmutableDigesterCacheProvider default setting\nEnable metrics server (Prometheus endpoint on /metrics).\nEnable metrics server (Prometheus endpoint on /metrics).\nEntities module\nWhat kind of runtime environment the configuration is …\nExecution environment\nEra reader adapter parameters\nEra reader adapter type\nEra reader adapter type\nEvent Store module This module proposes tools to send …\nexecute command\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nGenesis verification key\nRetrieves the current non-certified open message.\nRetrieves the current open message for a given signed …\nGet the saved <code>AggregatorEpochSettings</code> for the given Epoch …\nInfer the AggregatorEpochSettings from the configuration.\nAssociate each given immutable files with a digest.\nReturn the last <code>n</code> uploads that were triggered in …\nReturn the last upload that was triggered.\nBuild the local server URL from configuration.\nCheck configuration and return a representation of the …\nGet the saved <code>ProtocolParameter</code> for the given Epoch if any.\nGet the server URL from the configuration.\nReturns the list of signers for the given <code>epoch</code>.\nReturn the snapshots directory.\nReturn the directory of the SQLite stores. If the …\nReturn the actual state of the state machine.\nReturn the current TimePoint from the chain\nReturn the current time point from the chain\nReturns a HashMap of Signer indexed by PartyId for the …\nHandle discrepancies at startup in the epoch settings …\nHandle discrepancies at startup in the epoch settings …\nIncrement the runtime cycle success metric.\nIncrement the runtime cycle total metric.\nAsk services to update themselves for the new epoch\nInterval between each snapshot, in ms\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCheck if a certificate chain is valid.\nEasy matching Critical errors.\nWhether the aggregator is a follower\nCheck if the aggregator is running in follower mode.\nCheck if the follower aggregator is running the same epoch …\nChecks if the open message is considered outdated.\nCreate a new KeepState error\nThis is the endpoint of the aggregator that will be used …\nLevel of compression, default to 9.\nget log level from parameters\nMark expired open message.\nmetrics module. This module contains the aggregator …\nMetrics HTTP Server IP.\nMetrics HTTP server IP.\nMetrics HTTP Server listening port.\nMetrics HTTP server listening port.\nCardano network\nCardano Network Magic number\nCreate a new instance with a custom retry policy.\nMultiSignerImpl factory\nCreate a new instance of the Aggregator Runner.\nCreate a new instance of the state machine.\nSignersImporter factory\nCreates a new <code>SingleSignatureAuthenticator</code>.\nCreate a new instance of AggregatorConfig.\nCreate a sample configuration mainly for tests\nNumber of workers when compressing, 0 will disable …\nOpen the signer registration round of an epoch.\nPersist the given list of signers.\nTime interval at which usage metrics are persisted in …\nTime interval at which metrics are persisted in event …\nPrecompute what doesn’t change for the actual epoch\nProtocol parameters\nPrune all verification keys that are at or below the given …\nServer URL that can be accessed from the outside\nShould the immutable cache be reset or not\nImmutableDigesterCacheProvider default setting\nRetrieve the signers list.\nGet the retry policy for this uploader.\nGet the retry policy for this uploader.\nLaunches an infinite loop ticking the state machine.\nImport and persist the signers\nStart a loop that call run at the given time interval.\nRun Interval is the interval between two runtime cycles in …\nRun Mode\nSame as the store retention limit but will never yield a …\nSave the given <code>AggregatorEpochSettings</code> for the given Epoch.\nSave the verification key, for the given Signer for the …\nServer listening IP\nServer listening IP\nServer listening port\nServer listening port\nServices\nSigned entity types parameters (discriminants names in an …\nTime interval at which the signers in …\nSigner importer run interval default setting\nBucket name where the snapshots are stored if …\nCompression algorithm used for the snapshot archive …\nSnapshot compression algorithm default setting\nDirectory to store snapshot\nDirectory to store snapshot\nType of snapshot store to use\nType of snapshot uploader to use\nType of snapshot uploader to use\nUse CDN domain to construct snapshot urls if …\nUse CDN domain to construct snapshot urls default setting …\nMax number of records in stores. When new records are …\nSynchronize the follower aggregator signer registration.\nMethod to trigger the conversion.\nMatch the given result and do an early return with an …\nAsk the EpochService to update the epoch settings.\nUpdate the EraChecker with EraReader information.\nRead the stake distribution from the blockchain and store …\nPerform the upkeep tasks.\nUpload a file with retries according to the retry policy.\nUpload a file with retries according to the retry policy.\nTry to upload once.\nUpload a file\nVerbosity level\nVerify a single signature\nVerify a single signature\nVerify a single signature using the stake distribution of …\nWrite the error to the given logger.\nSpecific parameters when snapshot_compression_algorithm is …\nerror message\nerror message\nerror message\nEventual caught error\nEventual caught error\nEventual caught error\nMigration module\nAggregator related database records\nAggregator related database repositories\nGet all the migrations required by this version of the …\n<code>BufferedSingleSignatureRecord</code> record is the representation …\nCertificate record is the representation of a stored …\nSettings for an epoch, including the protocol parameters.\nImmutableFileDigestRecord is the record that stores the …\nOpenMessage\nOpen Message with associated single signatures if any.\nSignedEntity record is the representation of a stored …\nSigner record is the representation of a stored signer.\nSignerRegistration record is the representation of a …\nSingleSignature record is the representation of a stored …\nStake pool as read from Chain.\nAggregate verification key Note: used only if signature is …\nRaw artifact (in JSON format).\nCardano transactions signing configuration.\nCertificate id.\nCertificate id for this signed entity.\nDate and time when the buffered single signature was …\nMessage creation datetime, it is set by the database.\nMessage creation datetime, it is set by the database.\nDate and time when the signed_entity was created\nDate and time when the signer was created.\nDate and time when the signer_registration was created\nDate and time when the single_signature was created\nDateTime of the record creation.\nDigest of an immutable file\nEpoch of creation of the certificate.\nEpoch\nEpoch\nEpoch at which this pool is valid.\nEpoch settings id, i.e. the epoch number.\nEpoch of creation of the signer_registration.\nConstruct a Projection that will allow to hydrate this …\nMessage expiration datetime, if it exists.\nMessage expiration datetime, if it exists.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nImmutable file name\nDate and time when the certificate was initiated\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nHas this open message been converted into a certificate?\nHas this message been converted into a Certificate?\nHas this open message expired\nHas this open message expired\nThe kes period used to compute the verification key …\nDate and time when the signer registered for the last time.\nLottery indexes\nLottery indexes\nMessage that is signed.\nCardano network of the certificate.\nOpenMessage unique identifier\nOpenMessage unique identifier\nOpen message id.\nOperational certificate of the stake pool operator …\nParent Certificate id.\nParty id.\nPool ticker of the signer.\nStructured message that is used to create the signed …\nMessage used by the Mithril Protocol\nMessage used by the Mithril Protocol\nProtocol parameters.\nProtocol parameters.\nProtocol Version (semver)\nRegistration epoch setting id\nDate and time when the certificate was sealed\nThe STM single signature of the message\nSignature of the certificate. Note: multi-signature if …\nThe STM single signature of the message\nSigned entity id.\nSigned entity type of the message\nType of message\nType of message\nSigned entity type.\nSigned entity type discriminant.\nSigner id.\nSigner id.\nSigner id.\nThe list of the active signers with their stakes\nassociated single signatures\nThe stake associated to the signer\nTotal stake of this pool.\nPool Id\nDate and time when the signer was updated.\nVerification key of the signer\nSignature of the verification key of the signer\nAn implementation of BufferedSingleSignatureStore that …\nDatabase frontend API for Certificate queries.\nService to deal with epoch settings (read &amp; write).\nImmutableFileDigestRepository store for the immutable file …\nOpen message repository\nService to deal with signed_entity (read &amp; write).\nSigned entity storer trait\nService to get SignerRecord.\nService to deal with signer_registration (read &amp; write).\nService to deal with signer (read &amp; write).\nService to deal with single_signature (read &amp; write).\nService to deal with stake pools (read &amp; write).\nRemove all the OpenMessageRecord for the strictly previous …\nCreate a new certificate in the database.\nCreate many certificates at once in the database.\nCreate a new OpenMessageRecord in the database.\nCreate a new Single Signature in database\nDelete all ImmutableFileDigestRecord from the database.\nDelete all the given certificates from the database\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturn all stored records.\nReturn all the ImmutableFileDigestRecords.\nGet Cardano stake distribution signed entity by epoch\nReturn the certificate corresponding to the given hash if …\nReturn the expired OpenMessageRecord for the given Epoch …\nReturn the ImmutableFileDigestRecord for the given …\nGet last signed entities by signed entity type\nReturn the latest certificates.\nReturn the first certificate signed per epoch as the …\nReturn the latest OpenMessageRecord for the given Epoch …\nReturn an open message with its associated single …\nGet signed entities type by certificates ids\nGet signed entity type\nGet signed entity type by certificate id\nCreate many signers at once in the database, their …\nImport a signer in the database, its last_registered_at …\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCreates a new BufferedSingleSignatureRepository instance.\nInstantiate a new repository\nCreate a new EpochSettings store\nInstantiate service\nInstantiate service\nCreate a new SignedEntityStoreAdapter service\nCreate a new SignerRegistrationStore service\nCreate a new SignerStore service\nCreate a new SingleSignatureStoreAdapter service\nCreate a new StakePool service\nPrune useless old epoch settings.\nStore a signed entity\nUpdates an OpenMessageRecord in the database.\nPerform an update for all the given signed entities.\nCreate a new ImmutableFileDigestRecord in the database.\nDependencies container builder\nError that can occur during dependencies initialization …\nDependencyManager handles the dependencies\nEpochServiceWrapper wraps a EpochService\nContains the error value\nThe dependency has reached a state where dependencies are …\nUnrecoverable system initialization failure\nConfiguration parameter missing for initialization.\nContains the success value\nResult with the DependenciesBuilderError error.\nAPI Version provider\nAPI Version provider\nCardano block scanner.\nCardano block scanner.\nCreate CertifierService service\nReturn an unconfigured DependencyContainer\nbuild an AggregatorClient\nbuild HTTP message service\nBuild Prover service\nCreate TickerService instance.\nCardano CLI Runner for the ChainObserver\nCertificate repository.\nCertificate store.\nCertificate verifier service.\nCertificate verifier service.\nCertifier service\nCertifier Service\nChain block reader\nChain observer service.\nChain observer service.\nConfiguration parameters\nCreate the AggregatorRunner\nCreate a CardanoTransactionsPreloader instance.\nCreate dependencies for the EventStore task.\nCreate dependencies for genesis commands\nCreate the HTTP route instance\nCreate a SignersImporter instance.\nCreate a UsageReporter instance.\nDigester service.\nDigester service.\nExecute cleanup operations on SQLite connections\nEpoch service.\nEpoch service\nEpoch settings store.\nEpoch settings storer.\nEra checker service\nEra checker service\nEra reader service\nEra reader service\nAdapter for EraReader\nEvent Transmitter Service\nEvent Transmitter Service\nEvent transmitter Channel Sender endpoint\nFile archiver service.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nGenesis signature verifier service.\nGenesis signature verifier service.\nAPIVersionProvider service\nBlock scanner\nReturn a CardanoCliRunner\nGet a configured CertificateRepository.\nCertificateVerifier service.\nCertifierService service\nChain reader\nReturn a ChainObserver\nEpochService service\nGet a configured EpochSettingsStorer.\nEraReader service\nEraReader service\nGet EventStore SQLite connection\nTransmitterService service\nReturn the channel receiver setup for the EventStore. …\nReturn the EventMessage channel sender.\nReturn a ProtocolGenesisVerifier\nGet an ImmutableFileDigestCacheProvider\nImmutable digester.\nImmutable digest mapper.\nReturn a ImmutableFileObserver instance.\nReturns a leader AggregatorClient\nMessageService service\nMetricsService service\nReturn a MithrilSignerRegistrationFollower\nReturn a MithrilSignerRegistrationLeader\nGet a configured multi signer\nGet a configured OpenMessageRepository.\nProverService service\nSignableBuilderService service\nSignableSeedBuilder service\nGet the SignedEntityTypeLock instance\nSignedEntityService service\nSignedEntityStorer service\nReturn a SignerRegisterer\nReturn a EpochPruningTask instance\nReturn a SignerRegistrationRoundOpener\nReturn a SignerRegistrationVerifier\nSignerStore service\nReturn a SignerSynchronizer\nSingleSignatureAuthenticator service\nGet a FileUploader\nSnapshotter service.\nGet SQLite connection\nGet SQLite connection pool for the cardano transactions …\nStakeDistributionService service\nReturn a StakePoolStore\nTickerService service\nTransaction repository.\nGet the TransactionsImporter instance\nGet the UpkeepService instance\nGet a configured VerificationKeyStorer.\nImmutable cache provider service.\nImmutable file digester service.\nImmutable file digest mapper service.\nImmutable file observer service.\nImmutable file observer service.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nLeader aggregator client\nLeader aggregator client\nHTTP Message service\nHTTP message service\nMetrics service\nMetrics service\nMithril signer registration follower service\nMithril signer registration leader service\nMultisigner service.\nMultisigner service.\nCreate a new clean dependency builder\nOpen message repository.\nOpen message store.\nProver service\nProver service\nReturn a copy of the root logger.\nApplication root logger\nApplication root logger\nSignable Builder Service\nSignable Builder Service\nSignable Seed Builder\nSigned Entity Service\nSigned Entity Service\nSigned Entity storer\nSigned Entity storer\nSigned Entity Type Lock\nSigned Entity Type Lock\nSigner getter service\nSigner Recorder\nSigner registerer service\nSigner registerer service\nSigner registration round opener service\nSigner registration round opener service\nSigner registration verifier\nSigner registration verifier\nSigner Store\nSigner synchronizer service\nSigner synchronizer service\nSingle signer authenticator\nSingle signer authenticator\nSnapshot uploader service.\nSnapshot uploader service.\nSnapshotter service.\nSnapshotter service.\nSQLite database connection\nSQLite database connection\nCardano transactions SQLite database connection pool\nCardano transactions SQLite database connection pool\nEvent store SQLite database connection\nStake Distribution Service\nStake Distribution Service\nStake Store used by the StakeDistributionService It shall …\nStake Store used by the StakeDistributionService It shall …\nTicker Service\nTicker Service\nCardano transactions repository.\nCardano transactions store.\nTransactions Importer\nUpkeep service\nUpkeep service\nRemove the dependencies builder from memory to release Arc …\nVerification key store.\nVerification key store.\nEventual nested error\nError context message\nAggregatorEpochSettings represents the settings of an epoch\nLeaderAggregatorEpochSettings represents the settings of …\nOpenMessage\nMessage structure of a signer registration\nMessage structure of signer registrations for an epoch.\nMessage structure of a known signer\nMessage structure of signers known by the aggregator\nCardano transactions signing configuration\nCardano transactions signing configuration for the current …\nMessage creation datetime\nCurrent Signers\nCurrent Epoch\nEpoch\nMessage expiration datetime, if it exists.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nGather all signers party_id for this open message\nTrue if the signer have registered at least once\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nHas this message been converted into a Certificate?\nHas this open message expired\nCardano network of the aggregator\nBuild a SignerRegistrationsMessage from a list of signers …\nCardano transactions signing configuration for the next …\nSigners that will be able to sign on the next epoch\nThe registered signer party id\nThe signer party id\nThe signer pool ticker\nMessage used by the Mithril Protocol\nProtocol parameters\nThe epoch at which the registration was sent.\nRegistration protocol parameters\nThe signer registrations\nType of message\nKnown signers\nThe epoch at which the registration was able to send …\nassociated single signatures\nThe registered signer stake\nEvent persisted in the Event Store.\nEvent that is sent from a thread to be persisted.\nEventMessage receiver service.\nThe transmitter service is used to allow inter process …\nthe <code>action</code> of the original EventMessage this Event …\nThe action represent the action that is going to be …\nthe <code>content</code> of the original EventMessage this Event …\nJSON content of the message, its type is declared in the …\ntimestamp of event creation in the database.\ndatabase module. This module contains the entities …\nSequential number of the event, this is set by the …\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nClone the internal transmitter and return it.\nHeaders\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nInstantiate the EventMessage receiver service.\nInstantiate a new Service by passing a MPSC transmitter.\nInstantiate a new EventMessage.\nLaunch the service. It runs until all the transmitters are …\nSend an EventMessage. An error when sending a message has …\nCreate a signer registration event message.\nthe <code>source</code> of the original EventMessage this Event …\nThe source of the message shall be composed of the name of …\nSend an EventMessage.\nThe EventPersister is the adapter to persist EventMessage …\nReturns the argument unchanged.\nCalls <code>U::from(self)</code>.\nMigration module\nInstantiate an EventPersister\nSave an EventMessage in the database.\nGet all the migrations required by this version of the …\nMetrics service which is responsible for recording and …\nExport metrics in map.\nReturns the argument unchanged.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nGet the <code>$metric_attribute</code> counter.\nCalls <code>U::from(self)</code>.\nCreate a new MetricsService instance.\nAdapter error\nTrait for mocking and testing a <code>AggregatorClient</code>\nError structure for the Aggregator Client.\nAggregatorHTTPClient is a http client for an aggregator\nSignableSeedBuilder aggregator implementation\nImplementation of the upkeep service for the aggregator.\nThe open message is already certified, no more single …\nIncompatible API version error\nThe signature was buffered, it will be used later.\nA decorator of CertifierService that can buffer …\nBufferedSingleSignatureStore\nThe stake distribution compute is in progress for this …\nImport and store CardanoTransaction.\nNo certificate for this epoch\nCertifierService\nErrors dedicated to the CertifierService.\nChain observer error.\nCompressed Archive Snapshotter create a compressed file.\nCould not verify certificate chain because could not find …\nSnapshotter that does nothing. It is mainly used for test …\nDefine the task responsible for pruning a datasource below …\nService that aggregates all data that don’t change in a …\nEpoch service.\nDependencies required by the MithrilEpochService.\nErrors dedicated to the CertifierService.\nSigner is already registered.\nThe open message is expired, no more single signatures may …\nFailed fetching leader aggregator epoch settings.\nSigner recorder failed.\nSigner registration failed.\nSnapshotter that writes empty files to the filesystem. …\nHTTP client creation error\nMostly network errors.\nAn invalid signature was provided.\nCould not parse response.\nHTTP Message service trait.\nMithril CertifierService implementation\nImplementation of the epoch service.\nImplementation of the MessageService\nMithril prover\nMithril ArtifactBuilder Service\nA MithrilSignerRegistrationFollower supports signer …\nA MithrilSignerRegistrationLeader supports signer …\nImplementation of a SignerRegistrationVerifier\nImplementation of the stake distribution service.\nNo parent certificate could be found, this certifier …\nOpenMessage not found.\nRaised when service has not computed data for its current …\nRaised when service has not collected data at least once.\nProver service is the cryptographic engine in charge of …\nProxy creation error\nThe signature was registered and will be used for the next …\nSigner registration is always closed on a follower …\nNo signer registration round opened yet\nRegistration round for unexpected epoch\nThe aggregator host responded it cannot fulfill our …\nThe aggregator host has returned a technical error.\nCould not reach aggregator.\nStatus of a successful registration of a single signature.\nArtifactBuilder Service trait\nArtifactsBuilder dependencies required by the …\nSigner recorder trait\nTrait to register a signer\nError type for signer registerer service.\nRepresents the information needed to handle a signer …\nTrait to open a signer registration round\nA trait for verifying a Signer registration.\nSigner synchronization is not available on a leader …\nTrait to synchronize signers\nDefine the ability to create snapshots.\nResponsible of synchronizing with Cardano stake …\nErrors related to the StakeDistributionService.\nStore.\nCritical errors cannot be recovered.\nCardano transactions store\nTransactions retriever\nThe stake distribution for the given Epoch is not …\nOne of the data that is held for an epoch duration by the …\nUnhandled status code\nDefine the service responsible for the upkeep of the …\nReporter of usage metrics of the application.\nTools for signing generating signed manifests of ancillary …\nBuffer a single signature for later use.\nCheck if the signers can be synchronized\nGet the current Cardano era.\nClose a signer registration round\nReturn the compression algorithm used by the snapshotter.\nCompute the cache\nCompute the total and average uncompressed size of all …\nCompute the cryptographic proofs for the given transactions\nCreate artifact for a signed entity type and a certificate\nCreate a certificate if possible. If the pointed open …\nCreate a new EventMessage for a metrics.\nCreate an open message at the given beacon. If the open …\nGet aggregate verification key for current epoch\nGet cardano transactions signing configuration used in …\nGet protocol parameters used for signing in the current …\nGet signers for the current epoch\nGet signers with stake for the current epoch\nRegistration round epoch\nGet the current epoch for which the data stored in this …\nEpoch service\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nCreate an <code>AggregatorClientError</code> from a response.\nGet the buffered single signatures for the given signed …\nGet by block ranges\nGet a list of transactions by hashes using chronological …\nReturn the list of the Cardano database immutable file …\nReturn the list of the last Cardano database message.\nReturn the information regarding the Cardano database for …\nReturn the list of the last Cardano stake distributions …\nReturn the information regarding the Cardano stake …\nReturn the information regarding the Cardano stake …\nReturn the list of the last Cardano transactions set …\nReturn the information regarding the Cardano transactions …\nReturns a certificate from its hash.\nReturn the message representation of the last N …\nReturn the message representation of a certificate if it …\nGet current open round if exists\nReturn the epoch settings message if it exists.\nGet the highest known transaction beacon\nGet the highest stored block range root bounds\nReturn the last signed Cardano Transaction Snapshot.\nReturn a list of Cardano Database snapshots order by …\nReturn a list of signed Cardano stake distribution ordered …\nReturn a list of signed Mithril stake distribution ordered …\nReturn a list of signed snapshots order by creation date …\nReturn the last fake snapshot produced.\nReturns the list of the latest created certificates.\nReturn the list of the last Mithril stake distributions …\nReturn the information regarding the Mithril stake …\nReturn the open message at the given Beacon. If the …\nReturn a Cardano Database snapshot\nReturn a signed Mithril stake distribution\nReturn a signed snapshot\nReturn the list of the last signed snapshots. The limit of …\nReturn the information regarding the given snapshot.\nReturn the stake distribution fot the given epoch.\nGet transactions in an interval of blocks\nInform the certifier I have detected a new epoch, it may …\nInform the service a new epoch has been detected, telling …\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nMark the open message if it has expired.\nGet the current Mithril era.\nAggregatorHTTPClient factory\nCreate a new instance of <code>BufferedCertifierService</code>.\ninstantiate the service\nCreate a new instance of EpochServiceDependencies.\nCreate a new service instance\nConstructor\nCreate a new Mithril prover\nAggregatorSignableSeedBuilder factory\nCreate a new instance of …\nMithrilSignerRegistererFollower factory\nMithrilSignerRegistererLeader factory\nCreates a new MithrilSignerRegistrationVerifier.\nSnapshotter factory\nCreate a new instance of DumbSnapshotter.\n<code>FakeSnapshotter</code> factory, with a default compression …\nCreate a new service instance\nCreate a new instance of the aggregator upkeep service.\nCreate a new UsageReporter.\nConstructor\nMithrilSignedEntityService factory\nGet next aggregate verification key for next epoch\nGet next cardano transactions signing configuration used …\nGet the protocol multi signer for the next epoch\nGet protocol parameters used for signing in the next epoch.\nGet signers for the next epoch\nGet signers with stake for the next epoch\nOpen a signer registration round\nInform the service that it can precompute data for its …\nForge a client request adding protocol version in the …\nGet the protocol multi signer for the current epoch\nPrune the datasource based on the given current epoch.\nGet the name of the data that will be pruned.\nRecord a signer registration\nRegister a signer\nAdd a new single signature for the open message at the …\nRemove the given single signatures from the buffer.\nRemove transactions and block range roots that are in a …\nRetrieves epoch settings from the aggregator\nRun the upkeep service.\nStart a loop that send event about metrics at the given …\nGet the SignedEntityConfig for the current epoch.\nGet protocol parameters for signer registration.\nCreate a new snapshot containing all completed immutables.\nCreate a new snapshot of ancillary files.\nCreate a new snapshot of an immutable trio.\nStore list of block ranges with their corresponding merkle …\nStore list of transactions\nSynchronize all signers\nSimple way to nest technical errors\nGet the total stakes of signers for the next epoch\nGet the total number of SPOs for the current epoch in the …\nGet the total stake for the current epoch in the Cardano …\nGet the total stakes of signers for the current epoch\nInsert future epoch settings in the store based on this …\nUpdate the next signers with stake for the next epoch.\nThis launches the stake distribution computation if not …\nVacuum database.\nVerifies a Signer registration.\nVerify the certificate chain and epoch gap. This will …\nSet the size assigned to the produced snapshots.\nSet the compression algorithm used to for the output file …\nEpoch of the last issued certificate\nGiven current epoch\nEpoch of the current round\nEpoch of the received signer registration\nEventual nested error\nError message\nDefine how to sign the ancillary manifest.\nAncillary signer that uses an in memory secret key to sign …\nCompute the signature of the ancillary manifest.\nReturns the argument unchanged.\nCalls <code>U::from(self)</code>.\nCreate a new instance of <code>AncillarySignerWithSecretKey</code>.")