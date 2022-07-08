{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module MithrilAggregatorServer.API
  ( -- * Client and Server
    Config(..)
  , MithrilAggregatorServerBackend(..)
  , createMithrilAggregatorServerClient
  , runMithrilAggregatorServerServer
  , runMithrilAggregatorServerMiddlewareServer
  , runMithrilAggregatorServerClient
  , runMithrilAggregatorServerClientWithManager
  , callMithrilAggregatorServer
  , MithrilAggregatorServerClient
  , MithrilAggregatorServerClientError(..)
  -- ** Servant
  , MithrilAggregatorServerAPI
  -- ** Plain WAI Application
  , serverWaiApplicationMithrilAggregatorServer
  ) where

import           MithrilAggregatorServer.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import           Network.Wai                        (Middleware)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant                            (ServerError, serve)
import           Servant.API
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application)
import           Servant.Server.StaticFiles         (serveDirectoryFileServer)
import           Web.FormUrlEncoded
import           Web.HttpApiData




-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Servant type-level API, generated from the OpenAPI spec for MithrilAggregatorServer.
type MithrilAggregatorServerAPI
    =    "certificate" :> Capture "certificate_hash" Text :> Verb 'GET 200 '[JSON] Certificate -- 'certificateCertificateHashGet' route
    :<|> "certificate-pending" :> Verb 'GET 200 '[JSON] Value -- 'certificatePendingGet' route
    :<|> "register-signatures" :> ReqBody '[JSON] [SingleSignature] :> Verb 'POST 200 '[JSON] NoContent -- 'registerSignaturesPost' route
    :<|> "register-signer" :> ReqBody '[JSON] Signer :> Verb 'POST 200 '[JSON] NoContent -- 'registerSignerPost' route
    :<|> "snapshot" :> Capture "digest" Text :> Verb 'GET 200 '[JSON] Snapshot -- 'snapshotDigestGet' route
    :<|> "snapshots" :> Verb 'GET 200 '[JSON] [Snapshot] -- 'snapshotsGet' route
    :<|> Raw 


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype MithrilAggregatorServerClientError = MithrilAggregatorServerClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for MithrilAggregatorServer.
-- The backend can be used both for the client and the server. The client generated from the MithrilAggregatorServer OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createMithrilAggregatorServerClient@). Alternatively, provided
-- a backend, the API can be served using @runMithrilAggregatorServerMiddlewareServer@.
data MithrilAggregatorServerBackend m = MithrilAggregatorServerBackend
  { certificateCertificateHashGet :: Text -> m Certificate{- ^ Returns the certificate identified by its hash  -}
  , certificatePendingGet :: m Value{- ^ Returns the informations related to the current pending certificate:   * protocol parameters (to setup cryptography)   * beacon information (where on the chain the pending certificate should be triggered)   * verification keys of the signers  -}
  , registerSignaturesPost :: [SingleSignature] -> m NoContent{- ^ Registers the single signatures from a signer participant for the pending certificate  -}
  , registerSignerPost :: Signer -> m NoContent{- ^ Registers a signer for the next certificate production  -}
  , snapshotDigestGet :: Text -> m Snapshot{- ^ Returns the informations of a snapshot and where to retrieve its binary content  -}
  , snapshotsGet :: m [Snapshot]{- ^ Returns the list of the most recent snapshots  -}
  }

newtype MithrilAggregatorServerClient a = MithrilAggregatorServerClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative MithrilAggregatorServerClient where
  pure x = MithrilAggregatorServerClient (\_ -> pure x)
  (MithrilAggregatorServerClient f) <*> (MithrilAggregatorServerClient x) =
    MithrilAggregatorServerClient (\env -> f env <*> x env)

instance Monad MithrilAggregatorServerClient where
  (MithrilAggregatorServerClient a) >>= f =
    MithrilAggregatorServerClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO MithrilAggregatorServerClient where
  liftIO io = MithrilAggregatorServerClient (\_ -> liftIO io)

createMithrilAggregatorServerClient :: MithrilAggregatorServerBackend MithrilAggregatorServerClient
createMithrilAggregatorServerClient = MithrilAggregatorServerBackend{..}
  where
    ((coerce -> certificateCertificateHashGet) :<|>
     (coerce -> certificatePendingGet) :<|>
     (coerce -> registerSignaturesPost) :<|>
     (coerce -> registerSignerPost) :<|>
     (coerce -> snapshotDigestGet) :<|>
     (coerce -> snapshotsGet) :<|>
     _) = client (Proxy :: Proxy MithrilAggregatorServerAPI)

-- | Run requests in the MithrilAggregatorServerClient monad.
runMithrilAggregatorServerClient :: Config -> MithrilAggregatorServerClient a -> ExceptT ClientError IO a
runMithrilAggregatorServerClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runMithrilAggregatorServerClientWithManager manager clientConfig cl

-- | Run requests in the MithrilAggregatorServerClient monad using a custom manager.
runMithrilAggregatorServerClientWithManager :: Manager -> Config -> MithrilAggregatorServerClient a -> ExceptT ClientError IO a
runMithrilAggregatorServerClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a MithrilAggregatorServerClientError
callMithrilAggregatorServer
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> MithrilAggregatorServerClient a -> m a
callMithrilAggregatorServer env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (MithrilAggregatorServerClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the MithrilAggregatorServer server at the provided host and port.
runMithrilAggregatorServerServer
  :: (MonadIO m, MonadThrow m)
  => Config -> MithrilAggregatorServerBackend (ExceptT ServerError IO) -> m ()
runMithrilAggregatorServerServer config backend = runMithrilAggregatorServerMiddlewareServer config requestMiddlewareId backend

-- | Run the MithrilAggregatorServer server at the provided host and port.
runMithrilAggregatorServerMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> MithrilAggregatorServerBackend (ExceptT ServerError IO) -> m ()
runMithrilAggregatorServerMiddlewareServer Config{..} middleware backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationMithrilAggregatorServer backend

-- | Plain "Network.Wai" Application for the MithrilAggregatorServer server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationMithrilAggregatorServer :: MithrilAggregatorServerBackend (ExceptT ServerError IO) -> Application
serverWaiApplicationMithrilAggregatorServer backend = serve (Proxy :: Proxy MithrilAggregatorServerAPI) (serverFromBackend backend)
  where
    serverFromBackend MithrilAggregatorServerBackend{..} =
      (coerce certificateCertificateHashGet :<|>
       coerce certificatePendingGet :<|>
       coerce registerSignaturesPost :<|>
       coerce registerSignerPost :<|>
       coerce snapshotDigestGet :<|>
       coerce snapshotsGet :<|>
       serveDirectoryFileServer "static")
