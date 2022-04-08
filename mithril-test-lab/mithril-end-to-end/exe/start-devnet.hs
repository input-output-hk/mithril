import CardanoCluster (ClusterConfig (..), defaultNetworkId, withCluster)
import Control.Tracer (Tracer (Tracer))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Hydra.Prelude
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  workDir <- (</> "devnet") <$> getCurrentDirectory
  createDirectoryIfMissing True workDir
  let config =
        ClusterConfig
          { parentStateDirectory = workDir,
            networkId = defaultNetworkId
          }
      tr = Tracer $ \a -> LBS.hPut stdout (encode a <> "\n")
  withCluster tr config $ \_cluster -> do
    putTextLn "Press enter to quit"
    void getLine
