{-#LANGUAGE DataKinds, TypeOperators, DeriveGeneric, CPP, JavaScriptFFI #-}


import Servant.Client
import Servant.API
import Data.Aeson
import Data.String
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Data.Proxy
import GHC.Generics
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign

data Book = Book { title :: String
                 , author :: String
                 } deriving (Generic,Show)
instance FromJSON Book
instance ToJSON Book
type MyApi = "books" :> Get [Book]  :<|> "static" :> Raw

myApi :: Proxy MyApi
myApi = Proxy

getAllBooks :: BaseUrl -> EitherT String IO [Book]
(getAllBooks :<|> raw) = client myApi



main = runEitherT $ do
  case parseBaseUrl "http://test.arianvp.me" of
    Left s -> liftIO $ print s
    Right u -> do
      books <- getAllBooks u
      liftIO . doIt . fromString . show $ books
