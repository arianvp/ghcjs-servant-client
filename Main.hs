{-#LANGUAGE DataKinds, TypeOperators, DeriveGeneric  #-}

import Servant.Client
import Servant.API
import Data.Aeson
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Data.Proxy
import GHC.Generics
data Book = Book { title :: String
                 , author :: String
                 } deriving (Generic,Show)
instance FromJSON Book
instance ToJSON Book
type MyApi = "books" :> Get [Book] -- GET /books
        :<|> "books" :> ReqBody Book :> Post Book -- POST /books

myApi :: Proxy MyApi
myApi = Proxy

getAllBooks :: BaseUrl -> EitherT String IO [Book]
postNewBook :: Book -> BaseUrl -> EitherT String IO Book
(getAllBooks :<|> postNewBook) = client myApi

main = runEitherT $ do
  case parseBaseUrl "http://localhost:3000" of
    Left s -> liftIO $ print s
    Right u -> do
      books <- getAllBooks u
      liftIO $ print books
