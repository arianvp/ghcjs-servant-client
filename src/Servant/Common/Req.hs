{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
module Servant.Common.Req where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types
import Data.Attoparsec.ByteString
#if !defined(ghcjs_HOST_OS)
import Data.ByteString.Lazy hiding (pack)
#endif
import Data.String
import Data.String.Conversions
import Data.Text
import Data.Text.Encoding
#if !defined(ghcjs_HOST_OS)
import Network.HTTP.Client
#endif
import Network.HTTP.Types
import Network.URI
import Servant.Common.BaseUrl
import Servant.Common.Text
import System.IO.Unsafe
import Data.ByteString.Char8
#if !defined(ghcjs_HOST_OS)
import qualified Network.HTTP.Client as Client
#endif

#if defined(ghcjs_HOST_OS)
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import Control.Concurrent.MVar
import Foreign.ForeignPtr
import Data.Word
import Data.ByteString.Unsafe
import Unsafe.Coerce

#endif
data Req = Req
  { reqPath  :: String
  , qs       :: QueryText
  , reqBody  :: ByteString
  , headers  :: [(String, Text)]
  }

defReq :: Req
defReq = Req "" [] "" []

appendToPath :: String -> Req -> Req
appendToPath p req =
  req { reqPath = reqPath req ++ "/" ++ p }

appendToQueryString :: Text       -- ^ param name
                    -> Maybe Text -- ^ param value
                    -> Req
                    -> Req
appendToQueryString pname pvalue req =
  req { qs = qs req ++ [(pname, pvalue)]
      }

addHeader :: ToText a => String -> a -> Req -> Req
addHeader name val req = req { headers = headers req
                                      ++ [(name, toText val)]
                             }

setRQBody :: ByteString -> Req -> Req
setRQBody b req = req { reqBody = b }


#if !defined(ghcjs_HOST_OS)
reqToRequest :: (Functor m, MonadThrow m) => Req -> BaseUrl -> m Request
reqToRequest req (BaseUrl reqScheme reqHost reqPort) =
    fmap (setheaders . setrqb . setQS ) $ parseUrl url

  where url = show $ nullURI { uriScheme = case reqScheme of
                                  Http  -> "http:"
                                  Https -> "https:"
                             , uriAuthority = Just $
                                 URIAuth { uriUserInfo = ""
                                         , uriRegName = reqHost
                                         , uriPort = ":" ++ show reqPort
                                         }
                             , uriPath = reqPath req
                             }

        setrqb r = r { requestBody = RequestBodyLBS (reqBody req) }
        setQS = setQueryString $ queryTextToQuery (qs req)
        setheaders r = r { requestHeaders = Prelude.map toProperHeader (headers req) }

        toProperHeader (name, val) =
          (fromString name, encodeUtf8 val) 
#endif

-- * performing requests
#if !defined(ghcjs_HOST_OS)
{-# NOINLINE __manager #-}
__manager :: MVar Manager
__manager = unsafePerformIO (newManager defaultManagerSettings >>= newMVar)

__withGlobalManager :: (Manager -> IO a) -> IO a
__withGlobalManager action = modifyMVar __manager $ \ manager -> do
  result <- action manager
  return (manager, result)
#else
#endif


displayHttpRequest :: Method -> String
displayHttpRequest httpmethod = "HTTP " ++ cs httpmethod ++ " request"

#if defined(ghcjs_HOST_OS)
-- define wrapper functions for XMLHttpRequest
-- TODO: This should be split into its own library I think..
--
--
-- Note: The C preprocessor will fail if you use a single-quote in the name
#define JS(name, js, type) foreign import javascript unsafe js name :: type

data XMLHttpRequest
JS(newXhr, "new XMLHttpRequest()", IO (JSRef XMLHttpRequest))
JS(xhrOpen, "($1).open(($2), ($3), ($4))", JSRef XMLHttpRequest -> JSString -> JSString -> JSBool -> IO ())
JS(xhrSend, "($1).send()", JSRef XMLHttpRequest -> IO ())
JS(xhrSendWithData, "($1).send(($2))", JSRef XMLHttpRequest -> JSRef a -> IO ())
JS(xhrSetOnReadyStateChange, "($1).onreadystatechange = ($2)", JSRef XMLHttpRequest -> JSFun (IO ()) -> IO ())
JS(xhrGetReadyState, "($1).readyState", JSRef XMLHttpRequest -> IO (JSRef Int))
JS(xhrGetResponseText, "($1).responseText", JSRef XMLHttpRequest -> IO (JSString))
JS(xhrGetResponse, "($1).response", JSRef XMLHttpRequest -> IO (JSRef a))
JS(xhrSetResponseType, "($1).responseType = $2", JSRef XMLHttpRequest -> JSString -> IO ())
JS(xhrGetStatus, "($1).status", JSRef XMLHttpRequest -> IO Int)
JS(doIt, "document.querySelector('body').appendChild(document.createTextNode($1))", JSString -> IO ())



-- TODO: Error handling
mkBinaryRequest' :: String -> String -> Maybe ByteString -> IO (Either Int  (Int,ByteString))
mkBinaryRequest' method url body = do
  res <- newEmptyMVar
  xhr <- newXhr
  xhrOpen xhr (fromString method) (fromString url) jsTrue
  xhrSetResponseType xhr "arraybuffer"
  rec cb <- syncCallback AlwaysRetain True $ do
        readyState <- fromJSRef =<< xhrGetReadyState xhr
        if readyState == Just 4
          then do
            status <- xhrGetStatus xhr
            if status >= 200 && status < 300
              then do
                bs <- (bufferByteString 0 0 <=< xhrGetResponse) xhr
                putMVar res (return (status,bs))
              else
                putMVar res (Left status) 
            release cb
          else
            return ()
  xhrSetOnReadyStateChange xhr cb
  case body of
    Just bs -> do
      unsafeUseAsCString bs $ \cs -> do
        let a = unsafeCoerce cs
        xhrSendWithData xhr a
    Nothing -> xhrSend xhr
  takeMVar res

  
#endif

performRequest :: Method -> Req -> (Int -> Bool) -> BaseUrl -> EitherT String IO (Int, ByteString)
#if !defined(ghcjs_HOST_OS)
performRequest reqMethod req isWantedStatus reqHost = do
  partialRequest <- liftIO $ reqToRequest req reqHost

  let request = partialRequest { Client.method = reqMethod
                               , checkStatus = \ _status _headers _cookies -> Nothing
                               }

  eResponse <- liftIO $ __withGlobalManager $ \ manager ->
    catchStatusCodeException $
    Client.httpLbs request manager
  case eResponse of
    Left status ->
      left (displayHttpRequest reqMethod ++ " failed with status: " ++ showStatus status)

    Right response -> do
      let status = Client.responseStatus response
      unless (isWantedStatus (statusCode status)) $
        left (displayHttpRequest reqMethod ++ " failed with status: " ++ showStatus status)
      return $ (statusCode status, Client.responseBody response)
  where
    showStatus (Status code message) =
      show code ++ " - " ++ cs message
#else
performRequest reqMethod req isWantedStatus (BaseUrl reqScheme reqHost reqPort) = do
  let url = show $ nullURI { uriScheme = case reqScheme of
                              Http -> "http:"
                              Https -> "https:"
                           , uriAuthority = Just $
                              URIAuth { uriUserInfo = ""
                                      , uriRegName = reqHost
                                      , uriPort = ":" ++ show reqPort
                                      }
                           , uriPath = reqPath req
                           }
--TODO: Codesmell the unpacking. 
  res <- liftIO $ mkBinaryRequest' (Data.ByteString.Char8.unpack reqMethod) url (Just (reqBody req))
  -- TODO: set headers
  
  case res of
    Left status -> left (displayHttpRequest reqMethod ++ " failed with status: " ++ show status)
    Right (status,res) ->
      if isWantedStatus status
        then return $ (status, res)
        else left (displayHttpRequest reqMethod ++ " failed with status: " ++ show status)
  
  --bs <- liftIO $ mkBinaryRequest' "GET" url Nothing
#endif

performRequestJSON :: FromJSON result =>
  Method -> Req -> Int -> BaseUrl -> EitherT String IO result
performRequestJSON reqMethod req wantedStatus reqHost = do
  (_status, respBody) <- performRequest reqMethod req (== wantedStatus) reqHost
  either
    (\ message -> left (displayHttpRequest reqMethod ++ " returned invalid json: " ++ message))
    return
    (decodeLenient respBody)


#if !defined(ghcjs_HOST_OS)
catchStatusCodeException :: IO a -> IO (Either Status a)
catchStatusCodeException action =
  catch (Right <$> action) $ \e ->
    case e of
      Client.StatusCodeException status _ _ -> return $ Left status
      exc -> throwIO exc
#endif
-- | Like 'Data.Aeson.decode' but allows all JSON values instead of just
-- objects and arrays.
decodeLenient :: FromJSON a => ByteString -> Either String a
decodeLenient input = do
  v :: Value <- parseOnly (Data.Aeson.Parser.value <* endOfInput) (cs input)
  parseEither parseJSON v
