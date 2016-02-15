{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Files where

import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy (ByteString)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Parse
import Servant
import Servant.Server.Internal

-- Backends for file upload: in memory or in /tmp ?

data Mem
data Tmp

class KnownBackend b where
  type Storage b :: *

  withBackend :: Proxy b -> (BackEnd (Storage b) -> IO r) -> IO r

instance KnownBackend Mem where
  type Storage Mem = ByteString

  withBackend Proxy f = f lbsBackEnd

instance KnownBackend Tmp where
  type Storage Tmp = FilePath

  withBackend Proxy f = runResourceT . withInternalState $ \s ->
    f (tempFileBackEnd s)

-- * Files combinator, to get all of the uploaded files

data Files b

type MultiPartData b = ([Param], [File (Storage b)]) 

instance (HasServer sublayout config) => HasServer (Files b :> sublayout) config where
  type ServerT (Files b :> sublayout) m =
    ((MultiPartData Tmp -> IO ()) -> IO ()) -> ServerT sublayout m

  route Proxy config subserver = WithRequest $ \request ->
    route (Proxy :: Proxy sublayout) config (addBodyCheck subserver (bodyCheck request))
    where
      bodyCheck request = return $ Route (\f -> runResourceT . withInternalState $ \s -> parseRequestBody (tempFileBackEnd s) request >>= f)


type FilesMem = Files Mem
type FilesTmp = Files Tmp

-- test
type API = "files" :> FilesTmp :> Post '[JSON] ()
      :<|> Raw

api :: Proxy API
api = Proxy

server :: Server API
server = filesHandler :<|> serveDirectory "."

  where filesHandler :: ((MultiPartData Tmp -> IO ()) -> IO ()) -> ExceptT ServantErr IO ()
        filesHandler multipart = liftIO $ multipart $ \(inputs, files) -> do
          mapM_ ppFile files
          mapM_ print inputs

        ppFile :: File FilePath -> IO ()
        ppFile (name, fileinfo) = do
          putStrLn $ "Input name: " ++ show name
          putStrLn $ "File name: " ++ show (fileName fileinfo)
          putStrLn $ "Content type: " ++ show (fileContentType fileinfo)
          putStrLn $ "------- Content --------"
          readFile (fileContent fileinfo) >>= putStrLn
          putStrLn $ "------------------------"

app :: Application
app = serve api EmptyConfig server

