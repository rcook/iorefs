{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Data.IORef

newtype Handle = Handle Int

data Payload = Payload
    { hCtx :: Handle
    , hReaders :: [Handle]
    , hWriters :: [Handle]
    , hGeometries :: [Handle]
    }

type PayloadRef = IORef Payload

data Context = Context PayloadRef

data Reader = Reader PayloadRef

data Writer = Writer PayloadRef

mkContext :: Handle -> IO Context
mkContext hCtx = do
    let payload = Payload hCtx [] [] []
    r <- newIORef payload
    return $ Context r

releaseHandle :: (Handle -> IO ()) -> [Handle] -> IO ()
releaseHandle = mapM_

releaseContext :: Context -> IO ()
releaseContext (Context r) = do
    Payload{..} <- readIORef r
    releaseHandle (\(Handle value) -> putStrLn $ "release geometry " ++ show value) hGeometries
    releaseHandle (\(Handle value) -> putStrLn $ "release writer " ++ show value) hWriters
    releaseHandle (\(Handle value) -> putStrLn $ "release reader " ++ show value) hReaders

mkReader :: Context -> Handle -> IO Reader
mkReader (Context r) hReader = do
    modifyIORef' r (\p@Payload{..} -> p { hReaders = hReader : hReaders })
    return $ Reader r

mkWriter :: Context -> Handle -> IO Writer
mkWriter (Context r) hWriter = do
    modifyIORef' r (\p@Payload{..} -> p { hWriters = hWriter : hWriters })
    return $ Writer r

withContext :: (Context -> IO a) -> IO a
withContext = bracket (mkContext $ Handle 100) releaseContext

dump :: Context -> IO ()
dump (Context r) = do
    Payload{..} <- readIORef r
    putStrLn $ "readerCount=" ++ show (length hReaders)
    putStrLn $ "writerCount=" ++ show (length hWriters)
    putStrLn $ "geometryCount=" ++ show (length hGeometries)

main :: IO ()
main = do
    withContext $ \ctx -> do
        reader <- mkReader ctx (Handle 200)
        reader <- mkReader ctx (Handle 300)
        reader <- mkReader ctx (Handle 400)
        writer <- mkWriter ctx (Handle 500)
        dump ctx
        putStrLn "Done"
