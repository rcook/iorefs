module Main (main) where

import Control.Exception
import Control.Monad
import Data.IORef

newtype Handle = Handle Int

data Payload = Payload Handle [Handle] [Handle] [Handle]

type PayloadRef = IORef Payload

data Context = Context PayloadRef

data Reader = Reader PayloadRef

data Writer = Writer PayloadRef

mkContext :: Handle -> IO Context
mkContext hCtx = do
    let payload = Payload hCtx [] [] []
    r <- newIORef payload
    return $ Context r

releaseContext :: Context -> IO ()
releaseContext (Context r) = do
    (Payload hCtx hReaders hWriters hGeometries) <- readIORef r
    forM_ hGeometries $ \(Handle value) -> do
        putStrLn $ "release geometry " ++ show value
    forM_ hWriters $ \(Handle value) -> do
        putStrLn $ "release writer " ++ show value
    forM_ hReaders $ \(Handle value) -> do
        putStrLn $ "release reader " ++ show value

mkReader :: Context -> Handle -> IO Reader
mkReader (Context r) hReader = do
    modifyIORef' r (\(Payload hCtx hReaders hWriters hGeometries) -> Payload hCtx (hReader : hReaders) hWriters hGeometries)
    return $ Reader r

mkWriter :: Context -> Handle -> IO Writer
mkWriter (Context r) hWriter = do
    modifyIORef' r (\(Payload hCtx hReaders hWriters hGeometries) -> Payload hCtx hReaders (hWriter : hWriters) hGeometries)
    return $ Writer r

withContext :: (Context -> IO a) -> IO a
withContext = bracket (mkContext $ Handle 100) releaseContext

dump :: Context -> IO ()
dump (Context r) = do
    (Payload hCtx hReaders hWriters hGeometries) <- readIORef r
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
