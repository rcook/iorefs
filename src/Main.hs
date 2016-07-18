{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Data.IORef

newtype Handle = Handle Int

data ContextState = ContextState
    { hCtx :: Handle
    , hReaders :: [Handle]
    , hWriters :: [Handle]
    , hGeometries :: [Handle]
    }

type ContextStateRef = IORef ContextState

data Context = Context ContextStateRef

data Reader = Reader ContextStateRef

data Writer = Writer ContextStateRef

data Geometry = Geometry ContextStateRef

mkContext :: Handle -> IO Context
mkContext hCtx = do
    let s = ContextState hCtx [] [] []
    sr <- newIORef s
    return $ Context sr

releaseContext :: Context -> IO ()
releaseContext (Context sr) = do
    ContextState{..} <- readIORef sr
    releaseHandle (\(Handle value) -> putStrLn $ "release geometry " ++ show value) hGeometries
    releaseHandle (\(Handle value) -> putStrLn $ "release writer " ++ show value) hWriters
    releaseHandle (\(Handle value) -> putStrLn $ "release reader " ++ show value) hReaders
    where
        releaseHandle :: (Handle -> IO ()) -> [Handle] -> IO ()
        releaseHandle = mapM_

mkReader :: Context -> Handle -> IO Reader
mkReader (Context sr) hReader = do
    modifyIORef' sr (\p@ContextState{..} -> p { hReaders = hReader : hReaders })
    return $ Reader sr

mkWriter :: Context -> Handle -> IO Writer
mkWriter (Context sr) hWriter = do
    modifyIORef' sr (\p@ContextState{..} -> p { hWriters = hWriter : hWriters })
    return $ Writer sr

readGeometry :: Reader -> Handle -> IO Geometry
readGeometry (Reader sr) hGeometry = do
    modifyIORef' sr (\p@ContextState{..} -> p { hGeometries = hGeometry : hGeometries })
    return $ Geometry sr

withContext :: (Context -> IO a) -> IO a
withContext = bracket (mkContext $ Handle 100) releaseContext

dump :: Context -> IO ()
dump (Context r) = do
    ContextState{..} <- readIORef r
    putStrLn $ "readerCount=" ++ show (length hReaders)
    putStrLn $ "writerCount=" ++ show (length hWriters)
    putStrLn $ "geometryCount=" ++ show (length hGeometries)

main :: IO ()
main = do
    withContext $ \ctx -> do
        reader <- mkReader ctx (Handle 200)
        reader <- mkReader ctx (Handle 300)
        reader <- mkReader ctx (Handle 400)
        g0 <- readGeometry reader (Handle 10)
        writer <- mkWriter ctx (Handle 500)
        dump ctx
        putStrLn "Done"
