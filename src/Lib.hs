{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Lib
    ( someFunc
    ) where

import qualified GitHub                       as GH
import qualified Servant.Client               as SC
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Data.Text                    hiding (map, intercalate)
import           Data.List                    (intercalate)

someFunc :: IO ()
someFunc = do
  putStrLn "Let's try a GitHubCall"
  testGitHubCall "AmmarMgmv"
  putStrLn "End."


testGitHubCall :: Text -> IO ()
testGitHubCall name = 
  (SC.runClientM (GH.getUser (Just "haskell-app") name) =<< env) >>= \case

    Left err -> do
      putStrLn $ "Failure (User): " ++ show err
    Right res -> do
      putStrLn $ "Results are: " ++ show res

      (SC.runClientM (GH.getUserRepos (Just "haskell-app") name) =<< env) >>= \case
        Left err -> do
          putStrLn $ "Failure (Repos): " ++ show err
        Right res' -> do
          putStrLn $ "The repos are: " ++
            intercalate ", " (map (\(GH.GitHubRepo n _ _  ) -> unpack n) res')


  where env :: IO SC.ClientEnv
        env = do
          manager <- newManager tlsManagerSettings
          return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")

