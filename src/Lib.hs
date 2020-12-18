{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}

module Lib
    ( someFunc
    ) where

import qualified GitHub                       as GH
import qualified Servant.Client               as SC
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)

someFunc :: IO ()
someFunc = do
  putStrLn "Let's try a GitHubCall"
  testGitHubCall
  putStrLn "End."


testGitHubCall :: IO ()
testGitHubCall = 
  (SC.runClientM (GH.test "esjmb") =<< env) >>= \case

    Left err -> do
      putStrLn $ "We have a problem: " ++ show err
    Right res -> do
      putStrLn $ "Results are: " ++ show res


  where env :: IO SC.ClientEnv
        env = do
          manager <- newManager tlsManagerSettings
          return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")