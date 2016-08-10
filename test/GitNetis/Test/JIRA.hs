{-# LANGUAGE OverloadedStrings #-}

module GitNetis.Test.JIRA where

import           Data.ByteString      as BS
import           GitNetis.JIRA
import           Test.Tasty
import           Test.Tasty.HUnit

netisJIRAUrl :: String
netisJIRAUrl = "http://jira.dev.netis.com.cn:8080/rest/api/2"

tests :: TestTree
tests = testGroup "JIRA tests"
  [ testCase "Basic auth" $ do
      let authOptions = GenericAuthOptions { gaoUsername = "kenneth.zhao@netis.com.cn"
                                           , gaoPassword = "K3N1mx1jh2"
                                           }
      authResult <- auth BasicAuth authOptions netisJIRAUrl
      GenericAuthOK @=? authResult
  ]
