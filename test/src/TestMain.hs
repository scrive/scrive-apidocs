module Main where

import Test.Framework (defaultMain, testGroup)
import UserStateTest

main = defaultMain tests

tests = [testGroup "UserState" userStateTests]
