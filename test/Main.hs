module Main where

import PropertyTest
import UnitTest
import Test.Framework.Runners.Console (defaultMain)

main = defaultMain $ [UnitTest.tests, PropertyTest.tests]