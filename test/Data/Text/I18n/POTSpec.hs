{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : Data.Text.I18n.POT
Description : Generate simple POT file
-}
module Data.Text.I18n.POTSpec (
    spec
) where

import Test.Hspec ( describe, it, shouldBe, SpecWith )
import Data.Text.I18n.Types ( Msgid(..), CtxMap)
import Data.Text.I18n.POT ( genPot )

deriving instance Read Msgid

spec :: SpecWith ()
spec = do
    describe "Gen POT" $ do
        it "Simple" $ do
            s <- readFile "./test/testcase/case1/case1"
            genPot "./test/testcase/case1/case1.pot" (read s :: CtxMap)
            result <- cmpFiles "./test/testcase/case1/case1.pot"  "./test/testcase/case1/case1.expect"
            result `shouldBe` True

cmpFiles :: FilePath -> FilePath -> IO Bool
cmpFiles a b = do
    aContents <- readFile a
    bContents <- readFile b
    return (aContents == bContents)
