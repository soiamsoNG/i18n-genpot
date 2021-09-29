{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Data.Text.I18n.POT
Description : Generate simple POT file
-}
module Data.Text.I18n.POT (
    genPot
) where

import Data.Char.WCWidth ( wcwidth )
import qualified Data.Map.Strict as Map
import Data.Text.I18n.Types
    ( Msgstr, Msgid(..), CtxMap, MsgDec(..), Context )
import qualified Data.Text as T (unpack)
import Data.Text.I18n (CtxMap)
import Data.List (intercalate)

segmentByWidth n [] = []
segmentByWidth n _
    | n <= 0 = error "Segment length should not be Zero/Negative"
segmentByWidth n str = let (leading, tail) = splitAtWidth n str
        in
    leading : segmentByWidth n tail

splitAtWidth 0 str = ([],str)
splitAtWidth _ [] = ([], [])
splitAtWidth n str = splitAtWidth' [] n str

splitAtWidth' :: String -> Int -> String -> (String, String)
splitAtWidth' acc n [] = (acc, [])
splitAtWidth' acc 0 str = (acc, str)
splitAtWidth' acc (-1) str = (acc, str)
splitAtWidth' acc n (x:xs) = splitAtWidth' (acc ++ [x]) (n - wcwidth x) xs

-- |Generate POT file in the specific filepath
genPot :: FilePath -> CtxMap -> IO ()
genPot fp cm = writeFile fp $ intercalate "\n"  $ map display $ toList cm


toList::CtxMap -> [MsgDec]
toList m = [MsgDec ctxt id strs  | (ctxt, ids) <- Map.toList m, (id, strs) <- Map.toList ids]

class Display a where
    display:: a -> String

instance Display Msgid where
    display (Msgid t) =
        let mi = segmentByWidth 78 $ T.unpack t
            in
        case length mi of
            0 -> error "not implement"
            1 -> "msgid \"" ++ T.unpack t ++ "\"" ++ "\n"
            _ -> "msgid \"\"\n" ++ unlines (map (\x -> "\"" ++ x ++ "\"" ) mi)

instance Display (Maybe Context) where
    display (Just c) = "msgctxt \"" ++ T.unpack c ++ "\"" ++ "\n"
    display Nothing = ""

instance Display [Msgstr] where
    display _ = "msgstr " ++ "\"\"\n"

instance Display MsgDec where
    display (MsgDec ctxt msgid msgstr) = display ctxt ++ display msgid ++ display msgstr

