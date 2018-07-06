{-# LANGUAGE TemplateHaskell #-}

{- Small rough TemplateHaskell test generator for use in GenTesting -}

module Language.Pads.DataGen.GenTestingTH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List
import Data.Maybe

mkTest :: (Lift a) => String -> a -> Q Exp -> Q [Dec] --STest a
mkTest name expected got = do
  let name'  = lowerFirstWord name
  let name'' = lowerAllWords name
  let n_name = mkName (name'' ++ "Test_name")
  let e_name = mkName (name'' ++ "Test_expected")
  let g_name = mkName (name'' ++ "Test_got")
  let t_name = mkName (name'' ++ "Test")
  decName     <- [d| $(varP n_name) = name |]
  decExpected <- [d| $(varP e_name) = expected |]
  decGot      <- [d| $(varP g_name) = $got |]
  decTest     <- [d| $(varP t_name) = $((conE . mkName) "TestCase") ($(varE e_name) @=? $(varE g_name)) |]
  return $ decName ++ decExpected ++ decGot ++ decTest

lowerFirstWord (x:xs) = toLower x : takeWhile (flip elem alnum) xs
lowerAllWords  (x:xs) = toLower x : takeWhile (flip elem alnum) (filter (/= ' ') xs)
toLower x = fromJust $ lookup x (zip ['A'..'Z'] ['a'..'z'])
alnum = ['a'..'z']++['A'..'Z']++['0'..'9']
