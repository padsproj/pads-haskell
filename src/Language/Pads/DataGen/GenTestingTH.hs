{-# LANGUAGE TemplateHaskell #-}

module Language.Pads.DataGen.GenTestingTH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List
import Data.Maybe

-- createTest :: Int -> String -> Q Exp -> Maybe (Q [Dec]) -> Q [Dec]
-- createTest tnum tname texp tdecs = do
--     qletname   <- LetS <$> [d| name = tname |]
--     qprint     <- noBindS [| printf "Running test: %s\n" tname |]
--     qsamples   <- bindS (varP samples) [| makeMany (takeWhile (/= ' ') tname) |]
--     qletresult <- LetS <$> [d| result = $texp |]
--     qreturn    <- noBindS [| return $ $(conE $ mkName "Test") name result |]
--     qwhere     <- case tdecs of Just t -> t
--                                 Nothing -> [d||]
--     return $ [ValD (VarP test) (NormalB (DoE [qletname, qprint, qsamples, qletresult, qreturn])) qwhere]
--     where
--         test    = mkName $ "test" ++ show tnum
--         samples = mkName "samples"


mkSTest :: (Lift a) => String -> a -> Q Exp -> Q [Dec] --STest a
mkSTest name expected got = do
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

mkGTest :: String -> [String] -> Q [Dec] --STest a
mkGTest name constructors = do
  let name'   = lowerFirstWord name
  let name''  = lowerAllWords name
  let name3 = takeWhile (/= ' ') name
  let f_name = mkName (name'  ++ "_check")
  let g_name = mkName (name'  ++ "_genM")
  let n_name = mkName (name'' ++ "Test_name")
  let t_name = mkName (name'' ++ "Test")
  decName <- [d| $(varP n_name) = name |]
  --decSig  <- sigD f_name (appT (appT arrowT ((conT . mkName) name3)) ((conT . mkName) "Bool"))
  --decFunc <- funD f_name [clause [(varP . mkName) "x"] (normalB [|True|]) []]
  let clauses = [clause [recP (mkName c) []] (normalB [|True|]) [] | c <- constructors]
  decFunc <- funD f_name clauses
  decTest <- [d| $(varP t_name) = $((conE . mkName) "TestCase") (assertBool name ($(varE f_name) $ unsafePerformIO $(varE g_name))) |]
  return $ decName ++ [decFunc] ++ decTest

lowerFirstWord (x:xs) = toLower x : takeWhile (flip elem alnum) xs
lowerAllWords  (x:xs) = toLower x : takeWhile (flip elem alnum) (filter (/= ' ') xs)
toLower x = fromJust $ lookup x (zip ['A'..'Z'] ['a'..'z'])
alnum = ['a'..'z']++['A'..'Z']++['0'..'9']
