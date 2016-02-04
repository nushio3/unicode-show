{-# LANGUAGE ScopedTypeVariables #-}


import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.API (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit.Base hiding (Test)

import Text.Show.Unicode

data T試6験 = Å4 { すけろく :: String} deriving (Eq, Ord, Show, Read)
data T試7験 = String :\& String  deriving (Eq, Ord, Show, Read)

-- Derived instance of Read and Show produced by GHC
-- does not satisfy Haskell 2010 spec, for the moment (ghc 7.10.3).
-- Therefore, we must regretfully skip this test.
data T試8験 = String :＠\& String  deriving (Eq, Ord, Show, Read)


ushowTo :: Show a => a -> String -> Test
ushowTo f t = testCase ("ushow " ++ show f ++ " == " ++ t) $ t @=? ushow f

tests :: [Test]
tests =
  [ testGroup "individual representations test"
    [
      "漢6" `ushowTo` "\"漢6\""
    , "\na\ri\ta 国際空港" `ushowTo` "\"\\na\\ri\\ta 国際空港\""
    , "\SOH\SO\&H" `ushowTo` "\"\\SOH\\SO\\&H\""
    ]

  , testGroup "read . ushow == id"
    [ testProperty "read . ushow == id, for String" $
      \str -> read (ushow str) == (str :: String)
    , testProperty "read . read . ushow . ushow == id, for String" $
      \str -> (read $ read $ ushow $ ushow str) == (str :: String)
    , testProperty "read . ushow == id, for some crazy Unicode type" $
      \str -> let v = Å4 str in read (ushow v) == v
    , testProperty "read . ushow == id, for some crazy Unicode type" $
      \a b -> let v = a :\& b in read (ushow v) == v
--     , testProperty "read . ushow == id, for some crazy Unicode type" $
--       \a b -> let v = a :＠\& b in read (show v) == v
    , testProperty "read . ushow == id, for compound type" $
      \str -> read (ushow str) == (str :: Either [String] (String,String))
    ]
  ]
main :: IO ()
main = defaultMain tests
