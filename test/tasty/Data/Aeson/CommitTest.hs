{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.Aeson.CommitTest (tests, qcTests) where

import           Control.Applicative
import           Data.Aeson.Commit
import           Data.Aeson.QQ
import           Data.Aeson.Types
import           Data.Text             (Text)
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck (Arbitrary)
import qualified Test.Tasty.QuickCheck as QC

tests :: Spec
tests = testParserWithCases pNested
  [ ( "fails"
    , [aesonQQ| {} |]
    , Left "Error in $: key \"value\" not present"
    )
  , ( "succeeds unnested"
    , [aesonQQ| { value: "top" } |]
    , Right "top"
    )
  , ( "succeeds and prefers nested"
    , [aesonQQ| { value: "top" , nested: { value: "nest" } } |]
    , Right "nest"
    )
  , ( "succeeds with nested and no value"
    , [aesonQQ| { nested: { value: "nest" } } |]
    , Right "nest"
    )
  , ( "fails on malformed nested"
    , [aesonQQ| { value: "top", nested: { foo: 9 } } |]
    , Left "Error in $.nested: key \"value\" not present"
    )
  , ( "fails on nested type mismatch"
    , [aesonQQ| { value: "top", nested: 9 } |]
    , Left "Error in $.nested: parsing nestedObj failed, expected Object, but encountered Number"
    )
  ]
  where
    pNested :: Value -> Parser Text
    pNested = withObject "topLevel" $ \o -> runCommit $
      (o .:> "nested") (withObject "nestedObj" (.: "value"))
      <|> fromParser (o .: "value")


testParserWithCases
  :: (Eq a, Show a)
  => (v -> Parser a)
  -> [(String, v, Either String a)]
  -> Spec
testParserWithCases p =
  mapM_ ( \(name, v, result) -> it name (parseEither p v `shouldBe` result))

instance Arbitrary a => Arbitrary (Parser a) where
  arbitrary = QC.frequency [ (1, return $ fail "Fail")
                           , (9,pure <$> QC.arbitrary) ]

instance Show (Parser a) where
  show _ = "[Parser]"

instance Arbitrary a => Arbitrary (Commit a) where
  arbitrary = (\p -> commit p pure) <$> QC.arbitrary

instance Show (Commit a) where
  show _ = "[Commit]"

commitEither :: (a -> Commit b) -> a -> Either String b
commitEither f = parseEither (runCommit . f)

qcTests :: TestTree
qcTests =
  testGroup "QuickCheck Properties"
            [ QC.testProperty "Left Identity" leftIdentity
            , QC.testProperty "Right Identity" rightIdentity
            , QC.testProperty "Associativity" associativity
            , QC.testProperty "Alternative Associativity" altAssociativity
            , QC.testProperty "Failed Parser Switches to Alternative"
                              commitFailAlternative
            , QC.testProperty "Failed Continuation Switches to Alternative"
                              commitContFailAlternative
            ]

leftIdentity :: (Int -> Parser Int) -> Int -> Bool
leftIdentity f a =
  commitEither (\a' -> commit (pure (a::Int)) f) a == parseEither f a

rightIdentity :: Parser Int -> Bool
rightIdentity p =
  commitEither (const $ commit p pure) undefined ==
    parseEither (const p) undefined

associativity
  :: Parser Int -> (Int -> Parser Int) -> (Int -> Parser Int) -> Bool
associativity p f g =
  commitEither (const $ commit (runCommit $ commit p f) g) undefined ==
    commitEither (const $ commit p (\x -> runCommit $ commit (f x) g)) undefined

altAssociativity
  :: Commit Int -> Commit Int -> Commit Int -> Bool
altAssociativity c1 c2 c3 =
  commitEither (const $ (c1 <|> c2) <|> c3) undefined ==
    commitEither (const $ c1 <|> (c2 <|> c3)) undefined

commitFailAlternative
  :: (Int -> Parser Int) -> Commit Int -> Bool
commitFailAlternative f c =
  commitEither (const $ commit (fail "Fail") f <|> c) undefined ==
    commitEither (const c) undefined

commitContFailAlternative
  :: Parser Int -> Commit Int -> Bool
commitContFailAlternative p c =
  commitEither (const $ commit p (const $ fail "Fail") <|> c) undefined ==
    commitEither (const c) undefined
