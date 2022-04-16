{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module T.ParseSpec (spec) where

import           Data.Scientific (Scientific)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import           Prelude hiding (null)
import           Test.Hspec

import           T.Parse (parse)
import           T.Exp (Tmpl(..), Exp(..), Literal(..), Name(..))


spec :: Spec
spec =
  describe "parse" $
    it "examples" $ do
      parse "" `shouldBe` Right (Raw "")
      parse "foo" `shouldBe` Right (Raw "foo")
      parse "{{ x }}" `shouldBe` Right (Exp (var ["x"]))
      parse "{{ x }}{{ y }}" `shouldBe` Right (Exp (var ["x"]) :*: Exp (var ["y"]))
      parse "{{ x.y.z }}" `shouldBe` Right (Exp (var ["x", "y", "z"]))
      parse "foo{{ x }}" `shouldBe` Right (Raw "foo" :*: Exp (var ["x"]))
      parse "foo{{ x }}bar" `shouldBe` Right (Raw "foo" :*: Exp (var ["x"]) :*: Raw "bar")
      parse "{{ null }}" `shouldBe` Right (Exp null)
      parse "{{ false }}" `shouldBe` Right (Exp false)
      parse "{{ true }}" `shouldBe` Right (Exp true)
      parse "{{ 4 }}" `shouldBe` Right (Exp (number 4))
      parse "{{ \"foo\" }}" `shouldBe` Right (Exp (string "foo"))
      parse "{{ [] }}" `shouldBe` Right (Exp (array []))
      parse "{{ [1, 2, 3] }}" `shouldBe` Right (Exp (array [number 1, number 2, number 3]))
      parse "{{ [1, x.y, \"foo\"] }}" `shouldBe`
        Right
          (Exp
            (array
              [ number 1
              , var ["x", "y"]
              , string "foo"
              ]))

var :: NonEmpty Text -> Exp
var =
  Var . Name

null :: Exp
null =
  Lit Null

false :: Exp
false =
  Lit (Bool False)

true :: Exp
true =
  Lit (Bool True)

number :: Scientific -> Exp
number =
  Lit . Number

string :: Text -> Exp
string =
  Lit . String

array :: [Exp] -> Exp
array =
  Lit . Array
