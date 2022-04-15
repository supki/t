{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module T.ParseSpec (spec) where

import Test.Hspec

import T.Parse (parse)
import T.Exp (Tmpl(..), Exp(..), Name(..))


spec :: Spec
spec =
  describe "parse" $
    it "examples" $ do
      parse "" `shouldBe` Right (Raw "")
      parse "foo" `shouldBe` Right (Raw "foo")
      parse "{{ x }}" `shouldBe` Right (Exp (Var (Name ["x"])))
      parse "{{ x }}{{ y }}" `shouldBe` Right (Exp (Var (Name ["x"])) :*: Exp (Var (Name ["y"])))
      parse "{{ x.y.z }}" `shouldBe` Right (Exp (Var (Name ["x", "y", "z"])))
      parse "foo{{ x }}" `shouldBe` Right (Raw "foo" :*: Exp (Var (Name ["x"])))
      parse "foo{{ x }}bar" `shouldBe` Right (Raw "foo" :*: Exp (Var (Name ["x"])) :*: Raw "bar")
