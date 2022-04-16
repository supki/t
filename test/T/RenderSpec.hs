{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module T.RenderSpec (spec) where

import           Data.Aeson.QQ (aesonQQ)
import qualified Data.Text.Encoding as Text
import           Test.Hspec

import           T.Parse (parse)
import           T.Render (render, envFromJson)


spec :: Spec
spec =
  describe "render" $ do
    it "examples" $ do
      render2 [aesonQQ| {} |] "" `shouldBe` Right ""
      render2 [aesonQQ| {} |] "Слава Україні" `shouldBe` Right "Слава Україні"
      render2 [aesonQQ| {} |] "foo" `shouldBe` Right "foo"
      render2 [aesonQQ| {} |] "foo{{ 4 }}" `shouldBe` Right "foo4"
      render2 [aesonQQ| {} |] "foo{{ 4.7 }}" `shouldBe` Right "foo4.7"
      render2 [aesonQQ| {x: 4} |] "foo{{ x }}" `shouldBe` Right "foo4"
      render2 [aesonQQ| {x: {y: 4}} |] "foo{{ x.y }}" `shouldBe` Right "foo4"
      render2 [aesonQQ| {} |] "{% set x = 4 %}foo{{ x }}" `shouldBe` Right "foo4"
      render2 [aesonQQ| {} |] "{% set x = 4 %}{% set x = 7 %}foo{{ x }}" `shouldBe` Right "foo7"
      render2 [aesonQQ| {x: 4} |] "{% set y = x %}foo{{ y }}" `shouldBe` Right "foo4"
      render2 [aesonQQ| {} |] "{% if true %}foo{% else %}bar{% endif %}" `shouldBe` Right "foo"
      render2 [aesonQQ| {} |] "{% if false %}foo{% else %}bar{% endif %}" `shouldBe` Right "bar"
      render2 [aesonQQ| {x: true} |] "{% if x %}foo{% else %}bar{% endif %}" `shouldBe` Right "foo"
      render2 [aesonQQ| {} |] "{% set x = true %}{% if x %}foo{% else %}bar{% endif %}" `shouldBe` Right "foo"
      render2 [aesonQQ| {} |] "{% if false %}4{% elif true %}7{% endif %}" `shouldBe` Right "7"
      render2 [aesonQQ| {} |] "{% if false %}4{% elif false %}7{% else %}foo{% endif %}" `shouldBe` Right "foo"
      render2 [aesonQQ| {} |] "{{ true && false }}" `shouldBe` Right "false"
      render2 [aesonQQ| {} |] "{{ true || false }}" `shouldBe` Right "true"
      render2 [aesonQQ| {} |] "{{ ! true }}" `shouldBe` Right "false"

    context "if + set" $
      it "only evaluates {% set %} in the clause that is true" $ do
        render2 [aesonQQ| {} |] "{% if true %}{% set x = 4 %}{% else %}{% set x = 7 %}{% endif %}{{ x }}" `shouldBe` Right "4"
        render2 [aesonQQ| {} |] "{% if false %}{% set x = 4 %}{% elif false %}{% set x = \"foo\" %}{% elif true %}{% set x = \"bar\" %}{% else %}{% set x = 7 %}{% endif %}{{ x }}" `shouldBe` Right "bar"

    it "bool01" $ do
      render2 [aesonQQ| {} |] "{{ bool01(false) }}" `shouldBe` Right "0"
      render2 [aesonQQ| {} |] "{{ bool01(true) }}" `shouldBe` Right "1"

    it "join" $ do
      render2 [aesonQQ| {} |] "{{ join(\",\", [\"foo\", \"bar\", \"baz\"]) }}" `shouldBe`
        Right "foo,bar,baz"

    it "split" $ do
      render2 [aesonQQ| {} |] "{% for x in split(\",\", \"foo,bar,baz\") %}{{ x }}{% endfor %}" `shouldBe`
        Right "foobarbaz"

render2 json tmplStr = do
  let Right tmpl = parse (Text.encodeUtf8 tmplStr)
  render (envFromJson json) tmpl
