{-# LANGUAGE QuasiQuotes #-}
module T.RenderSpec (spec) where

import           Data.Aeson.QQ (aesonQQ)
import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Lazy (Text)
import           Test.Hspec

import           T.Parse (parse)
import           T.Render (render, envFromJson)


spec :: Spec
spec =
  describe "render" $ do
    it "examples" $ do
      render2_ "" `shouldBe` Right ""
      render2_ "Слава Україні" `shouldBe` Right "Слава Україні"
      render2_ "foo" `shouldBe` Right "foo"
      render2_ "foo{{ 4 }}" `shouldBe` Right "foo4"
      render2_ "foo{{ 4.7 }}" `shouldBe` Right "foo4.7"
      render2 [aesonQQ| {x: 4} |] "foo{{ x }}" `shouldBe` Right "foo4"
      render2 [aesonQQ| {x: {y: 4}} |] "foo{{ x.y }}" `shouldBe` Right "foo4"
      render2_ "{% set x = 4 %}foo{{ x }}" `shouldBe` Right "foo4"
      render2_ "{% set x = 4 %}{% set x = 7 %}foo{{ x }}" `shouldBe` Right "foo7"
      render2 [aesonQQ| {x: 4} |] "{% set y = x %}foo{{ y }}" `shouldBe` Right "foo4"
      render2_ "{% if true %}foo{% else %}bar{% endif %}" `shouldBe` Right "foo"
      render2_ "{% if false %}foo{% else %}bar{% endif %}" `shouldBe` Right "bar"
      render2 [aesonQQ| {x: true} |] "{% if x %}foo{% else %}bar{% endif %}" `shouldBe` Right "foo"
      render2_ "{% set x = true %}{% if x %}foo{% else %}bar{% endif %}" `shouldBe` Right "foo"
      render2_ "{% if false %}4{% elif true %}7{% endif %}" `shouldBe` Right "7"
      render2_ "{% if false %}4{% elif false %}7{% else %}foo{% endif %}" `shouldBe` Right "foo"
      render2_ "{{ true && false }}" `shouldBe` Right "false"
      render2_ "{{ true || false }}" `shouldBe` Right "true"
      render2_ "{{ ! true }}" `shouldBe` Right "false"
      render2_ "{{ {a: 4, b: 7}.a }}" `shouldBe` Right "4"
      render2_ "{% for _ in [1,2,3] %}{{ _ }}{% endfor %}" `shouldBe`
        Left "not in scope: Name {unName = \"_\"}"
      render2_ "{% for _foo in [1,2,3] %}{{ _foo }}{% endfor %}" `shouldBe`
        Left "not in scope: Name {unName = \"_foo\"}"

    context "line blocks" $
      it "examples" $ do
        render2_ "{% if true %}\n4\n{% endif %}\n" `shouldBe` Right "4\n"
        pendingWith "fiddly parsing"
        render2_ "  {% if true %}\n  4\n  {% endif %}\n" `shouldBe` Right "  4\n"

    context "for" $
      it "examples" $ do
        render2_ "{% for x in [1,2,3] %}{{ x }}{% else %}foo{% endfor %}" `shouldBe` Right "123"
        render2_ "{% for x in {a: 4, b: 7} %}{{ x }}{% else %}foo{% endfor %}" `shouldBe`
          Right "47"
        render2_ "{% for x in [1,2,3] %}{% endfor %}{{ x }}" `shouldBe`
          Left "not in scope: Name {unName = \"x\"}"
        render2_ "{% for x in [] %}{{ x }}{% else %}foo{% endfor %}" `shouldBe` Right "foo"
        render2_ "{% for x in {} %}{{ x }}{% else %}foo{% endfor %}" `shouldBe` Right "foo"
        render2_ "{% for x, it in [1,2,3] %}{{ it.first }}{% endfor %}" `shouldBe`
          Right "truefalsefalse"
        render2_ "{% for x, it in [1,2,3] %}{{ it.last }}{% endfor %}" `shouldBe`
          Right "falsefalsetrue"
        render2_ "{% for x, it in [1,2,3] %}{{ it.index }}{% endfor %}" `shouldBe`
          Right "012"
        render2_ "{% for x, it in [1,2,3] %}{{ it.length }}{% endfor %}" `shouldBe`
          Right "333"
        render2_ "{% for x, it in {a: 4, b: 7} %}{{ it.first }}{% endfor %}" `shouldBe`
          Right "truefalse"
        render2_ "{% for x, it in {a: 4, b: 7} %}{{ it.last }}{% endfor %}" `shouldBe`
          Right "falsetrue"
        render2_ "{% for x, it in {a: 4, b: 7} %}{{ it.index }}{% endfor %}" `shouldBe`
          Right "01"
        render2_ "{% for x, it in {a: 4, b: 7} %}{{ it.length }}{% endfor %}" `shouldBe`
          Right "22"
        render2_ "{% for x, it in {a: 4, b: 7} %}{{ it.key }}{% endfor %}" `shouldBe`
          Right "ab"

    context "if + set" $
      it "only evaluates {% set %} in the clause that is true" $ do
        render2_ "{% if true %}{% set x = 4 %}{% else %}{% set x = 7 %}{% endif %}{{ x }}" `shouldBe` Right "4"
        render2_ "{% if false %}{% set x = 4 %}{% elif false %}{% set x = \"foo\" %}{% elif true %}{% set x = \"bar\" %}{% else %}{% set x = 7 %}{% endif %}{{ x }}" `shouldBe` Right "bar"

    context "functions" $ do
      it "numeric operations" $ do
        render2_ "{{ 1 + 2 }}" `shouldBe` Right "3"
        render2_ "{{ 0.1 + 0.2 }}" `shouldBe` Right "0.3"
        render2_ "{{ 1 - 2 }}" `shouldBe` Right "-1"
        render2_ "{{ 4 * 7 }}" `shouldBe` Right "28"
        render2_ "{{ 4 / 8 }}" `shouldBe` Right "0.5"

      it "bool01" $ do
        render2_ "{{ bool01(false) }}" `shouldBe` Right "0"
        render2_ "{{ bool01(true) }}" `shouldBe` Right "1"

      it "null" $ do
        render2_ "{{ empty(\"\") }}" `shouldBe` Right "true"
        render2_ "{{ empty(\"hello\") }}" `shouldBe` Right "false"
        render2_ "{{ empty([]) }}" `shouldBe` Right "true"
        render2_ "{{ empty([1, 2, 3]) }}" `shouldBe` Right "false"
        render2_ "{{ empty({}) }}" `shouldBe` Right "true"
        render2_ "{{ empty({x: 4, y: 7}) }}" `shouldBe` Right "false"

      it "length" $ do
        render2_ "{{ length(\"hello\") }}" `shouldBe` Right "5"
        render2_ "{{ length([1, 2, 3]) }}" `shouldBe` Right "3"
        render2_ "{{ length({x: 4, y: 7}) }}" `shouldBe` Right "2"

      it "join" $ do
        render2_ "{{ join(\",\", [\"foo\", \"bar\", \"baz\"]) }}" `shouldBe`
          Right "foo,bar,baz"

      it "split" $ do
        render2_ "{% for x in split(\",\", \"foo,bar,baz\") %}{{ x }}{% endfor %}" `shouldBe`
          Right "foobarbaz"

      it "die" $ do
        render2_ "{{ die(\"reason\") }}" `shouldBe` Left "die: \"reason\""
        render2_ "{{ die(4) }}" `shouldBe` Left "die: 4"

render2 :: Aeson.Value -> Text -> Either String Lazy.Text
render2 json tmplStr = do
  let Right tmpl = parse (Text.encodeUtf8 tmplStr)
  render (envFromJson json) tmpl

render2_ :: Text -> Either String Lazy.Text
render2_ =
  render2 [aesonQQ| {} |]
