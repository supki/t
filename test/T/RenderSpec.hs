{-# LANGUAGE QuasiQuotes #-}
module T.RenderSpec (spec) where

import           Data.Aeson.QQ (aesonQQ)
import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Lazy (Text)
import           Test.Hspec

import           T.Error (Error(..))
import           T.Parse (parse)
import           T.Render (render, envFromJson)


spec :: Spec
spec =
  describe "render" $ do
    it "examples" $ do
      r_ "" `shouldRender` ""
      r_ "Слава Україні" `shouldRender` "Слава Україні"
      r_ "foo" `shouldRender` "foo"
      r_ "foo{{ 4 }}" `shouldRender` "foo4"
      r_ "foo{{ 4.7 }}" `shouldRender` "foo4.7"
      rWith [aesonQQ| {x: 4} |] "foo{{ x }}" `shouldRender` "foo4"
      rWith [aesonQQ| {x: {y: 4}} |] "foo{{ x.y }}" `shouldRender` "foo4"
      r_ "{% set x = 4 %}foo{{ x }}" `shouldRender` "foo4"
      -- TODO: Rethink shadowing maybe?
      r_ "{% set x = 4 %}{% set y = 7 %}foo{{ y }}" `shouldRender` "foo7"
      rWith [aesonQQ| {x: 4} |] "{% set y = x %}foo{{ y }}" `shouldRender` "foo4"
      r_ "{% if true %}foo{% else %}bar{% endif %}" `shouldRender` "foo"
      r_ "{% if false %}foo{% else %}bar{% endif %}" `shouldRender` "bar"
      rWith [aesonQQ| {x: true} |] "{% if x %}foo{% else %}bar{% endif %}" `shouldRender` "foo"
      r_ "{% set x = true %}{% if x %}foo{% else %}bar{% endif %}" `shouldRender` "foo"
      r_ "{% if false %}4{% elif true %}7{% endif %}" `shouldRender` "7"
      r_ "{% if false %}4{% elif false %}7{% else %}foo{% endif %}" `shouldRender` "foo"
      r_ "{{ true && false }}" `shouldRender` "false"
      r_ "{{ true || false }}" `shouldRender` "true"
      r_ "{{ ! true }}" `shouldRender` "false"
      r_ "{{ {a: 4, b: 7}.a }}" `shouldRender` "4"
      r_ "{% for _ in [1,2,3] %}{{ _ }}{% endfor %}" `shouldRaise` NotInScope "_"
      r_ "{% for _foo in [1,2,3] %}{{ _foo }}{% endfor %}" `shouldRaise` NotInScope "_foo"
      r_ "{% case 4 %}{% when 4 %}4{% when 7 %}7{% endcase %}" `shouldRender` "4"
      r_ "{% case 7 %}{% when 4 %}4{% when 7 %}7{% endcase %}" `shouldRender` "7"
      r_ "{% case 11 %}{% when 4 %}4{% else %}11{% endcase %}" `shouldRender` "11"
      r_ "{{ null }}" `shouldRender` ""

    context "line blocks" $
      it "examples" $ do
        r_ "{% if true %}\n4\n{% endif %}\n" `shouldRender` "4\n"
        r_ "  {% if true %}\n  4\n  {% endif %}\n" `shouldRender` "  4\n"
        r_ "{% if true %}  \n4  \n{% endif %}  \n" `shouldRender` "4  \n"
        r_ "  {% let x = true %}{% endlet %}x" `shouldRender` "  x"

    context "let" $
      it "examples" $
        -- TODO: Rethink shadowing maybe?
        rWith [aesonQQ| {x: 4} |] "{% let y = 7 %}{{ y }}{% let z = 11 %}{{ z }}{% endlet %}{% endlet %}" `shouldRender` "711"

    context "for" $
      it "examples" $ do
        r_ "{% for x in [1,2,3] %}{{ x }}{% else %}foo{% endfor %}" `shouldRender` "123"
        r_ "{% for x in {a: 4, b: 7} %}{{ x }}{% else %}foo{% endfor %}" `shouldRender`
          "47"
        r_ "{% for x in [1,2,3] %}{% endfor %}{{ x }}" `shouldRaise` NotInScope "x"
        r_ "{% for x in [] %}{{ x }}{% else %}foo{% endfor %}" `shouldRender` "foo"
        r_ "{% for x in {} %}{{ x }}{% else %}foo{% endfor %}" `shouldRender` "foo"
        r_ "{% for x, it in [1,2,3] %}{{ it.first }}{% endfor %}" `shouldRender`
          "truefalsefalse"
        r_ "{% for x, it in [1,2,3] %}{{ it.last }}{% endfor %}" `shouldRender`
          "falsefalsetrue"
        r_ "{% for x, it in [1,2,3] %}{{ it.index }}{% endfor %}" `shouldRender`
          "012"
        r_ "{% for x, it in [1,2,3] %}{{ it.length }}{% endfor %}" `shouldRender`
          "333"
        r_ "{% for x, it in {a: 4, b: 7} %}{{ it.first }}{% endfor %}" `shouldRender`
          "truefalse"
        r_ "{% for x, it in {a: 4, b: 7} %}{{ it.last }}{% endfor %}" `shouldRender`
          "falsetrue"
        r_ "{% for x, it in {a: 4, b: 7} %}{{ it.index }}{% endfor %}" `shouldRender`
          "01"
        r_ "{% for x, it in {a: 4, b: 7} %}{{ it.length }}{% endfor %}" `shouldRender`
          "22"
        r_ "{% for x, it in {a: 4, b: 7} %}{{ it.key }}{% endfor %}" `shouldRender`
          "ab"

    context "if + set" $
      it "only evaluates {% set %} in the clause that is true" $ do
        r_ "{% if true %}{% set x = 4 %}{% else %}{% set x = 7 %}{% endif %}{{ x }}" `shouldRender` "4"
        r_ "{% if false %}{% set x = 4 %}{% elif false %}{% set x = \"foo\" %}{% elif true %}{% set x = \"bar\" %}{% else %}{% set x = 7 %}{% endif %}{{ x }}" `shouldRender` "bar"

    context "shadowing" $
      it "is an error" $
        r_ "{% let x = 4 %}{% set x = 7 %}{% endlet %}" `shouldRaise`
          ("x" `ShadowedBy` "x")

    context "functions" $ do
      it "numeric operations" $ do
        r_ "{{ 1 + 2 }}" `shouldRender` "3"
        r_ "{{ 0.1 + 0.2 }}" `shouldRender` "0.3"
        r_ "{{ 1 - 2 }}" `shouldRender` "-1"
        r_ "{{ 4 * 7 }}" `shouldRender` "28"
        r_ "{{ 4 / 8 }}" `shouldRender` "0.5"

      it "bool01" $ do
        r_ "{{ bool01(false) }}" `shouldRender` "0"
        r_ "{{ bool01(true) }}" `shouldRender` "1"

      it "null" $ do
        r_ "{{ empty(\"\") }}" `shouldRender` "true"
        r_ "{{ empty(\"hello\") }}" `shouldRender` "false"
        r_ "{{ empty([]) }}" `shouldRender` "true"
        r_ "{{ empty([1, 2, 3]) }}" `shouldRender` "false"
        r_ "{{ empty({}) }}" `shouldRender` "true"
        r_ "{{ empty({x: 4, y: 7}) }}" `shouldRender` "false"

      it "length" $ do
        r_ "{{ length(\"hello\") }}" `shouldRender` "5"
        r_ "{{ length([1, 2, 3]) }}" `shouldRender` "3"
        r_ "{{ length({x: 4, y: 7}) }}" `shouldRender` "2"

      it "join" $ do
        r_ "{{ join(\",\", [\"foo\", \"bar\", \"baz\"]) }}" `shouldRender`
          "foo,bar,baz"

      it "split" $ do
        r_ "{% for x in split(\",\", \"foo,bar,baz\") %}{{ x }}{% endfor %}" `shouldRender`
          "foobarbaz"

      it "die" $ do
        r_ "{{ die(\"reason\") }}" `shouldRaise` GenericError "die: \"reason\""
        r_ "{{ die(4) }}" `shouldRaise` GenericError "die: 4"

      it "show" $
        r_ "{{ show([1, {a: 4}, \"foo\"]) }}" `shouldRender`
          "[1,{\"a\":4},\"foo\"]"

      it "pp" $
        r_ "{{ pp([1, {a: 4}, \"foo\"]) }}" `shouldRender`
          "[\n    1,\n    {\n        \"a\": 4\n    },\n    \"foo\"\n]"

      it "==" $ do
        r_ "{{ null == null }}" `shouldRender` "true"
        r_ "{{ true == true }}" `shouldRender` "true"
        r_ "{{ true == false }}" `shouldRender` "false"
        r_ "{{ 4 == 4 }}" `shouldRender` "true"
        r_ "{{ 4 == 7 }}" `shouldRender` "false"
        r_ "{{ \"foo\" == \"foo\" }}" `shouldRender` "true"
        r_ "{{ \"foo\" == \"bar\" }}" `shouldRender` "false"
        r_ "{{ [] == [] }}" `shouldRender` "true"
        r_ "{{ [] == [1, 2] }}" `shouldRender` "false"
        r_ "{{ [1, \"foo\", false] == [1, \"foo\", false] }}" `shouldRender` "true"
        r_ "{{ [1, \"foo\", false] == [1, \"bar\", true] }}" `shouldRender` "false"
        r_ "{{ {} == {} }}" `shouldRender` "true"
        r_ "{{ {} == {a: 4} }}" `shouldRender` "false"
        r_ "{{ {a: 4, b: \"foo\"} == {a: 4, b: \"foo\"} }}" `shouldRender` "true"
        r_ "{{ {a: 4, b: \"foo\"} == {a: 4, b: \"bar\"} }}" `shouldRender` "false"
        r_ "{{ 4 == \"foo\" }}" `shouldRender` "false"
        r_ "{{ {a: 4} == length }}" `shouldRender` "false"

      it "=~" $ do
        r_ "{{ \"foo\" =~ /foo/ }}" `shouldRender` "true"
        r_ "{{ \"Foo\" =~ /foo/ }}" `shouldRender` "false"
        r_ "{{ \"Foo\" =~ /foo/i }}" `shouldRender` "true"

      context "lazyness" $ do
        it "||" $ do
          r_ "{{ true || die(\"no reason\") }}" `shouldRender` "true"
          r_ "{{ false || die(4) }}" `shouldRaise` GenericError "die: 4"

        it "&&" $ do
          r_ "{{ false && die(\"no reason\") }}" `shouldRender` "false"
          r_ "{{ true && die(4) }}" `shouldRaise` GenericError "die: 4"

shouldRender :: (HasCallStack, Show e, Eq e, Show a, Eq a) => Either e a -> a -> Expectation
tmpl `shouldRender` res =
  tmpl `shouldBe` Right res

shouldRaise :: (HasCallStack, Show e, Eq e, Show a, Eq a) => Either e a -> e -> Expectation
tmpl `shouldRaise` res =
  tmpl `shouldBe` Left res

rWith :: Aeson.Value -> Text -> Either Error Lazy.Text
rWith json tmplStr = do
  let Just env = envFromJson json
      Right tmpl = parse (Text.encodeUtf8 tmplStr)
  render env tmpl

r_ :: Text -> Either Error Lazy.Text
r_ =
  rWith [aesonQQ| {} |]
