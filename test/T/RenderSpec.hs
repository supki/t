{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module T.RenderSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson (toHashMapText)
import Data.Aeson.QQ (aesonQQ)
import Data.List qualified as List
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as Lazy (Text)
import Test.Hspec

import T.Exp (Literal(..), litE_, appE, varE)
import T.Embed (embed0)
import T.Error (Error(..), Warning(..))
import T.Name (Name(..))
import T.Parse (parse)
import T.Parse.Macro (badArity)
import T.Prelude
import T.Render (Scope(..), render)
import T.Stdlib (def)
import T.Stdlib qualified as Stdlib
import T.Stdlib.Op qualified as Op
import T.Value (embedAeson)
import T.Type qualified as Type


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
      r_ "{% set x = 4 %}{% set x = 7 %}foo{{ x }}" `shouldRender` "foo7"
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

    context "indexing" $
      it "examples" $ do
        r_ "{{ [1,2,3][0] }}" `shouldRender` "1"
        r_ "{{ [[1,2],[3]][0][1] }}" `shouldRender` "2"
        r_ "{{ 4[0] }}" `shouldRaise`
          TypeError (litE_ (Int 4)) Type.Array Type.Int "4"
        r_ "{{ [1,2,3][\"foo\"] }}" `shouldRaise`
          TypeError (litE_ (String "foo")) Type.Int Type.String "\"foo\""
        r_ "{{ [1,2,3][-1] }}" `shouldRaise` OutOfBounds (litE_ (Int (-1))) "[1,2,3]" "-1"


    context "line blocks" $
      it "examples" $ do
        r_ "{% if true %}\n4\n{% endif %}\n" `shouldRender` "4\n"
        r_ "  {% if true %}\n  4\n  {% endif %}\n" `shouldRender` "  4\n"
        r_ "{% if true %}  \n4  \n{% endif %}  \n" `shouldRender` "4  \n"
        r_ "  {% let x = true %}{% endlet %}x" `shouldRender` "  x"

    context "let" $
      it "examples" $
        rWith [aesonQQ| {x: 4} |] "{% let x = 7 %}{{ x }}{% let x = 11 %}{{ x }}{% endlet %}{% endlet %}" `shouldRender` "711"

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

    it "shadowing is a warning" $ do
      r_ "{% let x = 4 %}{% set x = 7 %}{{x}}{% endlet %}" `shouldRender` "7"
      r_ "{% let x = 4 %}{% set x = 7 %}{{x}}{% endlet %}" `shouldWarn` [ShadowedBy "x"]
      r_ "{% set show = \"show\" %}" `shouldWarn` [ShadowedBy "show"]

    it "repeated warnings merge" $ do
      r_ "{% for x in [1, 2, 3] %}{% set x = 7 %}{% endfor %}" `shouldWarn`
        [ShadowedBy "x"]
      r_ "{% for x in [1, 2, 3] %}\
            \{% for y in [4, 5, 6] %}\
                \{% set x = 7 %}\
                \{% set y = 42 %}\
              \{% endfor %}\
            \{% endfor %}" `shouldWarn`
        [ShadowedBy "x", ShadowedBy "y"]

    context "functions" $ do
      context "numeric operations" $ do
        it "integer operations" $ do
          r_ "{{ 1 + 2 }}" `shouldRender` "3"
          r_ "{{ 1 - 2 }}" `shouldRender` "-1"
          r_ "{{ 1 - 2 - 3 }}" `shouldRender` "-4"
          r_ "{{ 4 * 7 }}" `shouldRender` "28"
          r_ "{{ 4 / 8 }}" `shouldRender` "0"
          r_ "{{ 8 / 4 }}" `shouldRender` "2"
          r_ "{{ 9 / 2 }}" `shouldRender` "4"

        it "floating operations" $ do
          r_ "{{ 0.1 + 0.2 }}" `shouldRender` "0.30000000000000004"
          r_ "{{ 0.1 - 0.2 }}" `shouldRender` "-0.1"
          r_ "{{ 0.4 * 0.7 }}" `shouldRender` "0.27999999999999997"
          r_ "{{ 4.0 / 8.0 }}" `shouldRender` "0.5"
          r_ "{{ 4.0 / 8.0 / 2.0 }}" `shouldRender` "0.25"
          r_ "{{ 1.0 / 3.0 }}" `shouldRender` "0.3333333333333333"

      context "numeric comparisons" $ do
        it "integer operations" $ do
          r_ "{{ 1 > 2 }}" `shouldRender` "false"
          r_ "{{ 1 >= 2 }}" `shouldRender` "false"
          r_ "{{ 1 < 2 }}" `shouldRender` "true"
          r_ "{{ 1 <= 2 }}" `shouldRender` "true"

        it "floating operations" $ do
          r_ "{{ 1.0 > 2.0 }}" `shouldRender` "false"
          r_ "{{ 1.0 >= 2.0 }}" `shouldRender` "false"
          r_ "{{ 1.0 < 2.0 }}" `shouldRender` "true"
          r_ "{{ 1.0 <= 2.0 }}" `shouldRender` "true"

      context "numberic conversions" $ do
        it "floor" $ do
          r_ "{{ floor(1.8) }}" `shouldRender` "1"

        it "ceiling" $ do
          r_ "{{ ceiling(1.2) }}" `shouldRender` "2"

        it "round" $ do
          r_ "{{ round(1.5) }}" `shouldRender` "2"
          r_ "{{ round(2.5) }}" `shouldRender` "2"
          r_ "{{ round(-1.5) }}" `shouldRender` "-2"
          r_ "{{ round(-2.5) }}" `shouldRender` "-2"

        it "int->double" $
          r_ "{{ int->double(4) }}" `shouldRender` "4.0"

      it "string concatenation" $ do
        r_ "{{ \"foo\" <> \"bar\" }}" `shouldRender` "foobar"
        r_ "{{ concat([\"foo\", \"bar\", \"baz\"]) }}" `shouldRender` "foobarbaz"

      it "bool01" $ do
        r_ "{{ bool01(false) }}" `shouldRender` "0"
        r_ "{{ bool01(true) }}" `shouldRender` "1"

      it "<+>" $ do
        r_ "{{ \"foo\" <+> \"bar\" }}" `shouldRender` "foo+bar"

      it "empty" $ do
        r_ "{{ empty?(\"\") }}" `shouldRender` "true"
        r_ "{{ empty?(\"hello\") }}" `shouldRender` "false"
        r_ "{{ empty?([]) }}" `shouldRender` "true"
        r_ "{{ empty?([1, 2, 3]) }}" `shouldRender` "false"
        r_ "{{ empty?({}) }}" `shouldRender` "true"
        r_ "{{ empty?({x: 4, y: 7}) }}" `shouldRender` "false"
        r_ "{{ empty?(4) }}" `shouldRaise`
          UserError "empty?" "not applicable to 4 (not a string, array, or record)"

      it "length" $ do
        r_ "{{ length(\"hello\") }}" `shouldRender` "5"
        r_ "{{ length([1, 2, 3]) }}" `shouldRender` "3"
        r_ "{{ length({x: 4, y: 7}) }}" `shouldRender` "2"
        r_ "{{ length(4) }}" `shouldRaise`
          UserError "length" "not applicable to 4 (not a string, array, or record)"

      it "split" $
        r_ "{% for x in split(\",\", \"foo,bar,baz\") %}{{ x }}{% endfor %}" `shouldRender`
          "foobarbaz"

      it "join" $
        r_ "{{ join(\",\", [\"foo\", \"bar\", \"baz\"]) }}" `shouldRender`
          "foo,bar,baz"

      it "die" $ do
        r_ "{{ die(\"reason\") }}" `shouldRaise` UserError "die" "\"reason\""
        r_ "{{ die(4) }}" `shouldRaise` UserError "die" "4"

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

      it "!=" $ do
        r_ "{{ null != null }}" `shouldRender` "false"
        r_ "{{ true != false }}" `shouldRender` "true"

      it "=~" $ do
        r_ "{{ \"foo\" =~ /foo/ }}" `shouldRender` "true"
        r_ "{{ \"Foo\" =~ /foo/ }}" `shouldRender` "false"
        r_ "{{ \"Foo\" =~ /foo/i }}" `shouldRender` "true"

      it "not-iterable" $
        r_ "{% for x in 4 %}{% endfor %}" `shouldRaise` TypeError (litE_ (Int 4)) Type.Iterable Type.Int "4"

      it "not-renderable" $
        r_ "{{ [] }}" `shouldRaise` TypeError (litE_ (Array [])) Type.Renderable Type.Array "[]"

      it "not-a-function" $
        rWith [aesonQQ|{f: "foo"}|] "{{ f(4) }}" `shouldRaise` TypeError (varE "f") Type.Fun Type.String "\"foo\""

      it "type errors" $
        r_ "{{ bool01(\"foo\") }}" `shouldRaise` TypeError (varE "bool01") Type.Bool Type.String "\"foo\""

      it "defined?" $
        rWith [aesonQQ|{foo: {}}|] "{{ defined?(foo.bar.baz) }}" `shouldRender` "false"

      it "coalesce" $ do
        r_ "{{ coalesce(false, true) }}" `shouldRender` "false"
        r_ "{{ coalesce(foo.bar.baz, true) }}" `shouldRender` "true"
        r_ "{{ coalesce(foo.bar, foo.bar.baz, true) }}" `shouldRender` "true"

      it "?||" $ do
        r_ "{{ false ?|| true }}" `shouldRender` "false"
        r_ "{{ foo.bar.baz ?|| true }}" `shouldRender` "true"
        r_ "{{ foo.bar ?|| foo.bar.baz ?|| true }}" `shouldRender` "true"

      it "macros are lazy and nestable" $ do
        r_ "{{ true || die(\"no reason\") }}" `shouldRender` "true"
        r_ "{{ false || die(4) }}" `shouldRaise` UserError "die" "4"
        r_ "{{ false && die(\"no reason\") }}" `shouldRender` "false"
        r_ "{{ true && die(4) }}" `shouldRaise` UserError "die" "4"
        r_ "{{ true && (false || true) }}" `shouldRender` "true"

      it "|" $ do
        r_ "{{ 4 | show }}" `shouldRender` "4"
        r_ "{{ [\"foo\", \"bar\"] | join(\"-\") }}" `shouldRender` "foo-bar"
        r_ "{{ \"ignored\" | const(false) | bool01 }}" `shouldRender` "0"

      it "upper-case" $ do
        r_ "{{ upper-case(\"hello\") }}" `shouldRender` "HELLO"
        r_ "{{ upper-case(\"HELLO\") }}" `shouldRender` "HELLO"
        r_ "{{ upper-case(\"123\") }}" `shouldRender` "123"

      it "lower-case" $ do
        r_ "{{ lower-case(\"hello\") }}" `shouldRender` "hello"
        r_ "{{ lower-case(\"HELLO\") }}" `shouldRender` "hello"
        r_ "{{ lower-case(\"123\") }}" `shouldRender` "123"

      it "title-case" $ do
        r_ "{{ title-case(\"hello\") }}" `shouldRender` "Hello"
        r_ "{{ title-case(\"HELLO\") }}" `shouldRender` "Hello"
        r_ "{{ title-case(\"123\") }}" `shouldRender` "123"

    context "milti-set" $
      it "examples" $ do
        r_ "{% set %}look ma no assignments" `shouldRender` "look ma no assignments"
        r_ "{% set x = 4 y = 7 %}{{x + y}}" `shouldRender` "11"
        r_
          "{% set\
          \     author-name = \"John Smith\"\n\
          \     email-domain = \"example.com\"\n\
          \     author-email =\n\
          \       concat([join(\".\", split(\" \", lower-case(author-name))), \"@\", email-domain])\
          \%}{{ author-email }}" `shouldRender` "john.smith@example.com"

    context "milti-let" $
      it "examples" $ do
        r_ "{% let %}look ma no assignments{% endlet %}" `shouldRender` "look ma no assignments"
        r_ "{% let x = 4 y = 7 %}{{x + y}}{% endlet %}" `shouldRender` "11"
        r_ "{% let x = 4 y = 7 %}{% let x = 40 %}{{x + y}}{% endlet %}{% endlet %}" `shouldRender` "47"
        r_
          "{% let\
          \     author-name = \"John Smith\"\n\
          \     email-domain = \"example.com\"\n\
          \     author-email =\n\
          \       concat([join(\".\", split(\" \", lower-case(author-name))), \"@\", email-domain])\
          \%}{{ author-email }}{% endlet %}" `shouldRender` "john.smith@example.com"

    context "comments" $
      it "examples" $ do
        r_ "{% for x in [1,2,3] %}{# this is x: #}{{ x }}{% else %}foo{% endfor %}" `shouldRender` "123"

shouldRender
  :: (HasCallStack, Show e, Eq e, Show a, Eq a)
  => Either e (ws, a)
  -> a
  -> Expectation
tmpl `shouldRender` res =
  map (\(_, a) -> a) tmpl `shouldBe` Right res

shouldWarn
  :: (HasCallStack, Show e, Eq e, Show ws, Eq ws, Show a, Eq a)
  => Either e (ws, a)
  -> ws
  -> Expectation
tmpl `shouldWarn` res =
  map (\(ws, _) -> ws) tmpl `shouldBe` Right res

shouldRaise
  :: (HasCallStack, Show e, Eq e, Show ws, Eq ws, Show a, Eq a)
  => Either e (ws, a)
  -> e
  -> Expectation
tmpl `shouldRaise` res =
  tmpl `shouldBe` Left res

rWith :: Aeson.Value -> Text -> Either Error ([Warning], Lazy.Text)
rWith json tmplStr = do
  let
    Aeson.Object o = json
    scope =
      Scope (map embedAeson (HashMap.mapKeys Name (Aeson.toHashMapText o)))
    stdlib =
      Stdlib.with (def.ops <> opExt) (def.funs <> funExt) (def.macros <> macroExt)
    Right tmpl =
      parse stdlib (Text.encodeUtf8 tmplStr)
  render (stdlib, scope) tmpl

opExt :: [Stdlib.Op]
opExt =
  [ Stdlib.Op "<+>" (flip embed0 (\str0 str1 -> str0 <> "+" <> str1 :: Text)) Stdlib.Infixr 6
  ]

funExt :: [Stdlib.Fun]
funExt =
  [ Stdlib.Fun "bool01" (flip embed0 (bool @Int 0 1))
  , Stdlib.Fun "const" (flip embed0 (const @Bool @Text))
  ]

macroExt :: [Stdlib.Macro]
macroExt =
  [ Stdlib.macroOp "?||" coalesceOp Op.Infixl 1
  ]
 where
  coalesceOp ann = \case
    [expl, expr] ->
      Right (appE ann "coalesce" [expl, expr])
    args ->
      badArity 2 (List.length args)

r_ :: Text -> Either Error ([Warning], Lazy.Text)
r_ =
  rWith [aesonQQ| {} |]
