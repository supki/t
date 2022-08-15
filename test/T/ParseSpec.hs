{-# LANGUAGE OverloadedLists #-}
module T.ParseSpec (spec) where

import           Data.ByteString (ByteString)
import           Data.List (foldl')
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.HashMap.Strict as HashMap
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Vector as Vector
import           Prelude hiding (null)
import           Test.Hspec

import           T.Parse (parse)
import           T.Exp (Exp(..), Literal(..), Name(..))
import           T.Exp.Ann (noann)
import           T.Tmpl (Tmpl((:*:)))
import qualified T.Tmpl as Tmpl


spec :: Spec
spec =
  describe "parse" $ do
    it "examples" $ do
      "" `shouldParseTo` ""
      "foo" `shouldParseTo` "foo"
      "{{ x }}" `shouldParseTo` Tmpl.Exp (var "x")
      "{{ x }}{{ y }}" `shouldParseTo` (Tmpl.Exp (var "x") :*: Tmpl.Exp (var "y"))
      "{{ x.y.z }}" `shouldParseTo`
        Tmpl.Exp
          (App
            (App
              (var ".")
              (App
                (App (var ".") (var "x"))
                (string "y")))
              (string "z"))
      "{{ x.y.z }}" `shouldParseTo` Tmpl.Exp (vars ["x", "y", "z"])
      "foo{{ x }}" `shouldParseTo` ("foo" :*: Tmpl.Exp (var "x"))
      "foo{{ x }}bar" `shouldParseTo` ("foo" :*: Tmpl.Exp (var "x") :*: "bar")
      "{{ null }}" `shouldParseTo` Tmpl.Exp null
      "{{ false }}" `shouldParseTo` Tmpl.Exp false
      "{{ true }}" `shouldParseTo` Tmpl.Exp true
      "{{ 4 }}" `shouldParseTo` Tmpl.Exp (number 4)
      "{{ \"foo\" }}" `shouldParseTo` Tmpl.Exp (string "foo")
      "{{ [] }}" `shouldParseTo` Tmpl.Exp (array [])
      "{{ [1, 2, 3] }}" `shouldParseTo` Tmpl.Exp (array [number 1, number 2, number 3])
      "{{ [1, x.y, \"foo\"] }}" `shouldParseTo`
        Tmpl.Exp
          (array
            [ number 1
            , vars ["x", "y"]
            , string "foo"
            ])
      "{{ {} }}" `shouldParseTo` Tmpl.Exp (object [])
      "{{ {x: 4} }}" `shouldParseTo` Tmpl.Exp (object [("x", number 4)])
      "{{ {x: 4, y: \"foo\"} }}" `shouldParseTo`
        Tmpl.Exp
          (object
            [ ("x", number 4)
            , ("y", string "foo")
            ])
      "{{ if 4 then \"foo\" else 7 }}" `shouldParseTo`
        Tmpl.Exp (If (number 4) (string "foo") (number 7))
      "{% set x = 4 %}" `shouldParseTo` Tmpl.Set "x" (number 4)
      "{% let x = 4 %}foo{% endlet %}" `shouldParseTo` Tmpl.Let "x" (number 4) "foo"
      "{% if x %}t{% endif %}" `shouldParseTo`
        whenIf (var "x") "t"
      "{% if x %}t{% else %}f{% endif %}" `shouldParseTo`
        simpleIf (var "x") "t" "f"
      "{% if 4 %}4{% elif 7 %}7{% endif %}" `shouldParseTo`
        Tmpl.If [(number 4, "4"), (number 7, "7")]
      "{% if 4 %}4{% elif 7 %}7{% else %}foo{% endif %}" `shouldParseTo`
        Tmpl.If [(number 4, "4"), (number 7, "7"), (true, "foo")]
      "{{ foo(bar) }}" `shouldParseTo` Tmpl.Exp (App (var "foo") (var "bar"))
      "{{ foo(bar, baz) }}" `shouldParseTo`
        Tmpl.Exp (App (App (var "foo") (var "bar")) (var "baz"))
      "{% if 4 && null %}foo{% endif %}" `shouldParseTo`
        whenIf (If (number 4) null false) "foo"
      "{% if null || 4 %}foo{% endif %}" `shouldParseTo`
        whenIf (If null true (number 4)) "foo"
      "{% if ! true %}foo{% endif %}" `shouldParseTo`
        whenIf (App (var "!") true) "foo"
      "{% for x in [1, 2, 3] %}{{ x }}{% endfor %}" `shouldParseTo`
        Tmpl.For "x" Nothing (array [number 1, number 2, number 3]) (Tmpl.Exp (var "x")) Nothing
      "{% for x in [] %}{{ x }}{% else %}foo{% endfor %}" `shouldParseTo`
        Tmpl.For "x" Nothing (array []) (Tmpl.Exp (var "x")) (pure "foo")
      "{% for x, it in [1, 2, 3] %}{{ x }}{% endfor %}" `shouldParseTo`
        Tmpl.For "x" (Just "it") (array [number 1, number 2, number 3]) (Tmpl.Exp (var "x")) Nothing

    context "if" $
      it "can nest arbitrarily" $
        "{% if x %}{% if y %}tt{% else %}tf{% endif %}{% else %}{% if z %}ft{% else %}ff{% endif %}{% endif %}" `shouldParseTo`
          simpleIf (var "x")
            (simpleIf (var "y") "tt" "tf")
            (simpleIf (var "z") "ft" "ff")

vars :: NonEmpty Name -> Exp
vars (chunk :| chunks) =
  foldl' (\acc chunk' -> App (App (var ".") acc) (string (unName chunk'))) (var chunk) chunks

var :: Name -> Exp
var =
  Var . noann

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
  Lit . Array . Vector.fromList

object :: [(Text, Exp)] -> Exp
object =
  Lit . Object . HashMap.fromList

simpleIf :: Exp -> Tmpl -> Tmpl -> Tmpl
simpleIf p thenTmpl elseTmpl =
  Tmpl.If [(p, thenTmpl), (true, elseTmpl)]

whenIf :: Exp -> Tmpl -> Tmpl
whenIf p thenTmpl =
  Tmpl.If [(p, thenTmpl)]

shouldParseTo :: HasCallStack => ByteString -> Tmpl -> Expectation
tmpl `shouldParseTo` res =
  parse tmpl `shouldBe` Right res
