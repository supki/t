{-# LANGUAGE OverloadedLists #-}
module T.ParseSpec (spec) where

import           Data.List (foldl')
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.HashMap.Strict as HashMap
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Vector as Vector
import           Prelude hiding (null)
import           Test.Hspec

import           T.Parse (parse)
import           T.Exp (Tmpl(..), Exp(..), Literal(..), Name(..))


spec :: Spec
spec =
  describe "parse" $ do
    it "examples" $ do
      parse "" `shouldBe` Right ""
      parse "foo" `shouldBe` Right "foo"
      parse "{{ x }}" `shouldBe` Right (Exp (var "x"))
      parse "{{ x }}{{ y }}" `shouldBe` Right (Exp (var "x") :*: Exp (var "y"))
      parse "{{ x.y.z }}" `shouldBe`
        Right
          (Exp
            (App
              (App
                (var ".")
                (App
                  (App (var ".") (var "x"))
                  (string "y")))
                (string "z")))
      parse "{{ x.y.z }}" `shouldBe` Right (Exp (vars ["x", "y", "z"]))
      parse "foo{{ x }}" `shouldBe` Right ("foo" :*: Exp (var "x"))
      parse "foo{{ x }}bar" `shouldBe` Right ("foo" :*: Exp (var "x") :*: "bar")
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
              , vars ["x", "y"]
              , string "foo"
              ]))
      parse "{{ {} }}" `shouldBe` Right (Exp (object []))
      parse "{{ {x: 4} }}" `shouldBe` Right (Exp (object [("x", number 4)]))
      parse "{{ {x: 4, y: \"foo\"} }}" `shouldBe`
        Right
          (Exp
            (object
              [ ("x", number 4)
              , ("y", string "foo")
              ]))
      parse "{% set x = 4 %}" `shouldBe` Right (Set "x" (number 4))
      parse "{% let x = 4 %}foo{% endlet %}" `shouldBe` Right (Let "x" (number 4) "foo")
      parse "{% if x %}t{% endif %}" `shouldBe`
        Right (whenIf (var "x") "t")
      parse "{% if x %}t{% else %}f{% endif %}" `shouldBe`
        Right (simpleIf (var "x") "t" "f")
      parse "{% if 4 %}4{% elif 7 %}7{% endif %}" `shouldBe`
        Right (If [(number 4, "4"), (number 7, "7")])
      parse "{% if 4 %}4{% elif 7 %}7{% else %}foo{% endif %}" `shouldBe`
        Right (If [(number 4, "4"), (number 7, "7"), (true, "foo")])
      parse "{{ foo(bar) }}" `shouldBe` Right (Exp (App (var "foo") (var "bar")))
      parse "{{ foo(bar, baz) }}" `shouldBe`
        Right (Exp (App (App (var "foo") (var "bar")) (var "baz")))
      parse "{% if true && false %}foo{% endif %}" `shouldBe`
        Right (whenIf (App (App (var "&&") true) false) "foo")
      parse "{% if ! true %}foo{% endif %}" `shouldBe`
        Right (whenIf (App (var "!") true) "foo")
      parse "{% for x in [1, 2, 3] %}{{ x }}{% endfor %}" `shouldBe`
        Right (For "x" Nothing (array [number 1, number 2, number 3]) (Exp (var "x")) Nothing)
      parse "{% for x in [] %}{{ x }}{% else %}foo{% endfor %}" `shouldBe`
        Right (For "x" Nothing (array []) (Exp (var "x")) (pure "foo"))
      parse "{% for x, it in [1, 2, 3] %}{{ x }}{% endfor %}" `shouldBe`
        Right (For "x" (Just "it") (array [number 1, number 2, number 3]) (Exp (var "x")) Nothing)

    context "if" $
      it "can nest arbitrarily" $
        parse "{% if x %}{% if y %}tt{% else %}tf{% endif %}{% else %}{% if z %}ft{% else %}ff{% endif %}{% endif %}" `shouldBe`
          Right
            (simpleIf (var "x")
              (simpleIf (var "y") "tt" "tf")
              (simpleIf (var "z") "ft" "ff"))

vars :: NonEmpty Name -> Exp
vars (chunk :| chunks) =
  foldl' (\acc chunk' -> App (App (Var ".") acc) (string (unName chunk'))) (Var chunk) chunks

var :: Name -> Exp
var =
  Var

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
  If [(p, thenTmpl), (true, elseTmpl)]

whenIf :: Exp -> Tmpl -> Tmpl
whenIf p thenTmpl =
  If [(p, thenTmpl)]
