{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
module T.ParseSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Vector qualified as Vector
import Test.Hspec

import T.Parse (parse)
import T.Exp (Exp, Literal(..), litE_, varE_, ifE_, appE_)
import T.Exp.Ann (noann)
import T.Name (Name(..))
import T.Prelude
import T.Stdlib qualified as Stdlib
import T.Tmpl (Tmpl)
import T.Tmpl qualified as Tmpl


spec :: Spec
spec =
  describe "parse" $ do
    it "examples" $ do
      "" `shouldParseTo` Tmpl.Cat []
      "foo" `shouldParseTo` "foo"
      "{{ x }}" `shouldParseTo` Tmpl.Exp (var "x")
      "{{ x? }}" `shouldParseTo` Tmpl.Exp (var "x?")
      "{{ foo-bar }}" `shouldParseTo` Tmpl.Exp (var "foo-bar")
      "{{ x }}{{ y }}" `shouldParseTo` Tmpl.Cat [Tmpl.Exp (var "x"), Tmpl.Exp (var "y")]
      "{{ x.y.z }}" `shouldParseTo`
        Tmpl.Exp
          (appE_
            "."
            [ appE_ "." [var "x", string "y"]
            , string "z"
            ])
      "{{ x.y.z }}" `shouldParseTo` Tmpl.Exp (vars ["x", "y", "z"])
      "foo{{ x }}" `shouldParseTo` Tmpl.Cat ["foo", Tmpl.Exp (var "x")]
      "foo{{ x }}bar" `shouldParseTo` Tmpl.Cat ["foo", Tmpl.Exp (var "x"), "bar"]
      "{{ null }}" `shouldParseTo` Tmpl.Exp null
      "{{ false }}" `shouldParseTo` Tmpl.Exp false
      "{{ true }}" `shouldParseTo` Tmpl.Exp true
      "{{ 4 }}" `shouldParseTo` Tmpl.Exp (int 4)
      "{{ 4.0 }}" `shouldParseTo` Tmpl.Exp (double 4)
      "{{ 4.2 }}" `shouldParseTo` Tmpl.Exp (double 4.2)
      "{{ \"foo\" }}" `shouldParseTo` Tmpl.Exp (string "foo")
      "{{ [] }}" `shouldParseTo` Tmpl.Exp (array [])
      "{{ [1, 2, 3] }}" `shouldParseTo` Tmpl.Exp (array [int 1, int 2, int 3])
      "{{ [1, x.y, \"foo\"] }}" `shouldParseTo`
        Tmpl.Exp
          (array
            [ int 1
            , vars ["x", "y"]
            , string "foo"
            ])
      "{{ {} }}" `shouldParseTo` Tmpl.Exp (object [])
      "{{ {x: 4} }}" `shouldParseTo` Tmpl.Exp (object [("x", int 4)])
      "{{ {x: 4, y: \"foo\"} }}" `shouldParseTo`
        Tmpl.Exp
          (object
            [ ("x", int 4)
            , ("y", string "foo")
            ])
      "{{ if 4 then \"foo\" else 7 }}" `shouldParseTo`
        Tmpl.Exp (ifE_ (int 4) (string "foo") (int 7))
      "{% set x = 4 %}" `shouldParseTo` Tmpl.Set [Tmpl.Assign "x" (int 4)]
      "{% let x = 4 %}foo{% endlet %}" `shouldParseTo` Tmpl.Let [Tmpl.Assign "x" (int 4)] "foo"
      "{% if x %}t{% endif %}" `shouldParseTo`
        whenIf (var "x") "t"
      "{% if x %}t{% else %}f{% endif %}" `shouldParseTo`
        simpleIf (var "x") "t" "f"
      "{% if 4 %}4{% elif 7 %}7{% endif %}" `shouldParseTo`
        Tmpl.If [(int 4, "4"), (int 7, "7")]
      "{% if 4 %}4{% elif 7 %}7{% else %}foo{% endif %}" `shouldParseTo`
        Tmpl.If [(int 4, "4"), (int 7, "7"), (true, "foo")]
      "{{ foo(bar) }}" `shouldParseTo`
        Tmpl.Exp (appE_ "foo" [var "bar"])
      "{{ bar | foo }}" `shouldParseTo`
        Tmpl.Exp (appE_ "foo" [var "bar"])
      "{{ bar | foo(baz) }}" `shouldParseTo`
        Tmpl.Exp (appE_ "foo" [var "baz", var "bar"])
      "{{ foo(bar, baz) }}" `shouldParseTo`
        Tmpl.Exp (appE_ "foo" [var "bar", var "baz"])
      "{% if 4 && null %}foo{% endif %}" `shouldParseTo`
        whenIf (ifE_ (int 4) null false) "foo"
      "{% if null || 4 %}foo{% endif %}" `shouldParseTo`
        whenIf (ifE_ null true (int 4)) "foo"
      "{% if ! true %}foo{% endif %}" `shouldParseTo`
        whenIf (appE_ "!" [true]) "foo"
      "{% for x in [1, 2, 3] %}{{ x }}{% endfor %}" `shouldParseTo`
        Tmpl.For "x" Nothing (array [int 1, int 2, int 3]) (Tmpl.Exp (var "x")) Nothing
      "{% for x in [] %}{{ x }}{% else %}foo{% endfor %}" `shouldParseTo`
        Tmpl.For "x" Nothing (array []) (Tmpl.Exp (var "x")) (pure "foo")
      "{% for x, it in [1, 2, 3] %}{{ x }}{% endfor %}" `shouldParseTo`
        Tmpl.For "x" (Just "it") (array [int 1, int 2, int 3]) (Tmpl.Exp (var "x")) Nothing
      "{# yo #}foo" `shouldParseTo`
        Tmpl.Cat [Tmpl.Comment "yo", Tmpl.Raw "foo"]
      "{# yo #}\nfoo" `shouldParseTo`
        Tmpl.Cat [Tmpl.Comment "yo", Tmpl.Raw "foo"]

    context "nesting" $
      it "examples" $ do
        "{{ (null) }}" `shouldParseTo` Tmpl.Exp null
        "{{ ((null)) }}" `shouldParseTo` Tmpl.Exp null
        "{{ 1 + 2 + 3 }}" `shouldParseTo`
          Tmpl.Exp
            (appE_
              "+"
              [ appE_
                  "+"
                  [ int 1
                  , int 2
                  ]
              , int 3
              ])
        "{{ (1 + 2) + 3 }}" `shouldParseTo`
          Tmpl.Exp
            (appE_
              "+"
              [ appE_
                  "+"
                  [ int 1
                  , int 2
                  ]
              , int 3
              ])
        "{{ 1 + (2 + 3) }}" `shouldParseTo`
          Tmpl.Exp
            (appE_
              "+"
              [ int 1
              , appE_
                  "+"
                  [ int 2
                  , int 3
                  ]
              ])

    context "if" $
      it "can nest arbitrarily" $
        "{% if x %}{% if y %}tt{% else %}tf{% endif %}{% else %}{% if z %}ft{% else %}ff{% endif %}{% endif %}" `shouldParseTo`
          simpleIf (var "x")
            (simpleIf (var "y") "tt" "tf")
            (simpleIf (var "z") "ft" "ff")

    context "layout" $
      it "layouts" $ do
        "{% set\n\
        \     foo =\n\
        \       4\n\
        \%}" `shouldParseTo` Tmpl.Set [Tmpl.Assign "foo" (int 4)]

    context "multi-*" $ do
      it "multi-sets" $ do
        "{% set\n\
        \     foo =\n\
        \       4\n\
        \     bar =\n\
        \       7\n\
        \     baz =\n\
        \       \"foo\"\n\
        \%}" `shouldParseTo` Tmpl.Set
          [ Tmpl.Assign "foo" (int 4)
          , Tmpl.Assign "bar" (int 7)
          , Tmpl.Assign "baz" (string "foo")
          ]

      it "multi-lets" $ do
        "{% let\n\
        \     foo =\n\
        \       4\n\
        \     bar =\n\
        \       7\n\
        \     baz =\n\
        \       \"foo\"\n\
        \%}{% endlet %}" `shouldParseTo`
          (Tmpl.Let
            [ Tmpl.Assign "foo" (int 4)
            , Tmpl.Assign "bar" (int 7)
            , Tmpl.Assign "baz" (string "foo")
            ]
            (Tmpl.Cat []))

vars :: NonEmpty Name -> Exp
vars (chunk :| chunks) =
  foldl' (\acc chunk' -> appE_ "." [acc, string chunk'.unName]) (var chunk) chunks

var :: Name -> Exp
var =
  varE_ . noann

null :: Exp
null =
  litE_ Null

false :: Exp
false =
  litE_ (Bool False)

true :: Exp
true =
  litE_ (Bool True)

int :: Int64 -> Exp
int =
  litE_ . Int

double :: Double -> Exp
double =
  litE_ . Double

string :: Text -> Exp
string =
  litE_ . String

array :: [Exp] -> Exp
array =
  litE_ . Array . Vector.fromList

object :: [(Text, Exp)] -> Exp
object =
  litE_ . Object . HashMap.fromList

simpleIf :: Exp -> Tmpl -> Tmpl -> Tmpl
simpleIf p thenTmpl elseTmpl =
  Tmpl.If [(p, thenTmpl), (true, elseTmpl)]

whenIf :: Exp -> Tmpl -> Tmpl
whenIf p thenTmpl =
  Tmpl.If [(p, thenTmpl)]

shouldParseTo :: HasCallStack => ByteString -> Tmpl -> Expectation
tmpl `shouldParseTo` res =
  first show (parse Stdlib.def tmpl) `shouldBe` Right res
