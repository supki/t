{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
module T.ParseSpec (spec) where

import Data.Either (isLeft)
import Data.HashMap.Strict qualified as HashMap
import Data.Vector qualified as Vector
import Test.Hspec

import T.Parse (parseText)
import T.Exp (Exp, Literal(..), litE_, varE, ifE_, appE_, idxE_, keyE_)
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
          (keyE_
            (keyE_
              (var "x")
              "y")
            "z")
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
      "{{ [1, x.y, \"foo\"] }}" `shouldParseTo` Tmpl.Exp
        (array
          [ int 1
          , keyE_ (var "x") "y"
          , string "foo"
          ])
      "{{ [1, 2, 3][0] }}" `shouldParseTo` Tmpl.Exp
        (idxE_
          (array [int 1, int 2, int 3])
          (int 0))
      "{{ foo.bar[4].baz[7] }}" `shouldParseTo` Tmpl.Exp
        (idxE_
          (keyE_
            (idxE_
              (keyE_
                (var "foo")
                "bar")
              (int 4))
            "baz")
          (int 7))
      "{{ {} }}" `shouldParseTo` Tmpl.Exp (record [])
      "{{ {x: 4} }}" `shouldParseTo` Tmpl.Exp (record [("x", int 4)])
      "{{ {x: 4, y: \"foo\"} }}" `shouldParseTo`
        Tmpl.Exp
          (record
            [ ("x", int 4)
            , ("y", string "foo")
            ])
      "{{ if 4 then \"foo\" else 7 }}" `shouldParseTo`
        Tmpl.Exp (ifE_ (int 4) (string "foo") (int 7))
      "{% set x = 4 %}" `shouldParseTo` Tmpl.Set [Tmpl.Assign (var "x") (int 4)]
      "{% let x = 4 %}foo{% endlet %}" `shouldParseTo` Tmpl.Let [Tmpl.Assign (var "x") (int 4)] "foo"
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
        \%}" `shouldParseTo` Tmpl.Set [Tmpl.Assign (var "foo") (int 4)]
        "{% set\n\
        \     arr =\n\
        \       [ 4\n\
        \       , 7\n\
        \       , 42\n\
        \       ]\n\
        \%}" `shouldParseTo` Tmpl.Set
          [ Tmpl.Assign
              (var "arr")
              (array
                [ int 4
                , int 7
                , int 42
                ])
          ]
        "{% set\n\
        \     rec =\n\
        \       { foo: 4\n\
        \       , bar: 7\n\
        \       , baz: 42\n\
        \       }\n\
        \%}" `shouldParseTo` Tmpl.Set
          [ Tmpl.Assign
              (var "rec")
              (record
                [ ("foo", int 4)
                , ("bar", int 7)
                , ("baz", int 42)
                ])
          ]

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
          [ Tmpl.Assign (var "foo") (int 4)
          , Tmpl.Assign (var "bar") (int 7)
          , Tmpl.Assign (var "baz") (string "foo")
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
            [ Tmpl.Assign (var "foo") (int 4)
            , Tmpl.Assign (var "bar") (int 7)
            , Tmpl.Assign (var "baz") (string "foo")
            ]
            (Tmpl.Cat []))

    it "fake block" $
      parse "{% fake %}" `shouldSatisfy` isLeft

var :: Name -> Exp
var name =
  varE (noann name)

null :: Exp
null =
  litE_ Null

false :: Exp
false =
  litE_ (Bool False)

true :: Exp
true =
  litE_ (Bool True)

int :: Int -> Exp
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

record :: [(Text, Exp)] -> Exp
record =
  litE_ . Record . HashMap.fromList

simpleIf :: Exp -> Tmpl -> Tmpl -> Tmpl
simpleIf p thenTmpl elseTmpl =
  Tmpl.If [(p, thenTmpl), (true, elseTmpl)]

whenIf :: Exp -> Tmpl -> Tmpl
whenIf p thenTmpl =
  Tmpl.If [(p, thenTmpl)]

shouldParseTo :: HasCallStack => Text -> Tmpl -> Expectation
tmpl `shouldParseTo` res =
  parse tmpl `shouldBe` Right res

parse :: Text -> Either String Tmpl
parse tmpl =
  first show (parseText Stdlib.def tmpl)
