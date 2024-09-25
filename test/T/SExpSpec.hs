{-# LANGUAGE OverloadedLists #-}
module T.SExpSpec (spec) where

import Data.Text.Lazy.Builder (Builder)
import Test.Hspec
import Text.Regex.PCRE.Light qualified as Pcre

import T.Parse (parse)
import T.Prelude
import T.Exp (Exp)
import T.SExp (render, sexp)
import T.Stdlib qualified as Stdlib
import T.Tmpl (Tmpl)
import T.Tmpl qualified as Tmpl
import T.Value qualified as Value


spec :: Spec
spec = do
  context "tmpl" $ do
    it "raw" $
      rexp2 "foo" `shouldBe` "(raw \"foo\")"

    it "comment" $
      rexp2 "{# bar #}" `shouldBe` "(comment \"bar\")"

    it "exp" $
      rexp2 "{{ 4 }}" `shouldBe` "(exp 4)"

    it "set" $
      rexp2 "{% set foo = 4 bar = 7 %}" `shouldBe` "(set [[foo 4] [bar 7]])"

    it "let" $
      rexp2 "{% let foo = 4 bar = 7 %}{{ foo + bar }}{% endlet %}" `shouldBe`
        "(let [[foo 4] [bar 7]] (exp (+ foo bar)))"

    it "if" $
      rexp2 "{% if true %}4{% elif false %}7{% endif %}" `shouldBe`
        "(if [[true (raw \"4\")] [false (raw \"7\")]])"

    it "for" $ do
      rexp2 "{% for k in [1,2,3] %}4{% endfor %}" `shouldBe`
        "(for [\"k\" _key] [1 2 3] (raw \"4\") _else)"
      rexp2 "{% for k, v in [1,2,3] %}4{% else %}7{% endfor %}" `shouldBe`
        "(for [\"k\" \"v\"] [1 2 3] (raw \"4\") (raw \"7\"))"

    it "cat" $
      rexp2 "{{ 4 }}foo{{ 7 }}" `shouldBe`
        "[(exp 4) (raw \"foo\") (exp 7)]"

  context "exp" $ do
    context "lit" $ do
      it "null" $
        rexp "{{ null }}" `shouldBe` "null"

      it "bool" $ do
        rexp "{{ true }}" `shouldBe` "true"
        rexp "{{ false }}" `shouldBe` "false"

      it "int" $
        rexp "{{ 42 }}" `shouldBe` "42"

      it "double" $
        rexp "{{ 4.2 }}" `shouldBe` "4.2"

      it "string" $
        rexp "{{ \"foo\" }}" `shouldBe` "\"foo\""

      it "regexp" $
        rexp "{{ /foo/ }}" `shouldBe` "(regexp \"foo\")"

      it "array" $
        rexp "{{ [false, 42] }}" `shouldBe` "[false 42]"

      it "object" $
        rexp "{{ {foo: 42, bar: 4.2} }}" `shouldBe` "{\"bar\" 4.2 \"foo\" 42}"

    it "var" $
      rexp "{{ foo }}" `shouldBe` "foo"

    it "if" $
      rexp "{{ if true then 4 else 7 }}" `shouldBe` "(if true 4 7)"

    it "app" $
      rexp "{{ foo(4, 7) }}" `shouldBe` "(foo 4 7)"

  context "value" $ do
    it "null" $
      render (sexp Value.Null) `shouldBe` "null"

    it "bool" $ do
      render (sexp (Value.Bool False)) `shouldBe` "false"
      render (sexp (Value.Bool True)) `shouldBe` "true"

    it "int" $
      render (sexp (Value.Int 42)) `shouldBe` "42"

    it "double" $
      render (sexp (Value.Double 4.2)) `shouldBe` "4.2"

    it "regexp" $ do
      let
        Right regexp =
          Pcre.compileM "foo" []
      render (sexp (Value.Regexp regexp)) `shouldBe` "(regexp \"foo\")"

    it "string" $
      render (sexp (Value.String "foo")) `shouldBe` "\"foo\""

    it "array" $
      render (sexp (Value.Array [Value.Int 1, Value.Int 2, Value.Int 3])) `shouldBe` "[1 2 3]"

    it "object" $
      render (sexp (Value.Object [("foo", Value.Int 4), ("bar",  Value.Int 7)])) `shouldBe`
        "{\"bar\" 7 \"foo\" 4}"

    it "lam" $
      render (sexp (Value.Lam (\_ -> pure Value.Null))) `shouldBe` "(lambda [_] ...)"

rexp :: ByteString -> Builder
rexp =
  render . sexp . pexp

pexp :: ByteString -> Exp
pexp str = do
  let
    Tmpl.Exp exp =
      texp str
  exp

rexp2 :: ByteString -> Builder
rexp2 =
  render . sexp . texp

texp :: ByteString -> Tmpl
texp str = do
  let
    Right tmpl =
      parse Stdlib.def str
  tmpl
