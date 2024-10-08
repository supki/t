module T.Parse.AnnSpec (spec) where

import Test.Hspec

import T
import T.Prelude


spec :: Spec
spec =
  describe "annotations" $ do
    context "literals" $ do
      it "null" $
        errorOf "{{ null + 1 }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in +: \n\
          \  expected: Number\n\
          \   but got: null : Null\n\
          \1 | {{ null + 1 }}<EOF> \n\
          \  |    ~~~~             "

      it "bool" $
        errorOf "{{ true + 1 }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in +: \n\
          \  expected: Number\n\
          \   but got: true : Bool\n\
          \1 | {{ true + 1 }}<EOF> \n\
          \  |    ~~~~             "

      it "int" $
        errorOf "{{ 1 <> \"foo\" }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in <>: \n\
          \  expected: String\n\
          \   but got: 1 : Int\n\
          \1 | {{ 1 <> \"foo\" }}<EOF> \n\
          \  |    ~                  "

      it "double" $
        errorOf "{{ 1.0 <> \"foo\" }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in <>: \n\
          \  expected: String\n\
          \   but got: 1.0 : Double\n\
          \1 | {{ 1.0 <> \"foo\" }}<EOF> \n\
          \  |    ~~~                  "

      it "string" $
        errorOf "{{ \"foo\" + 1 }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in +: \n\
          \  expected: Number\n\
          \   but got: \"foo\" : String\n\
          \1 | {{ \"foo\" + 1 }}<EOF> \n\
          \  |    ~~~~~             "

      it "regexp" $ do
        errorOf "{{ /foo/ + 1 }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in +: \n\
          \  expected: Number\n\
          \   but got: (regexp \"foo\") : Regexp\n\
          \1 | {{ /foo/ + 1 }}<EOF> \n\
          \  |    ~~~~~             "

      it "array" $ do
        errorOf "{{ [1,2,3] + 1 }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in +: \n\
          \  expected: Number\n\
          \   but got: [1 2 3] : Array\n\
          \1 | {{ [1,2,3] + 1 }}<EOF> \n\
          \  |    ~~~~~~~             "

      it "record" $ do
        errorOf "{{ {foo:4} + 1 }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in +: \n\
          \  expected: Number\n\
          \   but got: {foo 4} : Record\n\
          \1 | {{ {foo:4} + 1 }}<EOF> \n\
          \  |    ~~~~~~~             "

    context "property access" $ do
      it "record" $ do
        errorOf "{% set foo = {} foo.bar = [] %}{{ foo.bar }}" `shouldBe`
          "(interactive):1:38: error: mismatched types:\n\
          \  expected: Renderable\n\
          \   but got: [] : Array\n\
          \1 | {% set foo = {} foo.bar = [] %}{{ foo.bar }}<EOF> \n\
          \  |                                      ~~~~         "

errorOf :: Text -> String
errorOf str =
  let
    Right parsed = parseText stdlib str
  in
    either (show . prettyError) impossible (render (stdlib, mempty) parsed)
