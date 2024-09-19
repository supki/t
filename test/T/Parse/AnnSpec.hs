module T.Parse.AnnSpec (spec) where

import Data.ByteString (ByteString)
import Test.Hspec

import T


spec :: Spec
spec =
  describe "annotations" $
    context "literals" $ do
      it "null" $
        errorOf "{{ null + 1 }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in +: \n\
          \  expected: something convertable to Scientific\n\
          \   but got: null : null\n\
          \1 | {{ null + 1 }}<EOF> \n\
          \  |    ~~~~             "

      it "bool" $
        errorOf "{{ true + 1 }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in +: \n\
          \  expected: something convertable to Scientific\n\
          \   but got: true : bool\n\
          \1 | {{ true + 1 }}<EOF> \n\
          \  |    ~~~~             "

      it "number" $
        errorOf "{{ 1 <> \"foo\" }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in <>: \n\
          \  expected: something convertable to Text\n\
          \   but got: 1 : number\n\
          \1 | {{ 1 <> \"foo\" }}<EOF> \n\
          \  |    ~                  "

      it "string" $
        errorOf "{{ \"foo\" + 1 }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in +: \n\
          \  expected: something convertable to Scientific\n\
          \   but got: \"foo\" : string\n\
          \1 | {{ \"foo\" + 1 }}<EOF> \n\
          \  |    ~~~~~             "

      it "regexp" $ do
        errorOf "{{ /foo/ + 1 }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in +: \n\
          \  expected: something convertable to Scientific\n\
          \   but got: \"<regexp>\" : regexp\n\
          \1 | {{ /foo/ + 1 }}<EOF> \n\
          \  |    ~~~~~             "

      it "array" $ do
        errorOf "{{ [1,2,3] + 1 }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in +: \n\
          \  expected: something convertable to Scientific\n\
          \   but got: [1,2,3] : array\n\
          \1 | {{ [1,2,3] + 1 }}<EOF> \n\
          \  |    ~~~~~~~             "

      it "object" $ do
        errorOf "{{ {foo:4} + 1 }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in +: \n\
          \  expected: something convertable to Scientific\n\
          \   but got: {\"foo\":4} : object\n\
          \1 | {{ {foo:4} + 1 }}<EOF> \n\
          \  |    ~~~~~~~             "

errorOf :: ByteString -> String
errorOf str =
  let
    Right parsed = parse stdlib str
  in
    either (show . prettyError) (error "not-supposed-to-render") (render (stdlib, mempty) parsed)
