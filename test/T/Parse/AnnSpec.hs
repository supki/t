module T.Parse.AnnSpec (spec) where

import Test.Hspec

import T
import T.Prelude


spec :: Spec
spec =
  describe "annotations" $
    context "literals" $ do
      it "null" $
        errorOf "{{ null + 1 }}" `shouldBe`
          "(interactive):1:4: error: +: not applicable to null : null (not an integer or double)\n\
          \1 | {{ null + 1 }}<EOF> \n\
          \  |    ~~~~             "

      it "bool" $
        errorOf "{{ true + 1 }}" `shouldBe`
          "(interactive):1:4: error: +: not applicable to true : bool (not an integer or double)\n\
          \1 | {{ true + 1 }}<EOF> \n\
          \  |    ~~~~             "

      it "int" $
        errorOf "{{ 1 <> \"foo\" }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in <>: \n\
          \  expected: something convertable to Text\n\
          \   but got: 1 : int\n\
          \1 | {{ 1 <> \"foo\" }}<EOF> \n\
          \  |    ~                  "

      it "double" $
        errorOf "{{ 1.0 <> \"foo\" }}" `shouldBe`
          "(interactive):1:4: error: mismatched types in <>: \n\
          \  expected: something convertable to Text\n\
          \   but got: 1 : double\n\
          \1 | {{ 1.0 <> \"foo\" }}<EOF> \n\
          \  |    ~~~                  "

      it "string" $
        errorOf "{{ \"foo\" + 1 }}" `shouldBe`
          "(interactive):1:4: error: +: not applicable to \"foo\" : string (not an integer or double)\n\
          \1 | {{ \"foo\" + 1 }}<EOF> \n\
          \  |    ~~~~~             "

      it "regexp" $ do
        errorOf "{{ /foo/ + 1 }}" `shouldBe`
          "(interactive):1:4: error: +: not applicable to \"<regexp>\" : regexp (not an integer or double)\n\
          \1 | {{ /foo/ + 1 }}<EOF> \n\
          \  |    ~~~~~             "

      it "array" $ do
        errorOf "{{ [1,2,3] + 1 }}" `shouldBe`
          "(interactive):1:4: error: +: not applicable to [1,2,3] : array (not an integer or double)\n\
          \1 | {{ [1,2,3] + 1 }}<EOF> \n\
          \  |    ~~~~~~~             "

      it "object" $ do
        errorOf "{{ {foo:4} + 1 }}" `shouldBe`
          "(interactive):1:4: error: +: not applicable to {\"foo\":4} : object (not an integer or double)\n\
          \1 | {{ {foo:4} + 1 }}<EOF> \n\
          \  |    ~~~~~~~             "

errorOf :: ByteString -> String
errorOf str =
  let
    Right parsed = parse stdlib str
  in
    either (show . prettyError) (error "not-supposed-to-render") (render (stdlib, mempty) parsed)
