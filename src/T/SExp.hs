module T.SExp
  ( SExp(..)
  , Paren(..)
  , var
  , round
  , square
  , curly
  , To(..)
  , render
  , renderLazyText
  ) where

import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder
import Prettyprinter (Pretty(..))
import Text.Regex.PCRE.Light.Base qualified as Pcre

import T.Exp.Ann ((:+)(..))
import T.Name (Name(..))
import T.Prelude


data SExp
  = App Paren [SExp]
  | Var Text
    deriving (Show, Eq)

instance IsString SExp where
  fromString =
    Var . fromString

instance Pretty SExp where
  pretty =
    pretty . Builder.toLazyText . render

data Paren
  = Round
  | Square
  | Curly
    deriving (Show, Eq)

var :: Text -> SExp
var = Var

round :: [SExp] -> SExp
round =
  app Round

square :: [SExp] -> SExp
square =
  app Square

curly :: [SExp] -> SExp
curly =
  app Curly

app :: Paren -> [SExp] -> SExp
app parenType children =
  App parenType children

class To a where
  sexp :: a -> SExp

instance To SExp where
  sexp x = x

instance To Bool where
  sexp =
    Var . \case
      False -> "false"
      True -> "true"

instance To Int where
  sexp =
    Var . fromString . show

instance To Double where
  sexp =
    Var . fromString . show

instance To Text where
  sexp =
    Var . fromString . show

instance To Pcre.Regex where
  sexp (Pcre.Regex _ptr bytes) =
    sexp (Text.decodeUtf8Lenient bytes)

instance To Name where
  sexp (Name name) =
    Var name

instance name ~ Name => To (ann :+ name) where
  sexp (_ann :+ Name name) =
    sexp name

instance To a => To [a] where
  sexp =
    App Square . map sexp

instance To a => To (Vector a) where
  sexp =
    sexp . toList

instance (Ord k, To k, To v) => To (HashMap k v) where
  sexp xs =
    curly
      (concatMap (\(k, v) -> [sexp k, sexp v])
      (List.sortOn
        (\(k, _v) -> k)
        (HashMap.toList xs)))

renderLazyText :: To sexp => sexp -> Lazy.Text
renderLazyText =
  Builder.toLazyText . render

render :: To sexp => sexp -> Builder
render =
  go . sexp
 where
  go = \case
    App parenType children ->
      fromParenType parenType (sepBy (Builder.singleton ' ') (map go children))
    Var name ->
      Builder.fromText name
  fromParenType = \case
    Round -> parens
    Square -> brackets
    Curly -> braces

-- this is a sort of `intercalate` for `Builder`s
sepBy :: Builder -> [Builder] -> Builder
sepBy sep = go
 where
  go [] =
    mempty
  go (x0 : []) =
    x0
  go (x0 : xs@(_ : _)) =
    x0 <> sep <> go xs

parens :: Builder -> Builder
parens =
  between '(' ')'

brackets :: Builder -> Builder
brackets =
  between '[' ']'

braces :: Builder -> Builder
braces =
  between '{' '}'

between :: Char -> Char -> Builder -> Builder
between l r inside =
  Builder.singleton l <> inside <> Builder.singleton r
