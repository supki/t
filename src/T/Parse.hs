{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
module T.Parse
  ( ParseError(..)
  , parseFile
  , parseText
  , parseBytes
  , parser
  ) where

import Control.Monad.Reader (MonadReader, runReaderT, ask)
import Data.ByteString qualified as ByteString (readFile)
import Data.Char qualified as Char
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (fromList)
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Text.Trifecta hiding (err)
import Text.Trifecta.Delta (Delta(..))
import Text.Parser.Expression (Assoc(..), Operator(..), buildExpressionParser)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)
import Text.Parser.Token.Style (emptyOps)
import Text.Regex.PCRE.Light qualified as Pcre

import T.Exp
  ( Cofree(..)
  , Exp
  , ExpF(..)
  , Literal(..)
  , (:+)(..)
  , Ann
  , litE
  , litE_
  , varE
  , ifE
  , appE
  , appE_
  )
import T.Exp.Ann (anning, anned)
import T.Name (Name(..))
import T.Name qualified as Name
import T.Parse.Macro qualified as Macro
import T.Prelude
import T.Tmpl (Tmpl)
import T.Tmpl qualified as Tmpl
import T.Stdlib (Stdlib(..))
import T.Stdlib.Macro qualified as Macro
import T.Stdlib.Op qualified as Op


data ParseError
  = ParseError (Doc AnsiStyle)
    deriving (Show)

parseFile :: Stdlib -> FilePath -> IO (Either ParseError Tmpl)
parseFile stdlib path =
  map (parseDelta stdlib (Directed (fromString path) 0 0 0 0)) (ByteString.readFile path)

parseText :: Stdlib -> Text -> Either ParseError Tmpl
parseText stdlib =
  parseBytes stdlib . Text.encodeUtf8

parseBytes :: Stdlib -> ByteString -> Either ParseError Tmpl
parseBytes stdlib =
  parseDelta stdlib mempty

parseDelta :: Stdlib -> Delta -> ByteString -> Either ParseError Tmpl
parseDelta stdlib delta str =
  case parseByteString (runReaderT parser stdlib) delta str of
    Failure err ->
      Left (ParseError err._errDoc)
    Success tmpl ->
      Right tmpl

parser :: (MonadFail m, e ~ Stdlib, MonadReader e m, DeltaParsing m, LookAheadParsing m) => m Tmpl
parser =
  go []
 where
  go :: (MonadFail m, e ~ Stdlib, MonadReader e m, DeltaParsing m, LookAheadParsing m) => [Tmpl] -> m Tmpl
  go acc =
    asum
      [ do comment <- parseComment
           go (comment : acc)
      , do set <- parseSet
           go (set : acc)
      , do let_ <- parseLet
           go (let_ : acc)
      , do if_ <- parseIf
           go (if_ : acc)
      , do case_ <- parseCase
           go (case_ : acc)
      , do for <- parseFor
           go (for : acc)
      , do exp <- parseExp
           go (exp : acc)
        -- `parseRaw` succeeds on the empty string; so to avoid
        -- looping indefinitely, we check that the parser position
        -- moved at least a bit.
      , do pos0 <- position
           raw <- parseRaw
           pos1 <- position
           bool (go (raw : acc)) (pure (cleanup (reverse acc))) (pos0 == pos1)
      ]
   where
    cleanup [] =
      Tmpl.Cat []
    cleanup [x] =
      x
    cleanup xs =
      Tmpl.Cat xs

parseComment :: CharParsing m => m Tmpl
parseComment =
  map Tmpl.Comment commentP

parseSet :: (e ~ Stdlib, MonadReader e m, DeltaParsing m) => m Tmpl
parseSet = do
  assignments <- blockP "set" (many assignP)
  pure (Tmpl.Set assignments)

parseLet :: (MonadFail m, e ~ Stdlib, MonadReader e m, DeltaParsing m, LookAheadParsing m) => m Tmpl
parseLet = do
  assignments <- blockP "let" (many assignP)
  tmpl <- parser
  _ <- blockP_ "endlet"
  pure (Tmpl.Let assignments tmpl)

assignP :: (e ~ Stdlib, MonadReader e m, DeltaParsing m) => m Tmpl.Assign
assignP = do
  lvalue <- expP
  _ <- symbol "="
  rvalue <- expP
  pure Tmpl.Assign {Tmpl.lvalue, Tmpl.rvalue}

parseIf :: (MonadFail m, e ~ Stdlib, MonadReader e m, DeltaParsing m, LookAheadParsing m) => m Tmpl
parseIf = do
  exp <- blockP "if" expP
  ifTmpl <- parser
  thenClauses <- many (liftA2 (,) (blockP "elif" expP) parser)
  elseTmplQ <- optional (blockP_ "else" *> parser)
  _ <- blockP_ "endif"
  let ifClause =
        (exp, ifTmpl)
      elseClauseQ =
        map (\tmpl -> (litE_ (Bool True), tmpl)) elseTmplQ
  case elseClauseQ of
    Nothing ->
      pure (Tmpl.If (ifClause :| thenClauses))
    Just elseClause ->
      pure (Tmpl.If ((ifClause :| thenClauses) <> (elseClause :| [])))

parseCase :: (MonadFail m, e ~ Stdlib, MonadReader e m, DeltaParsing m, LookAheadParsing m) => m Tmpl
parseCase = do
  exp0 <- blockP "case" expP
  branch : branches <- some (liftA2 (,) (blockP "when" expP) parser)
  elseTmplQ <- optional (blockP_ "else" *> parser)
  _ <- blockP_ "endcase"
  let clauses =
        map (\(exp1, tmpl) -> (appE_ "==" (fromList [exp0, exp1]), tmpl)) (branch :| branches)
      elseClauseQ =
        map (\tmpl -> (litE_ (Bool True), tmpl)) elseTmplQ
  case elseClauseQ of
    Nothing ->
      pure (Tmpl.If clauses)
    Just elseClause ->
      pure (Tmpl.If (clauses <> (elseClause :| [])))

parseFor :: (MonadFail m, e ~ Stdlib, MonadReader e m, DeltaParsing m, LookAheadParsing m) => m Tmpl
parseFor = do
  (name, it, exp) <- blockP "for" $ do
    name <- nameP
    it <- optional $ do
      _ <- symbol ","
      nameP
    _ <- symbol "in"
    exp <- expP
    pure (name, it, exp)
  forTmpl <- parser
  elseTmpl <- optional (blockP_ "else" *> parser)
  _ <- blockP_ "endfor"
  pure (Tmpl.For name it exp forTmpl elseTmpl)

parseExp :: (e ~ Stdlib, MonadReader e m, DeltaParsing m) => m Tmpl
parseExp =
  between (string "{{" *> spaces) (spaces <* string "}}") (map Tmpl.Exp expP)

expP :: (e ~ Stdlib, MonadReader e m, DeltaParsing m) => m Exp
expP = do
  stdlib <- ask
  exp0 <- buildExpressionParser (table stdlib.macros stdlib.ops) goP
  case Macro.expand (Macro.expansions stdlib.macros) exp0 of
    Left expansionErr ->
      unexpected ("expansion error: " <> Macro.displayError expansionErr)
    Right exp ->
      pure exp

 where
  table macros operators =
    [accessOp] : fromMap (Macro.priorities macros <> Op.priorities operators)

  fromMap =
    map (\(_k, v) -> map fromFixity v) . Map.toDescList

  goP =
    asum
      [ parens expP
      , litP
      , ifP
        -- We have to use `try` here because, unfortunately, function
        -- application `f(x)` shares a prefix with the standalone
        -- variable lookup `f`.
      , try appP
      , varP
      ]

fromFixity :: DeltaParsing m => (Name, Op.Fixity) -> Operator m Exp
fromFixity (name, fixity) = do
  op (Name.toString name)
 where
  op =
    case fixity of
      Op.Prefix ->
        prefixOp
      Op.Infix ->
        infixOp
      Op.Infixl ->
        infixlOp
      Op.Infixr ->
        infixrOp

accessOp :: (e ~ Stdlib, MonadReader e m, DeltaParsing m) => Operator m Exp
accessOp =
  Postfix (chainl1 (idxP <|> dotP) (pure (flip (.))))
 where
  idxP = do
    ann :+ expIdx <- anned $ do
      _ <- symbol "["
      expIdx <- expP
      _ <- symbol "]"
      pure expIdx
    pure (\exp -> ann :< Idx exp expIdx)
  dotP = do
    ann :+ key <- anned $ do
      _ <- symbol "."
      nameP
    pure (\exp -> ann :< Key exp key)

infixOp :: DeltaParsing m => String -> Operator m Exp
infixOp name =
  binaryOp name AssocNone

infixlOp :: DeltaParsing m => String -> Operator m Exp
infixlOp name =
  binaryOp name AssocLeft

infixrOp :: DeltaParsing m => String -> Operator m Exp
infixrOp name =
  binaryOp name AssocRight

binaryOp :: DeltaParsing m => String -> Assoc -> Operator m Exp
binaryOp name =
  Infix
    (do ann <- anning (reserve emptyOps name)
        pure (\a b -> appE ann (fromString name) (fromList [a, b])))

prefixOp :: DeltaParsing m => String -> Operator m Exp
prefixOp name =
  Prefix
    (do ann <- anning (reserve emptyOps name)
        pure (\a -> appE ann (fromString name) (fromList [a])))

litP :: (e ~ Stdlib, MonadReader e m, DeltaParsing m) => m Exp
litP = do
  ann :+ lit <- anned . runUnspaced $ asum
    [ nullP
    , boolP
    , numberP
    , stringP
    , regexpP
    , arrayP
    , recordP
    ]
  _ <- spaces
  pure (litE ann lit)
 where
  nullP =
    Null <$ symbol "null"
  boolP =
    asum
      [ Bool False <$ symbol "false"
      , Bool True <$ symbol "true"
      ]
  numberP =
    map (either (Int . fromIntegral) Double) integerOrDouble
  stringP =
    map String stringLiteral
  arrayP =
    map (Array . Vector.fromList)
      (brackets (Unspaced (sepBy expP (symbol ","))))
  recordP =
    map (Record . HashMap.fromList)
      (braces (Unspaced (sepBy kv (symbol ","))))
   where
    kv = do
      k <- map fromString (some letter)
      _ <- symbol ":"
      v <- expP
      pure (k, v)

regexpP :: (Monad m, TokenParsing m) => m Literal
regexpP = do
  str <- between (char '/') (char '/') (normalChar [])
  modifiers <- many modifier
  whiteSpace
  either unexpected (pure . Regexp) (Pcre.compileM (Text.encodeUtf8 str) (Pcre.utf8 : modifiers))
 where
  normalChar acc = do
    c <- noneOf ['/']
    case c of
      '\\' -> excapedChar acc
      _ -> normalChar (c : acc)
   <|>
    pure (fromString (reverse acc))
  excapedChar acc = do
    c <- anyChar
    case c of
      '/' ->
        normalChar ('/' : acc)
      _ ->
        normalChar (c : '\\' : acc)
  modifier = do
    _ <- char 'i'
    pure Pcre.caseless

ifP :: (e ~ Stdlib, MonadReader e m, DeltaParsing m) => m Exp
ifP = do
  ann :+ (p, t, f) <- anned $ do
    _ <- symbol "if"
    p <- expP
    _ <- symbol "then"
    t <- expP
    _ <- symbol "else"
    f <- expP
    pure (p, t, f)
  pure (ifE ann p t f)

appP :: (e ~ Stdlib, MonadReader e m, DeltaParsing m) => m Exp
appP = do
  ann :+ (name, args) <- anned $ do
    name <- nameP
    args <-
      between (symbol "(" *> spaces) (spaces *> symbol ")") (sepByNonEmpty expP (symbol ","))
    pure (name, args)
  pure (appE ann name args)

varP :: DeltaParsing m => m Exp
varP =
  map varE nameP

nameP :: DeltaParsing m => m (Ann :+ Name)
nameP =
  anned (map fromString (liftA2 (:) firstL (many restL))) <* spaces
 where
  firstL =
    letter <|> char '_'
  restL =
    letter <|> digit <|> oneOf "_-?!><"

parseRaw :: (Monad m, CharParsing m, LookAheadParsing m) => m Tmpl
parseRaw =
  map (Tmpl.Raw . fromString . reverse) (go [])
 where
  go acc =
    asum
      [ do _ <- eof
           pure acc
           -- Attempt to fish for a line block.
      , do _ <- lookAhead (try (spacesExceptNewline *> string "{%" *> manyTill anyChar (try (string "%}")) *> spacesExceptNewline *> newline))
           pure acc
           -- Attempt to fish for a inline block.
      , do _ <- lookAhead (string "{{" <|> string "{%")
           pure acc
      , do x <- anyChar
           go (x : acc)
      ]

-- | Line blocks "remove" all the whitespace around the block
-- including the newline; inline blocks do not touch it.
--
-- this is a line block: ^{% set foo = 4 %}\n$
-- this is an inline block: ^foo{% set foo = 4 %}bar$
blockP :: CharParsing m => String -> m a -> m a
blockP name p =
  try lineP <|> inlineP
 where
  -- the weird `"{%" <> " " <> name` construction avoids *a lot* of `try`ing,
  -- because all statements start with `{%`
  lineP =
    between
      (spaces *> string (beginWith <> " " <> name) *> spaces)
      (spaces <* string endWith <* spacesExceptNewline <* newline)
      p
  inlineP =
    between (string (beginWith <> " " <> name) *> spaces) (spaces <* string endWith) p
  (beginWith, endWith) =
    ("{%", "%}")

-- parse name-only blocks such as {% endlet %}
blockP_ :: CharParsing m => String -> m ()
blockP_ name =
  blockP name (pure ())

commentP :: CharParsing m => m Text
commentP =
  try lineP <|> inlineP
 where
  lineP = do
    -- unfortunately, this `between` isn't /symmetrical/, because
    -- `p` eats the ending "#}"
    between
      (spaces *> string beginWith *> spaces)
      (spacesExceptNewline <* newline)
      p
  inlineP =
    string beginWith *> spaces *> p
  (beginWith, endWith) =
    ("{#", "#}")
  p =
    map fromString (manyTill anyChar (try (spaces *> string endWith)))

spacesExceptNewline :: CharParsing m => m String
spacesExceptNewline =
  many spaceExceptNewline

spaceExceptNewline :: CharParsing m => m Char
spaceExceptNewline =
  satisfy (\c -> Char.isSpace c && (c /= '\n'))
