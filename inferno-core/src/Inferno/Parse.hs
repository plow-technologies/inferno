{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Inferno.Parse
  ( Comment (..),
    OpsTable,
    TopLevelDefn (..),
    QQDefinition (..),
    InfernoParsingError (..),
    topLevel,
    expr,
    parseExpr,
    parseType,
    parseTCScheme,
    modulesParser,
    prettyError,
    rws,
  )
where

import Control.Applicative (asum, (<|>))
import Control.Arrow ((&&&))
import Control.Monad (join, void)
import Control.Monad.Combinators.Expr
  ( Operator (InfixL, InfixN, InfixR, Prefix),
    makeExprParser,
  )
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.State.Strict (StateT, modify')
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isSpace)
import Data.Data (Data)
import Data.Foldable (foldl') -- foldl': DO NOT REMOVE; needed for older GHC compat
import Data.Functor (($>), (<&>))
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Inferno.Parse.Error (prettyError)
import Inferno.Types.Syntax
  ( Comment (BlockComment, LineComment),
    CustomType,
    Expr
      ( App,
        Array,
        Bracketed,
        Empty,
        Enum,
        Lit,
        One,
        Op,
        OpVar,
        PreOp,
        Record,
        Tuple,
        Var
      ),
    ExtIdent (ExtIdent),
    Fixity (InfixOp, PrefixOp),
    Ident (Ident),
    ImplExpl (Expl, Impl),
    Import,
    InfixFixity (LeftFix, NoFix, RightFix),
    Lit (LDouble, LHex, LInt, LText),
    ModuleName (ModuleName),
    Pat,
    RestOfRecord,
    Scoped (LocalScope, Scope),
    SigVar,
    TList (TNil),
    fromScoped,
    rws,
    tListFromList,
    unModuleName,
  )
import Inferno.Types.Type
  ( InfernoType,
    TCScheme,
    TypeClass,
  )
import Text.Megaparsec
  ( MonadParsec (hidden, label, notFollowedBy, takeWhile1P, takeWhileP, try),
    ParseError,
    Parsec,
    ShowErrorComponent (showErrorComponent),
    SourcePos,
    anySingle,
    between,
    getSourcePos,
    manyTill,
    satisfy,
    some,
    sourceColumn,
    unPos,
    (<?>),
  )
import Text.Megaparsec.Char (char, char', letterChar, spaceChar, string)
import qualified Text.Megaparsec.Char.Lexer as Lexer

data InfernoParsingError
  = ModuleNotFound ModuleName
  | InfixOpNotFound ModuleName Ident
  | UnboundTyVar Text
  | ImplicitVarTypeAnnot
  deriving (Eq, Show, Ord)

instance ShowErrorComponent InfernoParsingError where
  showErrorComponent (ModuleNotFound (ModuleName modNm)) =
    "Module '" <> Text.unpack modNm <> "' could not be found"
  showErrorComponent (InfixOpNotFound (ModuleName modNm) (Ident op)) =
    "Module " <> Text.unpack modNm <> " does not export `(" <> Text.unpack op <> ")`"
  showErrorComponent (UnboundTyVar ty) =
    "Unbound type variable '" <> Text.unpack ty
  showErrorComponent ImplicitVarTypeAnnot =
    "Implicit variables cannot have type annotations"

type BaseParsec = Parsec InfernoParsingError Text

type ParserOver r = ReaderT r (StateT [Comment SourcePos] BaseParsec)

type Parser = ParserOver ParseEnv

data TopLevelDefn def
  = -- FIXME Don't use record fields in sum types, damn it!
    Signature
      { documentation :: Maybe Text
      , name :: SigVar
      , def :: def
      }
  | EnumDef (Maybe Text) Text [Ident]
  | TypeClassInstance TypeClass
  | Export ModuleName
  deriving (Eq, Show, Data)

type OpsTable = IntMap.IntMap [(Fixity, Scoped ModuleName, Text)]

data QQDefinition
  = QQRawDef String
  | QQToValueDef String
  | InlineDef (Expr () SourcePos)
  deriving (Data)

type TyParser = ParserOver TyParseEnv

-- | Operator table entry.
data OpEntry = OpEntry
  { fixity :: !Fixity
  , scope :: !(Scoped ModuleName)
  , name :: !Text
  }

-- | A bracketed, separator-delimited sequence with positional metadata.
data Delimited a = Delimited
  { open :: !SourcePos
  , items :: [Separated a]
  , close :: !SourcePos
  }

-- | An element followed by an optional separator position.
data Separated a = Separated
  { val :: a
  , sep :: !(Maybe SourcePos)
  }

-- | A record field binding.
data Field a = Field
  { name :: !Ident
  , val :: a
  }

-- | Parser environment; precomputed data derived from the operator table.
data ParseEnv = ParseEnv
  { ops :: !OpsTable
  , modOps :: !(Map ModuleName OpsTable)
  , customTypes :: ![CustomType]
  , reserved :: !(Set Text)
  , localOps :: ![Text]
  , qualOps :: ![(Scoped ModuleName, Text)]
  , infixOps :: ![Text]
  , prefixOps :: ![Text]
  , -- NOTE: This is *intentionally lazy*, used for recursive knot-tying. If
    -- @StrictData@ is ever enabled for this module, please use @~@ explicitly
    exprP :: Parser (Expr () SourcePos)
  }

-- | Type parser environment.
data TyParseEnv = TyParseEnv
  { tyVars :: !(Map Text Int)
  , ops :: !OpsTable
  , modOps :: !(Map ModuleName OpsTable)
  , ctypes :: ![CustomType]
  }

fromOpTuple :: (Fixity, Scoped ModuleName, Text) -> OpEntry
fromOpTuple (f, s, n) = OpEntry{fixity = f, scope = s, name = n}

toOpTuple :: OpEntry -> (Fixity, Scoped ModuleName, Text)
toOpTuple e = (e.fixity, e.scope, e.name)

mkParseEnv :: OpsTable -> Map ModuleName OpsTable -> [CustomType] -> ParseEnv
mkParseEnv ops modOps cts = env
  where
    env :: ParseEnv
    env =
      ParseEnv
        { ops
        , modOps
        , customTypes = cts
        , reserved = Set.fromList $ rws <> fmap (.name) allOps
        , localOps = fmap (.name) . filter ((== LocalScope) . (.scope)) $ allOps
        , qualOps = fmap ((.scope) &&& (.name)) . filter ((/= LocalScope) . (.scope)) $ allOps
        , infixOps = mapMaybe infixName allOps
        , prefixOps = mapMaybe prefixName allOps
        , exprP = makeExprParser app $ mkOperators ops
        }

    allOps :: [OpEntry]
    allOps = fmap fromOpTuple . concat . IntMap.elems $ ops

    infixName :: OpEntry -> Maybe Text
    infixName e = case e.fixity of
      InfixOp _ -> Just e.name
      _ -> Nothing

    prefixName :: OpEntry -> Maybe Text
    prefixName e = case e.fixity of
      PrefixOp -> Just e.name
      _ -> Nothing

-- | Converts a curried function to a function on a triple.
--
-- NOTE: Strict, unlike the version from @Data.Tuple.Extra@
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

choiceOf :: (t -> Parser a) -> [t] -> Parser a
choiceOf = \cases
  _ [] -> fail "none of the operators matched"
  p [x] -> p x
  p (x : xs) -> p x <|> choiceOf p xs

tryMany :: (t -> Parser a) -> [t] -> Parser a
tryMany = \cases
  _ [] -> fail "none of the operators matched"
  p [x] -> p x
  p (x : xs) -> try (p x) <|> tryMany p xs

output :: Comment SourcePos -> ParserOver r ()
output c = modify' (c :)

skipLineComment :: ParserOver r ()
skipLineComment = do
  startPos <- getSourcePos
  comment <- string "//" *> takeWhileP (Just "character") (/= '\n')
  endPos <- getSourcePos
  output $ LineComment startPos (Text.strip comment) endPos

skipBlockComment :: ParserOver r ()
skipBlockComment = do
  startPos <- getSourcePos
  comment <- string "/*" *> manyTill anySingle (string "*/")
  endPos <- getSourcePos
  output $ BlockComment startPos (stripIndent startPos comment) endPos
  where
    stripIndent :: SourcePos -> [Char] -> Text
    stripIndent pos = Text.replace ws "\n" . Text.pack
      where
        ws :: Text
        ws = Text.pack $ '\n' : replicate (unPos pos.sourceColumn - 1) ' '

sc :: ParserOver r ()
sc = Lexer.space (void spaceChar) skipLineComment skipBlockComment

lexeme :: ParserOver r a -> ParserOver r a
lexeme = Lexer.lexeme sc

symbol :: Text -> ParserOver r Text
symbol = Lexer.symbol sc

-- | 'parens' parses something between parenthesis.
parens :: ParserOver r a -> ParserOver r a
parens = hidden . between (symbol "(") (symbol ")")

-- | 'rword' for parsing reserved words.
rword :: Text -> ParserOver r ()
rword w = string w *> notFollowedBy alphaNumCharOrSeparator *> sc

isAlphaNumOrSeparator :: Char -> Bool
isAlphaNumOrSeparator = (||) <$> isAlphaNum <*> (== '_')

alphaNumCharOrSeparator :: ParserOver r Char
alphaNumCharOrSeparator =
  satisfy isAlphaNumOrSeparator <?> "alphanumeric character or '_'"

withSourcePos :: (SourcePos -> ParserOver r a) -> ParserOver r a
withSourcePos = (getSourcePos >>=)

variable :: Parser Text
variable =
  -- Checks the set of known keywords, making lookup O(1) instead of the previous
  -- O(n) linear scan every time
  asks (.reserved) >>= try . (p >>=) . check
  where
    p :: Parser Text
    p =
      label "a variable" $
        Text.cons
          <$> letterChar
          <*> hidden (takeWhileP Nothing isAlphaNumOrSeparator)

    check :: Set Text -> Text -> Parser Text
    check res x
      | Set.member x res =
          fail $
            unwords
              [ "Keyword"
              , show x
              , "cannot be a variable/function name"
              ]
      | otherwise = pure x

mIdent :: Parser (SourcePos, Maybe Ident)
mIdent = lexeme . withSourcePos $ \pos ->
  label "a wildcard parameter '_'" $
    asum
      [ (pos,) . Just . Ident <$> variable
      , char '_' *> takeWhileP Nothing isAlphaNumOrSeparator $> (pos, Nothing)
      ]

mExtIdent :: Parser (SourcePos, Maybe ExtIdent)
mExtIdent = lexeme . withSourcePos $ \pos ->
  label "a wildcard parameter '_'" $
    asum
      [ (pos,) . Just . ExtIdent . Right <$> variable
      , char '_' *> takeWhileP Nothing isAlphaNumOrSeparator $> (pos, Nothing)
      ]

implicitVariable :: Parser Text
implicitVariable = hidden $ char '?' *> t
  where
    t :: Parser Text
    t = Text.cons <$> letterChar <*> takeWhileP Nothing isAlphaNumOrSeparator

enumConstructor :: ParserOver r Ident
enumConstructor =
  Ident <$> lexeme (char '#' *> takeWhile1P Nothing isAlphaNumOrSeparator)
    <?> "an enum constructor\nfor example: #true, #false"

-- | Parses an integer with an optional sign (with no space).
signedInteger :: (Num a) => Parser a
signedInteger =
  flip Lexer.signed Lexer.decimal $
    takeWhileP Nothing isHSpace $> ()

-- | Parses a float/double with an optional sign (with no space).
signedFloat :: Parser Double
signedFloat =
  flip Lexer.signed Lexer.float $
    takeWhileP Nothing isHSpace $> ()

enumE :: (SourcePos -> () -> Scoped ModuleName -> Ident -> f) -> Parser f
enumE f = lexeme . withSourcePos $ \pos ->
  asum
    [ try $ f pos () . Scope . ModuleName <$> variable <*> (char '.' *> enumConstructor)
    , f pos () LocalScope <$> enumConstructor
    ]

implVarE :: Parser (Expr () SourcePos)
implVarE = lexeme . withSourcePos $ \pos ->
  Var pos () LocalScope . Impl . ExtIdent . Right <$> implicitVariable

-- | Parses unsigned integer and double literals.
intE, doubleE :: Parser (Expr () SourcePos)
intE =
  label "a number\nfor example: 42, 3.1415, (-6)" . lexeme . withSourcePos $
    \pos -> Lit pos . LInt <$> Lexer.decimal
doubleE =
  label "a number\nfor example: 42, 3.1415, (-6)" . lexeme . withSourcePos $
    \pos -> Lit pos . LDouble <$> Lexer.float

hexadecimal :: (SourcePos -> Lit -> f SourcePos) -> Parser (f SourcePos)
hexadecimal f =
  label "a hexadecimal number\nfor example: 0xE907, 0XE907" . lexeme . withSourcePos $
    \pos -> f pos . LHex <$> (char '0' *> char' 'x' *> Lexer.hexadecimal)

signedIntE, signedDoubleE :: (SourcePos -> Lit -> f SourcePos) -> Parser (f SourcePos)
signedIntE f =
  label "a number\nfor example: 42, 3.1415, (-6)" . lexeme . withSourcePos $
    \pos -> f pos . LInt <$> signedInteger
signedDoubleE f =
  label "a number\nfor example: 42, 3.1415, (-6)" . lexeme . withSourcePos $
    \pos -> f pos . LDouble <$> signedFloat

noneE :: (SourcePos -> a) -> Parser a
noneE e =
  label "an optional\nfor example: Some x, None" . lexeme . withSourcePos $
    \pos -> e pos <$ hidden (string "None")

someE :: (SourcePos -> t -> a) -> Parser t -> Parser a
someE f p =
  label "an optional\nfor example: Some x, None" . withSourcePos $
    \pos -> lexeme (hidden $ string "Some") *> fmap (f pos) p

stringE :: (SourcePos -> Lit -> f SourcePos) -> Parser (f SourcePos)
stringE f =
  label "a string\nfor example: \"hello world\"" . lexeme . withSourcePos $
    \pos -> f pos . LText . Text.pack <$> (char '"' *> manyTill charNoNewline (char '"'))
  where
    charNoNewline :: Parser Char
    charNoNewline = notFollowedBy (char '\n') *> Lexer.charLiteral

interpolatedStringE :: Parser (Expr () SourcePos)
interpolatedStringE = undefined

arrayComprE :: Parser (Expr () SourcePos)
arrayComprE = undefined

array :: Parser a -> Parser (SourcePos, [(a, Maybe SourcePos)], SourcePos)
array = undefined

record :: Parser a -> Parser (SourcePos, [(Ident, a, Maybe SourcePos)], SourcePos)
record = undefined

mkInterpolatedString :: [Either Text e] -> [Either Text e]
mkInterpolatedString = undefined

funE :: Parser (Expr () SourcePos)
funE = undefined

renameModE :: Parser (Expr () SourcePos)
renameModE = undefined

openModArgs :: ModuleName -> Parser ([(Import SourcePos, Maybe SourcePos)], SourcePos, Expr () SourcePos)
openModArgs = undefined

openModE :: Parser (Expr () SourcePos)
openModE = undefined

letE :: Parser (Expr () SourcePos)
letE = undefined

pat :: Parser (Pat () SourcePos)
pat = undefined

casePatts :: Parser [(SourcePos, Pat () SourcePos, SourcePos, Expr () SourcePos)]
casePatts = undefined

caseE :: Parser (Expr () SourcePos)
caseE = undefined

tupleArgs :: ParserOver r a -> ParserOver r [(a, Maybe SourcePos)]
tupleArgs = undefined

isHSpace :: Char -> Bool
isHSpace c = isSpace c && c /= '\n' && c /= '\r'

tuple :: ParserOver r a -> ParserOver r (SourcePos, TList (a, Maybe SourcePos), SourcePos)
tuple = undefined

assertE :: Parser (Expr () SourcePos)
assertE = undefined

ifE :: Parser (Expr () SourcePos)
ifE = undefined

-- | Parses an op in prefix syntax WITHOUT opening paren @(@ but with closing paren @)@
-- E.g. @+)@
prefixOpsWithoutModule :: SourcePos -> Parser (Expr () SourcePos)
prefixOpsWithoutModule pos = hidden $ asks (.localOps) >>= choiceOf prefixOp
  where
    prefixOp :: Text -> Parser (Expr () SourcePos)
    prefixOp s = symbol (s <> ")") $> OpVar pos () LocalScope (Ident s)

-- | Parses an op in prefix syntax of the form @Mod.(+)@
prefixOpsWithModule :: SourcePos -> Parser (Expr () SourcePos)
prefixOpsWithModule pos = hidden $ asks (.qualOps) >>= tryMany prefixOp
  where
    prefixOp :: (Scoped ModuleName, Text) -> Parser (Expr () SourcePos)
    prefixOp (ns, s) = symbol t $> OpVar pos () ns (Ident s)
      where
        t :: Text
        t =
          mconcat
            [ fromScoped mempty $ (<> ".") . unModuleName <$> ns
            , "("
            , s
            , ")"
            ]

-- | Parses @a, b, c, ...)@ WITHOUT the opening paren but WITH the closing paren.
tupleElems :: Parser ([(Expr () SourcePos, Maybe SourcePos)], SourcePos)
tupleElems =
  asum
    [ expr >>= \e ->
        asum
          [ char ')' *> withSourcePos (pure . ([(e, Nothing)],))
          , lexeme (char ',' *> getSourcePos) >>= \commaPos ->
              fmap (first ((e, Just commaPos) :)) tupleElems
          ]
    , char ')' *> withSourcePos (pure . ([],))
    ]

-- | Parses any bracketed expression: tuples, bracketed exprs, and prefix ops.
bracketedE :: Parser (Expr () SourcePos)
bracketedE = withSourcePos $ \pos ->
  symbol "(" *> (prefixOpsWithoutModule pos <|> bracketedOrTuple pos)
  where
    bracketedOrTuple :: SourcePos -> Parser (Expr () SourcePos)
    bracketedOrTuple pos =
      tupleElems >>= \(es, endPos) ->
        lexeme . pure $ case es of
          [] -> Tuple pos TNil endPos
          [(e, _)] -> Bracketed pos e endPos
          _ -> Tuple pos (tListFromList es) endPos

term :: Parser (Expr () SourcePos)
term =
  asum
    [ bracketedE
    , try $ hexadecimal Lit
    , try doubleE
    , intE
    , enumE Enum
    , withSourcePos $ \pos ->
        lexeme $
          asum
            [ try $ qualVar pos <$> variable <*> (char '.' *> variable)
            , prefixOpsWithModule pos
            , try $
                Var pos () LocalScope . Expl . ExtIdent . Right
                  <$> variable
                  <* notFollowedBy (char '.')
            ]
    , noneE Empty
    , someE One expr
    , ifE
    , try renameModE
    , letE
    , openModE
    , assertE
    , funE
    , caseE
    , try implVarE
    , stringE Lit
    , interpolatedStringE
    , try $ uncurry3 Array <$> array expr
    , try $ uncurry3 Record <$> record expr
    , arrayComprE
    ]
  where
    qualVar :: SourcePos -> Text -> Text -> Expr () SourcePos
    qualVar pos ns x = Var pos () (Scope $ ModuleName ns) . Expl . ExtIdent $ Right x

app :: Parser (Expr () SourcePos)
app =
  term >>= \x ->
    asum
      [ foldl' App x <$> some term
      , pure x
      ]

expr :: Parser (Expr () SourcePos)
expr = join $ asks (.exprP)

mkOperators :: OpsTable -> [[Operator Parser (Expr () SourcePos)]]
mkOperators tbl =
  IntMap.toDescList tbl <&> \(prec, grp) -> fmap (uncurry3 (mkOp prec)) grp
  where
    opStr :: Scoped ModuleName -> Text -> Text
    opStr = \cases
      LocalScope s -> s
      (Scope (ModuleName ns)) s -> ns <> "." <> s

    fixCtor :: InfixFixity -> Parser (a -> a -> a) -> Operator Parser a
    fixCtor = \cases
      NoFix -> InfixN
      LeftFix -> InfixL
      RightFix -> InfixR

    mkOp ::
      Int ->
      Fixity ->
      Scoped ModuleName ->
      Text ->
      Operator Parser (Expr () SourcePos)
    mkOp = \cases
      prec (InfixOp fix) ns o ->
        fixCtor fix . label "an infix operator\nfor example: +, *, ==, >, <" $
          opP ns o >>= \pos -> pure $ \e1 e2 ->
            Op e1 pos () (prec, fix) ns (Ident o) e2
      prec PrefixOp ns o ->
        Prefix . label "a prefix operator\nfor example: -, !" $
          opP ns o >>= \pos -> pure $ PreOp pos () prec ns (Ident o)

    opP :: Scoped ModuleName -> Text -> Parser SourcePos
    opP ns o = lexeme $ getSourcePos <* string (opStr ns o)

parseExpr ::
  OpsTable ->
  Map.Map ModuleName OpsTable ->
  [CustomType] ->
  Text ->
  Either
    (NonEmpty (ParseError Text InfernoParsingError, SourcePos))
    (Expr () SourcePos, [Comment SourcePos])
parseExpr = undefined

-- Parsing types

rwsType :: [Text] -- list of reserved type sig words
rwsType = undefined

typeIdent :: TyParser Text
typeIdent = undefined

baseType :: TyParser InfernoType
baseType = undefined

typeVariableRaw :: TyParser Text
typeVariableRaw = undefined

typeVariable :: TyParser Int
typeVariable = undefined

recordType :: TyParser (Map.Map Ident InfernoType, RestOfRecord)
recordType = undefined

typeParserBase :: TyParser InfernoType
typeParserBase = undefined

typeParser :: TyParser InfernoType
typeParser = undefined

parseType ::
  Text ->
  Either
    (NonEmpty (ParseError Text InfernoParsingError, SourcePos))
    InfernoType
parseType = undefined

parseTCScheme :: Text -> Either String TCScheme
parseTCScheme = undefined

listParser :: TyParser a -> TyParser [a]
listParser = undefined

tyContext :: TyParser [Either TypeClass (Text, InfernoType)]
tyContext = undefined

typeClass :: TyParser TypeClass
typeClass = undefined

tyContextSingle :: TyParser (Either TypeClass (Text, InfernoType))
tyContextSingle = undefined

schemeParser :: TyParser TCScheme
schemeParser = undefined

doc :: Parser Text
doc = undefined

enumConstructors :: Parser [Ident]
enumConstructors = undefined

sigVariable :: Parser SigVar
sigVariable = undefined

exprOrBuiltin :: Parser QQDefinition
exprOrBuiltin = undefined

sigParser :: Parser (TopLevelDefn (Maybe TCScheme, QQDefinition))
sigParser = undefined

fixityP :: Parser Fixity
fixityP = undefined

fixityLvl :: Parser Int
fixityLvl = undefined

sigsParser :: Parser (OpsTable, [TopLevelDefn (Maybe TCScheme, QQDefinition)])
sigsParser = undefined

insertIntoOpsTable :: OpsTable -> Fixity -> Int -> Text -> OpsTable
insertIntoOpsTable tbl f lvl op = IntMap.alter go lvl tbl
  where
    go ::
      Maybe [(Fixity, Scoped ModuleName, Text)] ->
      Maybe [(Fixity, Scoped ModuleName, Text)]
    go = \cases
      Nothing -> Just [(f, LocalScope, op)]
      (Just xs) -> Just $ (f, LocalScope, op) : xs

modulesParser ::
  Parser [(ModuleName, OpsTable, [TopLevelDefn (Maybe TCScheme, QQDefinition)])]
modulesParser = undefined

topLevel :: ParserOver r a -> ParserOver r a
topLevel = undefined
