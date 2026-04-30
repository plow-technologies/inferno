{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}

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

import Control.Monad.Combinators.Expr (Operator)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Writer (WriterT)
import Data.Data (Data)
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Monoid (Endo)
import Data.Text (Text)
import qualified Data.Text as Text
import Inferno.Parse.Error (prettyError)
import Inferno.Types.Syntax
  ( Comment,
    CustomType,
    Expr,
    ExtIdent,
    Fixity,
    Ident (Ident),
    Import,
    Lit,
    ModuleName (ModuleName),
    Pat,
    RestOfRecord,
    Scoped,
    SigVar,
    TList,
    rws,
  )
import Inferno.Types.Type
  ( InfernoType,
    TCScheme,
    TypeClass,
  )
import Text.Megaparsec
  ( ParseError,
    Parsec,
    ShowErrorComponent (showErrorComponent),
    SourcePos,
  )

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

type Parser = ReaderT (OpsTable, Map.Map ModuleName OpsTable, [CustomType]) (WriterT Comments (Parsec InfernoParsingError Text))

type SomeParser r = ReaderT r (WriterT Comments (Parsec InfernoParsingError Text))

type Comments = Endo [Comment SourcePos]

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

type TyParser =
  ReaderT
    ( Map.Map Text Int
    , OpsTable
    , Map.Map ModuleName OpsTable
    , [CustomType]
    )
    (WriterT Comments (Parsec InfernoParsingError Text))

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a, b, c) = f a b c

choiceOf :: (t -> Parser a) -> [t] -> Parser a
choiceOf = undefined

tryMany :: (t -> Parser a) -> [t] -> Parser a
tryMany = undefined

output :: Comment SourcePos -> SomeParser r ()
output = undefined

skipLineComment :: SomeParser r ()
skipLineComment = undefined

skipBlockComment :: SomeParser r ()
skipBlockComment = undefined

sc :: SomeParser r ()
sc = undefined

lexeme :: SomeParser r a -> SomeParser r a
lexeme = undefined

symbol :: Text -> SomeParser r Text
symbol = undefined

-- | 'parens' parses something between parenthesis.
parens :: SomeParser r a -> SomeParser r a
parens p = undefined

-- | 'rword' for parsing reserved words.
rword :: Text -> SomeParser r ()
rword = undefined

isAlphaNumOrSeparator :: Char -> Bool
isAlphaNumOrSeparator = undefined

alphaNumCharOrSeparator :: SomeParser r Char
alphaNumCharOrSeparator = undefined

variable :: Parser Text
variable = undefined

mIdent :: Parser (SourcePos, Maybe Ident)
mIdent = undefined

mExtIdent :: Parser (SourcePos, Maybe ExtIdent)
mExtIdent = undefined

implicitVariable :: Parser Text
implicitVariable = undefined

enumConstructor :: SomeParser r Ident
enumConstructor = undefined

-- | 'signedInteger' parses an integer with an optional sign (with no space)
signedInteger :: (Num a) => Parser a
signedInteger = undefined

-- | 'signedInteger' parses a float/double with an optional sign (with no space)
signedFloat :: Parser Double
signedFloat = undefined

enumE :: (SourcePos -> () -> Scoped ModuleName -> Ident -> f) -> Parser f
enumE f = undefined

implVarE :: Parser (Expr () SourcePos)
implVarE = undefined

-- | 'intE' and 'doubleE' parse unsigned numbers
intE, doubleE :: Parser (Expr () SourcePos)
intE = undefined
doubleE = undefined

hexadecimal :: (SourcePos -> Lit -> f SourcePos) -> Parser (f SourcePos)
hexadecimal f = undefined

signedIntE, signedDoubleE :: (SourcePos -> Lit -> f SourcePos) -> Parser (f SourcePos)
signedIntE f = undefined
signedDoubleE f = undefined

noneE :: (SourcePos -> a) -> Parser a
noneE = undefined

someE :: (SourcePos -> t -> a) -> Parser t -> Parser a
someE = undefined

stringE :: (SourcePos -> Lit -> f SourcePos) -> Parser (f SourcePos)
stringE f = undefined

interpolatedStringE :: Parser (Expr () SourcePos)
interpolatedStringE = undefined

arrayComprE :: Parser (Expr () SourcePos)
arrayComprE = undefined

array :: Parser a -> Parser (SourcePos, [(a, Maybe SourcePos)], SourcePos)
array p = undefined

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

tupleArgs :: SomeParser r a -> SomeParser r [(a, Maybe SourcePos)]
tupleArgs = undefined

isHSpace :: Char -> Bool
isHSpace = undefined

tuple :: SomeParser r a -> SomeParser r (SourcePos, TList (a, Maybe SourcePos), SourcePos)
tuple = undefined

assertE :: Parser (Expr () SourcePos)
assertE = undefined

ifE :: Parser (Expr () SourcePos)
ifE = undefined

-- | Parses an op in prefix syntax WITHOUT opening paren @(@ but with closing paren @)@
-- E.g. @+)@
prefixOpsWithoutModule :: SourcePos -> Parser (Expr () SourcePos)
prefixOpsWithoutModule = undefined

-- | Parses a op in prefix syntax of the form @Mod.(+)@
prefixOpsWithModule :: SourcePos -> Parser (Expr () SourcePos)
prefixOpsWithModule = undefined

-- | Parses a tuple @a, b, c, ...)@ WITHOUT opening paren @(@ but with closing paren @)@
-- Returns (list of (expr, commaPos), endParenPos)
tupleElems :: Parser ([(Expr () SourcePos, Maybe SourcePos)], SourcePos)
tupleElems = undefined

-- | Parses any bracketed expression: tuples, bracketed exprs (1 + 2), and prefix ops (+)
bracketedE :: Parser (Expr () SourcePos)
bracketedE = undefined

term :: Parser (Expr () SourcePos)
term = undefined

app :: Parser (Expr () SourcePos)
app = undefined

expr :: Parser (Expr () SourcePos)
expr = undefined

mkOperators :: OpsTable -> [[Operator Parser (Expr () SourcePos)]]
mkOperators = undefined

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
parseType s = undefined

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
insertIntoOpsTable = undefined

modulesParser :: Parser [(ModuleName, OpsTable, [TopLevelDefn (Maybe TCScheme, QQDefinition)])]
modulesParser = undefined

topLevel :: SomeParser r a -> SomeParser r a
topLevel p = undefined
