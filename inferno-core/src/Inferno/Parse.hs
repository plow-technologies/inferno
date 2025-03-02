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

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (foldM, void, when)
import Control.Monad.Combinators.Expr
  ( Operator (InfixL, InfixN, InfixR, Prefix),
    makeExprParser,
  )
import Control.Monad.Reader (ReaderT (..), ask, withReaderT)
import Control.Monad.Writer (WriterT (..), tell)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isAlphaNum, isSpace)
import Data.Data (Data)
import Data.Either (partitionEithers)
import Data.Functor (($>))
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEList
import qualified Data.Map as Map
import Data.Monoid (Endo (..))
import qualified Data.Set as Set
import Data.Text (Text, pack, singleton, unpack)
import qualified Data.Text as Text
import Inferno.Infer.Env (closeOver)
import Inferno.Parse.Error (prettyError)
import Inferno.Types.Syntax
  ( Comment (..),
    CustomType (),
    Expr (..),
    ExtIdent (ExtIdent),
    Fixity (..),
    Ident (Ident),
    ImplExpl (Expl, Impl),
    Import (..),
    InfixFixity (..),
    Lit (..),
    ModuleName (..),
    Pat (..),
    RestOfRecord (..),
    Scoped (..),
    SigVar (..),
    TList (..),
    fromEitherList,
    fromScoped,
    rws,
    tListFromList,
  )
import Inferno.Types.Type
  ( BaseType (..),
    ImplType (..),
    InfernoType (..),
    TCScheme (..),
    TV (TV),
    TypeClass (TypeClass),
  )
import Inferno.Utils.Prettyprinter (renderPretty)
import Text.Megaparsec
  ( MonadParsec
      ( eof,
        hidden,
        label,
        notFollowedBy,
        takeWhile1P,
        takeWhileP,
        try
      ),
    ParseError,
    ParseErrorBundle (ParseErrorBundle),
    Parsec,
    ShowErrorComponent (..),
    SourcePos (..),
    anySingle,
    attachSourcePos,
    between,
    choice,
    customFailure,
    errorOffset,
    getSourcePos,
    many,
    manyTill,
    optional,
    runParser,
    satisfy,
    sepBy1,
    some,
    unPos,
    (<?>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    char',
    letterChar,
    spaceChar,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as Lexer

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a, b, c) = f a b c

choiceOf :: (t -> Parser a) -> [t] -> Parser a
choiceOf _ [] = fail "none of the operators matched"
choiceOf p [x] = p x
choiceOf p (x : xs) = p x <|> choiceOf p xs

tryMany :: (t -> Parser a) -> [t] -> Parser a
tryMany _ [] = fail "none of the operators matched"
tryMany p [x] = p x
tryMany p (x : xs) = try (p x) <|> tryMany p xs

type Comments = Endo [Comment SourcePos]

output :: Comment SourcePos -> SomeParser r ()
output x = tell $ Endo ([x] <>)

type Parser = ReaderT (OpsTable, Map.Map ModuleName OpsTable, [CustomType]) (WriterT Comments (Parsec InfernoParsingError Text))

type SomeParser r = ReaderT r (WriterT Comments (Parsec InfernoParsingError Text))

skipLineComment :: SomeParser r ()
skipLineComment = do
  startPos <- getSourcePos
  comment <- string "//" *> takeWhileP (Just "character") (/= '\n')
  endPos <- getSourcePos
  output $ LineComment startPos (Text.strip comment) endPos

skipBlockComment :: SomeParser r ()
skipBlockComment = do
  startPos@(SourcePos _ _ col) <- getSourcePos
  comment <- string "/*" >> manyTill anySingle (string "*/")
  endPos <- getSourcePos
  output $
    BlockComment
      startPos
      (Text.replace (pack $ '\n' : List.replicate (unPos col - 1) ' ') "\n" $ Text.pack comment)
      endPos

sc :: SomeParser r ()
sc = Lexer.space (void spaceChar) skipLineComment skipBlockComment

lexeme :: SomeParser r a -> SomeParser r a
lexeme = Lexer.lexeme sc

symbol :: Text -> SomeParser r Text
symbol = Lexer.symbol sc

-- | 'parens' parses something between parenthesis.
parens :: SomeParser r a -> SomeParser r a
parens p = hidden $ between (symbol "(") (symbol ")") p

-- | 'rword' for parsing reserved words.
rword :: Text -> SomeParser r ()
rword w = string w *> notFollowedBy alphaNumCharOrSeparator *> sc

isAlphaNumOrSeparator :: Char -> Bool
isAlphaNumOrSeparator c = isAlphaNum c || c == '_'
{-# INLINE isAlphaNumOrSeparator #-}

alphaNumCharOrSeparator :: SomeParser r Char
alphaNumCharOrSeparator = satisfy isAlphaNumOrSeparator <?> "alphanumeric character or '_'"
{-# INLINE alphaNumCharOrSeparator #-}

variable :: Parser Text
variable = do
  (opsTable, _, _) <- ask
  try (p >>= check opsTable)
  where
    p = pack <$> (((:) <$> letterChar <*> hidden (many alphaNumCharOrSeparator)) <?> "a variable")
    check oT x =
      if x `elem` rws ++ map (\(_, _, i) -> i) (concat oT)
        then fail $ "Keyword " <> show x <> " cannot be a variable/function name"
        else return x

mIdent :: Parser (SourcePos, Maybe Ident)
mIdent = lexeme $ do
  startPos <- getSourcePos
  (startPos,) . Just . Ident <$> variable
    <|> (char '_' *> takeWhileP Nothing isAlphaNumOrSeparator $> (startPos, Nothing))
    <?> "a wildcard parameter '_'"

mExtIdent :: Parser (SourcePos, Maybe ExtIdent)
mExtIdent = lexeme $ do
  startPos <- getSourcePos
  (startPos,) . Just . ExtIdent . Right <$> variable
    <|> (char '_' *> takeWhileP Nothing isAlphaNumOrSeparator $> (startPos, Nothing))
    <?> "a wildcard parameter '_'"

implicitVariable :: Parser Text
implicitVariable = hidden $ char '?' *> (Text.cons <$> letterChar <*> takeWhileP Nothing isAlphaNumOrSeparator)

enumConstructor :: SomeParser r Ident
enumConstructor =
  Ident
    <$> lexeme (char '#' *> takeWhile1P Nothing isAlphaNumOrSeparator)
    <?> "an enum constructor\nfor example: #true, #false"

-- | 'signedInteger' parses an integer with an optional sign (with no space)
signedInteger :: (Num a) => Parser a
signedInteger = Lexer.signed (takeWhileP Nothing isHSpace $> ()) Lexer.decimal

-- | 'signedInteger' parses a float/double with an optional sign (with no space)
signedFloat :: Parser Double
signedFloat = Lexer.signed (takeWhileP Nothing isHSpace $> ()) Lexer.float

enumE :: (SourcePos -> () -> Scoped ModuleName -> Ident -> f) -> Parser f
enumE f = do
  startPos <- getSourcePos
  lexeme $
    try (f startPos () <$> (Scope . ModuleName <$> variable) <*> (char '.' *> enumConstructor))
      <|> f startPos () LocalScope <$> enumConstructor

implVarE :: Parser (Expr () SourcePos)
implVarE = do
  startPos <- getSourcePos
  lexeme $ Var startPos () LocalScope . Impl . ExtIdent . Right <$> implicitVariable

-- | 'intE' and 'doubleE' parse unsigned numbers
intE, doubleE :: Parser (Expr () SourcePos)
intE = label "a number\nfor example: 42, 3.1415, (-6)" $ do
  startPos <- getSourcePos
  lexeme $ Lit startPos . LInt <$> Lexer.decimal
doubleE = label "a number\nfor example: 42, 3.1415, (-6)" $ do
  startPos <- getSourcePos
  lexeme $ Lit startPos . LDouble <$> Lexer.float

hexadecimal :: (SourcePos -> Lit -> f SourcePos) -> Parser (f SourcePos)
hexadecimal f = label "a hexadecimal number\nfor example: 0xE907, 0XE907" $ do
  startPos <- getSourcePos
  lexeme $ f startPos . LHex <$> (char '0' *> char' 'x' *> Lexer.hexadecimal)

signedIntE, signedDoubleE :: (SourcePos -> Lit -> f SourcePos) -> Parser (f SourcePos)
signedIntE f = label "a number\nfor example: 42, 3.1415, (-6)" $ do
  startPos <- getSourcePos
  lexeme $ f startPos . LInt <$> signedInteger
signedDoubleE f = label "a number\nfor example: 42, 3.1415, (-6)" $ do
  startPos <- getSourcePos
  lexeme $ f startPos . LDouble <$> signedFloat

noneE :: (SourcePos -> a) -> Parser a
noneE e = label "an optional\nfor example: Some x, None" $ do
  startPos <- getSourcePos
  lexeme (e startPos <$ hidden (string "None"))

someE :: (SourcePos -> t -> a) -> Parser t -> Parser a
someE f p = label "an optional\nfor example: Some x, None" $ do
  startPos <- getSourcePos
  lexeme (hidden $ string "Some")
  f startPos <$> p

stringE :: (SourcePos -> Lit -> f SourcePos) -> Parser (f SourcePos)
stringE f = label "a string\nfor example: \"hello world\"" $ do
  startPos <- getSourcePos
  lexeme $ f startPos . LText . pack <$> (char '\"' *> manyTill charNoNewline (char '\"'))
  where
    charNoNewline = notFollowedBy (char '\n') *> Lexer.charLiteral

interpolatedStringE, arrayComprE :: Parser (Expr () SourcePos)
interpolatedStringE = label "an interpolated string\nfor example: `hello ${1 + 2}`" $
  lexeme $ do
    startPos@(SourcePos _ _ col) <- getSourcePos
    es <- mkInterpolatedString <$> (char '`' *> go)
    InterpolatedString startPos (fromEitherList $ fixSpacing (unPos col) es) <$> getSourcePos
  where
    go =
      [] <$ char '`'
        <|> try ((:) . Left . singleton <$> hidden (char '\\' *> char '\\') <*> go)
        <|> try ((:) . Left . singleton <$> hidden (char '\\' *> char '`') <*> go)
        <|> try ((:) . Left . singleton <$> hidden (char '\\' *> char '$') <*> go)
        <|> (:) . Right
          <$> hidden
            ( do
                startPos <- getSourcePos
                e <- char '$' *> char '{' *> sc *> expr <* char '}'
                endPos <- getSourcePos
                pure (startPos, e, endPos)
            )
          <*> go
        <|> (:) . Left . singleton <$> Lexer.charLiteral <*> go

    fixSpacing newlineSpaceLength =
      map (first (Text.replace (pack $ '\n' : List.replicate (newlineSpaceLength - 1) ' ') "\n"))
arrayComprE = label "array builder\nfor example: [n * 2 + 1 | n <- range 0 10, if n % 2 == 0]" $
  lexeme $ do
    startPos <- getSourcePos
    symbol "["
    e <- expr
    midPos <- getSourcePos
    symbol "|"
    (sels, cond) <- rhsE
    endPos <- getSourcePos
    _ <- optional $ symbol ","
    char ']'
    return $ ArrayComp startPos e midPos (NEList.fromList sels) cond endPos
  where
    selectE :: Parser (SourcePos, Ident, SourcePos, Expr () SourcePos)
    selectE = do
      startPos <- getSourcePos
      var <- lexeme $ Ident <$> variable
      arrPos <- getSourcePos
      e <- symbol "<-" *> expr
      return (startPos, var, arrPos, e)

    rhsE :: Parser ([(SourcePos, Ident, SourcePos, Expr () SourcePos, Maybe SourcePos)], Maybe (SourcePos, Expr () SourcePos))
    rhsE =
      try
        ( do
            (startPos, var, arrPos, e) <- selectE
            pos <- getSourcePos <* symbol ","
            (xs, mcond) <- try rhsE <|> (\c -> ([], Just c)) <$> condE
            pure ((startPos, var, arrPos, e, Just pos) : xs, mcond)
        )
        <|> (\(startPos, var, arrPos, e) -> ([(startPos, var, arrPos, e, Nothing)], Nothing)) <$> selectE

    condE :: Parser (SourcePos, Expr () SourcePos)
    condE = do
      ifPos <- getSourcePos
      (ifPos,) <$> (rword "if" *> expr)

array :: Parser a -> Parser (SourcePos, [(a, Maybe SourcePos)], SourcePos)
array p = label "array\nfor example: [1,2,3,4,5]" $
  lexeme $ do
    startPos <- getSourcePos
    symbol "["
    args <- argsE
    endPos <- getSourcePos
    char ']'
    return (startPos, args, endPos)
  where
    argsE =
      try
        ( do
            e <- p
            commaPos <- getSourcePos
            symbol ","
            es <- argsE
            return ((e, Just commaPos) : es)
        )
        <|> try
          ( do
              e1 <- p
              return [(e1, Nothing)]
          )
        <|> pure []

record :: Parser a -> Parser (SourcePos, [(Ident, a, Maybe SourcePos)], SourcePos)
record p = label "record\nfor example: {name = \"Zaphod\"; age = 391}" $
  lexeme $ do
    startPos <- getSourcePos
    symbol "{"
    args <- argsE
    endPos <- getSourcePos
    char '}'
    return (startPos, args, endPos)
  where
    argsE =
      try
        ( do
            f <- lexeme $ Ident <$> variable
            symbol "="
            e <- p
            commaPos <- getSourcePos
            symbol ";"
            es <- argsE
            return ((f, e, Just commaPos) : es)
        )
        <|> try
          ( do
              f <- lexeme $ Ident <$> variable
              symbol "="
              e1 <- p
              return [(f, e1, Nothing)]
          )
        <|> pure []

mkInterpolatedString :: [Either Text e] -> [Either Text e]
mkInterpolatedString [] = []
mkInterpolatedString (Left x : Left y : xs) = mkInterpolatedString (Left (x <> y) : xs)
mkInterpolatedString (x : xs) = x : mkInterpolatedString xs

funE :: Parser (Expr () SourcePos)
funE = label "a function\nfor example: fun x y -> x + y" $ do
  startPos <- getSourcePos
  hidden $ rword "fun"
  args <- some mExtIdent
  arrPos <- getSourcePos
  symbol "->" <?> "'->'"
  Lam startPos (NEList.fromList args) arrPos <$> expr

renameModE :: Parser (Expr () SourcePos)
renameModE = label "a 'let module' expression\nfor example: let module A = Base in A.#true" $
  do
    hidden $ rword "let"
    hidden $ rword "module"
    newNmPos <- getSourcePos
    newNm <- lexeme (ModuleName <$> variable <?> "module name")
    symbol "=" <?> "'='"
    oldNmPos <- getSourcePos
    oldNm <- lexeme (ModuleName <$> variable <?> "name of an existing module")
    inPos <- getSourcePos
    (opsTable, modOpsTables, customTypes) <- ask
    case Map.lookup oldNm modOpsTables of
      Nothing -> customFailure $ ModuleNotFound oldNm
      Just opsTableOldNm -> do
        let opsTable' = IntMap.unionWith (<>) opsTable $ IntMap.map (\xs -> [(fix, Scope newNm, op) | (fix, _, op) <- xs]) opsTableOldNm
        let modOpsTables' = Map.insert newNm opsTableOldNm modOpsTables
        e <- withReaderT (const (opsTable', modOpsTables', customTypes)) $ (rword "in" <?> "_the 'in' keyword") *> expr
        pure $ RenameModule newNmPos newNm oldNmPos oldNm inPos e

data InfernoParsingError
  = ModuleNotFound ModuleName
  | InfixOpNotFound ModuleName Ident
  | UnboundTyVar Text
  | ImplicitVarTypeAnnot
  deriving (Eq, Show, Ord)

instance ShowErrorComponent InfernoParsingError where
  showErrorComponent (ModuleNotFound (ModuleName modNm)) =
    "Module '" <> unpack modNm <> "' could not be found"
  showErrorComponent (InfixOpNotFound (ModuleName modNm) (Ident op)) =
    "Module " <> unpack modNm <> " does not export `(" <> unpack op <> ")`"
  showErrorComponent (UnboundTyVar ty) =
    "Unbound type variable '" <> unpack ty
  showErrorComponent ImplicitVarTypeAnnot =
    "Implicit variables cannot have type annotations"

openModArgs :: ModuleName -> Parser ([(Import SourcePos, Maybe SourcePos)], SourcePos, Expr () SourcePos)
openModArgs modNm = do
  symbol "("
  is <- go
  _ <- optional $ symbol ","
  symbol ")"
  (opsTable, modOpsTables, customTypes) <- ask
  opsTable' <-
    foldM
      ( \oTbl i -> case i of
          IOpVar _ op -> do
            foundOpTbl <- findOp op modOpsTables
            return $ IntMap.unionWith (<>) oTbl foundOpTbl
          _ -> pure oTbl
      )
      opsTable
      (map fst is)
  inPos <- getSourcePos
  e <- withReaderT (const (opsTable', modOpsTables, customTypes)) $ (rword "in" <?> "_the 'in' keyword") *> expr
  return (is, inPos, e)
  where
    findOp :: Ident -> Map.Map ModuleName OpsTable -> Parser OpsTable
    findOp i@(Ident op) modOpsTables =
      case Map.lookup modNm modOpsTables of
        Just opsTable -> do
          let filteredOpsTable =
                IntMap.mapMaybe
                  (\xs -> let xs' = [(fix, LocalScope, op') | (fix, _modNm, op') <- xs, op == op'] in if null xs' then Nothing else Just xs')
                  opsTable
          when (null filteredOpsTable) $ customFailure $ InfixOpNotFound modNm i
          return filteredOpsTable
        Nothing -> customFailure $ ModuleNotFound modNm
    go =
      try
        ((:) <$> ((,) <$> parseImport <*> lexeme (Just <$> getSourcePos <* symbol ",")) <*> go)
        <|> (\i -> [(i, Nothing)]) <$> parseImport

    parseImport =
      try (IOpVar <$> getSourcePos <*> lexeme (char '(' *> (Ident <$> takeWhile1P Nothing isAlphaNum) <* char ')'))
        <|> try (IEnum <$> lexeme (getSourcePos <* string "enum") <*> getSourcePos <*> lexeme (Ident <$> variable))
        <|> IVar <$> getSourcePos <*> lexeme (Ident <$> variable)

openModE :: Parser (Expr () SourcePos)
openModE = label "an 'open' module expression\nfor example: open A in ..." $
  do
    hidden $ rword "open"
    nmPos <- getSourcePos
    nm <- lexeme (ModuleName <$> variable <?> "module name")
    uncurry3 (OpenModule nmPos () nm) <$> (try (openModArgs nm) <|> (\inPos e -> ([], inPos, e)) <$> getSourcePos <*> openAll nm)
  where
    openAll modNm = do
      (opsTable, modOpsTables, customTypes) <- ask
      case Map.lookup modNm modOpsTables of
        Just opsTable' ->
          withReaderT (const (IntMap.unionWith (<>) opsTable opsTable', modOpsTables, customTypes)) $
            (rword "in" <?> "_the 'in' keyword") *> expr
        Nothing -> customFailure $ ModuleNotFound modNm

letE :: Parser (Expr () SourcePos)
letE = label ("a 'let' expression" ++ example "x") $
  do
    startPos <- getSourcePos
    hidden $ rword "let"
    varPos <- getSourcePos
    x <- lexeme ((Expl . ExtIdent . Right <$> variable <|> Impl . ExtIdent . Right <$> implicitVariable) <?> "a variable")
    let xStr = unpack $ renderPretty x
    tPos <- getSourcePos
    maybeTy <-
      optional $
        symbol ":"
          *> withReaderT (\(ops, m, customTypes) -> (mempty, ops, m, customTypes)) schemeParser
    eqPos <- getSourcePos
    symbol "=" <?> "'='"
    e1 <- expr <?> ("an expression to bind to '" ++ xStr ++ "'" ++ example xStr)
    inPos <- getSourcePos
    e2 <- (rword "in" <?> "_the 'in' keyword") *> expr
    case (x, maybeTy) of
      (Expl x', Just t) -> pure $ LetAnnot startPos varPos x' tPos t eqPos e1 inPos e2
      (Impl _, Just _) -> customFailure ImplicitVarTypeAnnot
      (_, Nothing) -> pure $ Let startPos varPos x eqPos e1 inPos e2
  where
    example x = "\nfor example: let " ++ x ++ " = 2 * 5 in ...\nor: let " ++ x ++ " : double = 1 + 2 in ..."

pat :: Parser (Pat () SourcePos)
pat =
  uncurry3 PArray <$> array pat
    <|> uncurry3 PRecord <$> record pat
    <|> try (uncurry3 PTuple <$> tuple pat)
    <|> parens pat
    <|> try (hexadecimal PLit)
    <|> try (signedDoubleE PLit)
    <|> signedIntE PLit
    <|> enumE PEnum
    <|> uncurry PVar <$> mIdent
    <|> noneE PEmpty
    <|> someE POne pat
    <|> stringE PLit

casePatts :: Parser [(SourcePos, Pat () SourcePos, SourcePos, Expr () SourcePos)]
casePatts = do
  -- The | is optional before the first match clause:
  _ <- optional (symbol "|" <?> "'|'")
  onePat `sepBy1` (symbol "|" <?> "'|'")
  where
    onePat :: Parser (SourcePos, Pat () SourcePos, SourcePos, Expr () SourcePos)
    onePat = label "_a pattern match clause\nfor example: #true -> ..." $ do
      startPos <- getSourcePos
      p <- pat
      arrPos <- getSourcePos
      symbol "->" <?> "'->'"
      e <- expr
      return (startPos, p, arrPos, e)

caseE :: Parser (Expr () SourcePos)
caseE = label "a pattern-match expression\nfor example: match x with { 1 -> #true | _ -> #false }" $
  lexeme $ do
    startPos <- getSourcePos
    rword "match"
    e <- expr <?> "an expression to pattern match on\nfor example: match (x, y) with { ... }"
    rword "with"
    brPos <- getSourcePos
    symbol "{"
    cs <- casePatts
    endPos <- getSourcePos
    char '}'
    return $ Case startPos e brPos (NEList.fromList cs) endPos

tupleArgs :: SomeParser r a -> SomeParser r [(a, Maybe SourcePos)]
tupleArgs p =
  try
    ( do
        e <- p
        commaPos <- lexeme $ char ',' *> getSourcePos
        es <- tupleArgs p
        return ((e, Just commaPos) : es)
    )
    <|> do
      e1 <- p
      commaPos <- lexeme $ char ',' *> getSourcePos
      e2 <- p
      return [(e1, Just commaPos), (e2, Nothing)]

isHSpace :: Char -> Bool
isHSpace x = isSpace x && x /= '\n' && x /= '\r'

tuple :: SomeParser r a -> SomeParser r (SourcePos, TList (a, Maybe SourcePos), SourcePos)
tuple p = label "a tuple\nfor example: (2, #true, 4.4)" $
  lexeme $ do
    startPos <- getSourcePos
    symbol "("
    r <- tListFromList <$> tupleArgs p <|> takeWhileP Nothing isHSpace $> TNil
    endPos <- getSourcePos
    char ')'
    return (startPos, r, endPos)

assertE :: Parser (Expr () SourcePos)
assertE = label "an assertion\nfor example: assert x > 10 in ..." $ do
  startPos <- getSourcePos
  hidden $ rword "assert"
  e1 <- expr <?> "a boolean expression\nfor example: x > 10 && x <= 25"
  inPos <- getSourcePos
  e2 <- (rword "in" <?> "_the 'in' keyword") *> expr
  return $ Assert startPos e1 inPos e2

ifE :: Parser (Expr () SourcePos)
ifE = do
  ifPos <- getSourcePos
  hidden $ rword "if"
  cond <- hidden expr <?> "_a conditional expression\nfor example: x > 2"
  thenPos <- getSourcePos
  tr <- (rword "then" *> expr) <?> "_the 'then' branch\nfor example: if x > 2 then 1 else 0"
  elsePos <- getSourcePos
  fl <- (rword "else" *> expr) <?> "_the 'else' branch\nfor example: if x > 2 then 1 else 0"
  return $ If ifPos cond thenPos tr elsePos fl

-- | Parses an op in prefix syntax WITHOUT opening paren @(@ but with closing paren @)@
-- E.g. @+)@
prefixOpsWithoutModule :: SourcePos -> Parser (Expr () SourcePos)
prefixOpsWithoutModule startPos =
  hidden (ask >>= choiceOf (prefixOp startPos) . opsInLocalScope)
  where
    opsInLocalScope (ops, _, _) = [s | (_, modNm, s) <- concat ops, modNm == LocalScope]
    prefixOp :: SourcePos -> Text -> Parser (Expr () SourcePos)
    prefixOp pos s = do
      _ <- symbol $ s <> ")"
      return $ OpVar pos () LocalScope $ Ident s

-- | Parses a op in prefix syntax of the form @Mod.(+)@
prefixOpsWithModule :: SourcePos -> Parser (Expr () SourcePos)
prefixOpsWithModule startPos =
  hidden (ask >>= tryMany prefixOp . opsNotInLocal)
  where
    opsNotInLocal (ops, _, _) = [(modNm, s) | (_, modNm, s) <- concat ops, modNm /= LocalScope]
    prefixOp :: (Scoped ModuleName, Text) -> Parser (Expr () SourcePos)
    prefixOp (modNm, s) = do
      _ <- symbol $ fromScoped "" (unModuleName <$> modNm) <> ".(" <> s <> ")"
      return $ OpVar startPos () modNm $ Ident s

-- | Parses a tuple @a, b, c, ...)@ WITHOUT opening paren @(@ but with closing paren @)@
-- Returns (list of (expr, commaPos), endParenPos)
tupleElems :: Parser ([(Expr () SourcePos, Maybe SourcePos)], SourcePos)
tupleElems =
  ( do
      e <- expr
      ( do
          char ')'
          endPos <- getSourcePos
          return ([(e, Nothing)], endPos)
        )
        <|> ( do
                commaPos <- lexeme $ char ',' *> getSourcePos
                (es, endPos) <- tupleElems
                return ((e, Just commaPos) : es, endPos)
            )
  )
    <|> do
      char ')'
      endPos <- getSourcePos
      return ([], endPos)

-- | Parses any bracketed expression: tuples, bracketed exprs (1 + 2), and prefix ops (+)
bracketedE :: Parser (Expr () SourcePos)
bracketedE = do
  startPos <- getSourcePos
  symbol "("
  -- Either a prefix op or a tuple. bracketed exprs are 1-tuples
  prefixOpsWithoutModule startPos <|> bracketedOrTuple startPos
  where
    bracketedOrTuple startPos = do
      (es, endPos) <- tupleElems
      lexeme $ pure $ case es of
        [] -> Tuple startPos TNil endPos
        [(e, _)] -> Bracketed startPos e endPos
        _ -> Tuple startPos (tListFromList es) endPos

term :: Parser (Expr () SourcePos)
term =
  bracketedE
    <|> try (hexadecimal Lit)
    <|> try doubleE
    <|> intE
    <|> enumE Enum
    <|> do
      -- Variable: foo or Mod.foo or Mod.(+)
      -- Record field access rec.field is also parsed as Var and converted to RecordField
      -- in pinExpr
      startPos <- getSourcePos
      lexeme $
        try
          ( ( \nmspc x ->
                Var startPos () (Scope $ ModuleName nmspc) $
                  Expl $
                    ExtIdent $
                      Right x
            )
              <$> variable
              <*> (char '.' *> variable)
          )
          <|> prefixOpsWithModule startPos
          <|> try
            ( Var startPos () LocalScope . Expl . ExtIdent . Right
                <$> variable
                <* notFollowedBy (char '.')
            )
    <|> noneE Empty
    <|> someE One expr
    <|> ifE
    <|> try renameModE
    <|> letE
    <|> openModE
    <|> assertE
    <|> funE
    <|> caseE
    <|> try implVarE
    <|> stringE Lit
    <|> interpolatedStringE
    <|> try (uncurry3 Array <$> array expr)
    <|> try (uncurry3 Record <$> record expr)
    <|> arrayComprE

app :: Parser (Expr () SourcePos)
app =
  term >>= \x ->
    (some term >>= \xs -> return (foldl App x xs))
      <|> return x

expr :: Parser (Expr () SourcePos)
expr = ask >>= \(opsTable, _, _) -> makeExprParser app $ mkOperators opsTable

mkOperators :: OpsTable -> [[Operator Parser (Expr () SourcePos)]]
mkOperators opsTable =
  [ map (uncurry3 $ mkOperatorP prec) opGrp | (prec, opGrp) <- IntMap.toDescList opsTable
  ]
  where
    infixLabel = label "an infix operator\nfor example: +, *, ==, >, <"
    prefixLabel = label "a prefix operator\nfor example: -, !"

    opString :: Scoped ModuleName -> Text -> Text
    opString modNm s = case modNm of
      LocalScope -> s
      Scope (ModuleName ns) -> ns <> "." <> s

    mkOperatorP :: Int -> Fixity -> Scoped ModuleName -> Text -> Operator Parser (Expr () SourcePos)
    mkOperatorP prec (InfixOp NoFix) ns o =
      InfixN $
        infixLabel $
          (\pos e1 e2 -> Op e1 pos () (prec, NoFix) ns (Ident o) e2) <$> lexeme (getSourcePos <* string (opString ns o))
    mkOperatorP prec (InfixOp LeftFix) ns o =
      InfixL $
        infixLabel $
          (\pos e1 e2 -> Op e1 pos () (prec, LeftFix) ns (Ident o) e2) <$> lexeme (getSourcePos <* string (opString ns o))
    mkOperatorP prec (InfixOp RightFix) ns o =
      InfixR $
        infixLabel $
          (\pos e1 e2 -> Op e1 pos () (prec, RightFix) ns (Ident o) e2) <$> lexeme (getSourcePos <* string (opString ns o))
    mkOperatorP prec PrefixOp ns o =
      Prefix $
        prefixLabel $
          (\pos e -> PreOp pos () prec ns (Ident o) e) <$> lexeme (getSourcePos <* string (opString ns o))

parseExpr ::
  OpsTable ->
  Map.Map ModuleName OpsTable ->
  [CustomType] ->
  Text ->
  Either
    (NonEmpty (ParseError Text InfernoParsingError, SourcePos))
    (Expr () SourcePos, [Comment SourcePos])
parseExpr opsTable modOpsTables customTypes s =
  case runParser (runWriterT $ flip runReaderT (opsTable, modOpsTables, customTypes) $ topLevel expr) "<stdin>" s of
    Left (ParseErrorBundle errs pos) -> Left $ fst $ attachSourcePos errorOffset errs pos
    Right (e, comments) -> Right (e, appEndo comments [])

-- parsing types

type TyParser = ReaderT (Map.Map Text Int, OpsTable, Map.Map ModuleName OpsTable, [CustomType]) (WriterT Comments (Parsec InfernoParsingError Text))

rws_type :: [Text] -- list of reserved type sig words
rws_type = ["define", "on", "forall"]

typeIdent :: TyParser Text
typeIdent = try (p >>= check)
  where
    p = pack <$> (((:) <$> letterChar <*> hidden (many alphaNumChar)) <?> "a type")
    check x =
      if x `elem` rws_type
        then fail $ "Keyword " <> show x <> " cannot be a variable/function name"
        else return x

baseType :: TyParser InfernoType
baseType = do
  (_, _, _, customTypes) <- ask
  TBase
    <$> ( symbol "int" $> TInt
            <|> symbol "double" $> TDouble
            <|> symbol "word16" $> TWord16
            <|> symbol "word32" $> TWord32
            <|> symbol "word64" $> TWord64
            <|> symbol "text" $> TText
            <|> try (symbol "timeDiff" $> TTimeDiff)
            <|> symbol "time" $> TTime
            <|> symbol "resolution" $> TResolution
            <|> choice (map (\t -> symbol (pack t) $> TCustom t) customTypes)
        )

type_variable_raw :: TyParser Text
type_variable_raw = char '\'' *> takeWhile1P Nothing isAlphaNum

type_variable :: TyParser Int
type_variable = do
  nm <- type_variable_raw
  (tys, _, _, _) <- ask
  case Map.lookup nm tys of
    Just i -> return i
    Nothing -> customFailure $ UnboundTyVar nm

recordType :: TyParser (Map.Map Ident InfernoType, RestOfRecord)
recordType = label "record type\nfor example: {name: text; age: int}" $
  lexeme $ do
    symbol "{"
    (fields, rest) <- argsE
    return (Map.fromList fields, rest)
  where
    argsE =
      try
        ( do
            f <- lexeme $ Ident <$> fieldName
            symbol ":"
            e <- typeParser
            symbol ";"
            (fields, rest) <- argsE
            return ((f, e) : fields, rest)
        )
        <|> ( do
                f <- lexeme $ Ident <$> fieldName
                symbol ":"
                e1 <- typeParser
                char '}'
                return ([(f, e1)], RowAbsent)
            )
        <|> ( do
                t <- type_variable
                char '}'
                return ([], RowVar $ TV t)
            )
        <|> ( do
                char '}'
                pure ([], RowAbsent)
            )
    keywords = Set.fromList $ rws ++ rws_type
    fieldName :: TyParser Text
    fieldName = f >>= check
      where
        f = pack <$> (((:) <$> letterChar <*> hidden (many alphaNumCharOrSeparator)) <?> "a record field name")
        check x =
          if x `Set.member` keywords
            then fail $ "Keyword " <> show x <> " cannot be a record field name"
            else return x

typeParserBase :: TyParser InfernoType
typeParserBase =
  try ((\(_, tys, _) -> TTuple $ fmap fst tys) <$> tuple typeParser)
    <|> parens typeParser
    <|> try (lexeme baseType)
    <|> uncurry TRecord <$> recordType
    <|> lexeme (TBase <$> (TEnum <$> typeIdent <*> (Set.fromList <$> (symbol "{" *> enumList <* symbol "}"))))
    <|> lexeme (TVar . TV <$> type_variable)
  where
    enumList =
      try
        ((:) <$> enumConstructor <* symbol "," <*> enumList)
        <|> (: []) <$> enumConstructor

typeParser :: TyParser InfernoType
typeParser =
  makeExprParser
    typeParserBase
    [
      [ Prefix (TArray <$ rword "array" <* rword "of")
      , Prefix (TSeries <$ rword "series" <* rword "of")
      , Prefix (TOptional <$ rword "option" <* rword "of")
      ]
    ,
      [ InfixR (TArr <$ symbol "->")
      , InfixR (TArr <$ symbol "→")
      ]
    ]

parseType ::
  Text ->
  Either
    (NonEmpty (ParseError Text InfernoParsingError, SourcePos))
    InfernoType
parseType s = case runParser (runWriterT $ flip runReaderT (mempty, mempty, mempty, []) $ topLevel typeParser) "<stdin>" s of
  Left (ParseErrorBundle errs pos) -> Left $ fst $ attachSourcePos errorOffset errs pos
  Right (e, _) -> Right e

parseTCScheme :: Text -> Either String TCScheme
parseTCScheme s =
  case runParser (runWriterT $ flip runReaderT (mempty, mempty, mempty, []) $ topLevel schemeParser) "<stdin>" s of
    Left (ParseErrorBundle errs pos) -> Left $ show $ fst $ attachSourcePos errorOffset errs pos
    Right (e, _) -> Right e

listParser :: TyParser a -> TyParser [a]
listParser p =
  try
    ( do
        e <- p
        symbol ","
        es <- listParser p
        return (e : es)
    )
    <|> do
      e1 <- p
      return [e1]

tyContext :: TyParser [Either TypeClass (Text, InfernoType)]
tyContext = lexeme $ do
  _ <- symbol "{"
  res <- listParser tyContextSingle
  _ <- symbol "}"
  _ <- symbol "=>" <|> symbol "⇒"
  return res

typeClass :: TyParser TypeClass
typeClass = TypeClass <$> (lexeme typeIdent <* symbol "on") <*> many typeParser

tyContextSingle :: TyParser (Either TypeClass (Text, InfernoType))
tyContextSingle = Left <$> (symbol "requires" *> typeClass) <|> Right <$> ((,) <$> (symbol "implicit" *> lexeme (withReaderT (\(_, ops, m, customTypes) -> (ops, m, customTypes)) variable)) <*> (symbol ":" *> typeParser))

schemeParser :: TyParser TCScheme
schemeParser = do
  vars <- try (rword "forall" *> many (lexeme type_variable_raw) <* rword ".") <|> pure mempty
  withReaderT (\(_, ops, m, ts) -> (Map.fromList $ zip vars [0 ..], ops, m, ts)) $
    constructScheme <$> (try tyContext <|> pure []) <*> typeParser
  where
    constructScheme :: [Either TypeClass (Text, InfernoType)] -> InfernoType -> TCScheme
    constructScheme cs t =
      let (tcs, impls) = partitionEithers cs
       in closeOver (Set.fromList tcs) $ ImplType (Map.fromList $ map (first (ExtIdent . Right)) impls) t

doc :: Parser Text
doc = do
  _ <- symbol "@doc"
  txt <- takeWhileP Nothing (/= ';')
  _ <- symbol ";"
  return txt

data TopLevelDefn def
  = Signature
      { documentation :: Maybe Text
      , name :: SigVar
      , def :: def
      }
  | EnumDef (Maybe Text) Text [Ident]
  | TypeClassInstance TypeClass
  | Export ModuleName
  deriving (Eq, Show, Data)

enumConstructors :: Parser [Ident]
enumConstructors = try ((:) <$> (lexeme enumConstructor <* symbol "|") <*> enumConstructors) <|> (: []) <$> lexeme enumConstructor

sigVariable :: Parser SigVar
sigVariable =
  ask >>= \(opsTable, _, _) ->
    let opList =
          concatMap
            ( \case
                (InfixOp _, _ns, i) -> [i]
                _ -> []
            )
            $ concat
            $ IntMap.elems opsTable
        preOpList =
          concatMap
            ( \case
                (PrefixOp, _ns, i) -> [i]
                _ -> []
            )
            $ concat
            $ IntMap.elems opsTable
     in lexeme $
          tryMany (\op -> char '(' *> (SigOpVar <$> string op) <* char ')') opList
            <|> tryMany (fmap SigVar . string) preOpList
            <|> SigVar <$> variable

data QQDefinition = QQRawDef String | QQToValueDef String | InlineDef (Expr () SourcePos) deriving (Data)

exprOrBuiltin :: Parser QQDefinition
exprOrBuiltin =
  try (QQToValueDef . unpack <$> lexeme (string "###" *> withReaderT (const (mempty, mempty, [])) variable <* string "###"))
    <|> try (QQRawDef . unpack <$> lexeme (string "###!" *> withReaderT (const (mempty, mempty, [])) variable <* string "###"))
    <|> InlineDef <$> expr

sigParser :: Parser (TopLevelDefn (Maybe TCScheme, QQDefinition))
sigParser =
  ( try (Signature <$> (try (Just <$> doc) <|> pure Nothing) <*> sigVariable <*> ((,) <$> (try (Just <$> (symbol ":" *> withReaderT (\(ops, m, customTypes) -> (mempty, ops, m, customTypes)) schemeParser)) <|> pure Nothing) <*> (symbol ":=" *> exprOrBuiltin)))
      <|> try ((EnumDef . Just <$> doc) <*> (symbol "enum" *> lexeme variable <* symbol ":=") <*> enumConstructors)
      <|> EnumDef Nothing <$> (symbol "enum" *> lexeme variable <* symbol ":=") <*> enumConstructors
      <|> TypeClassInstance <$> (symbol "define" *> withReaderT (\(ops, m, customTypes) -> (mempty, ops, m, customTypes)) typeClass)
      <|> Export <$> (symbol "export" *> (ModuleName <$> lexeme variable))
  )
    <* symbol ";"

fixityP :: Parser Fixity
fixityP =
  lexeme $
    try (rword "infixr" $> InfixOp RightFix)
      <|> try (rword "infixl" $> InfixOp LeftFix)
      <|> try (rword "infix" $> InfixOp NoFix)
      <|> try (rword "prefix" $> PrefixOp)

type OpsTable = IntMap.IntMap [(Fixity, Scoped ModuleName, Text)]

fixityLvl :: Parser Int
fixityLvl = try (lexeme Lexer.decimal >>= check)
  where
    check x =
      if x >= 0 && x < 20
        then return x
        else fail "Fixity level annotation must be between 0 and 19 (inclusive)"

sigsParser :: Parser (OpsTable, [TopLevelDefn (Maybe TCScheme, QQDefinition)])
sigsParser =
  try (opDeclP >>= \opsTable' -> withReaderT (const opsTable') sigsParser)
    <|> try
      ( do
          def <- sigParser
          (opsTable, defs) <- sigsParser
          return (opsTable, def : defs)
      )
    <|> try
      ( do
          (opsTable, _, _) <- opDeclP
          return (opsTable, [])
      )
    <|> (sigParser >>= \r -> ask >>= \(opsTable, _, _) -> return (opsTable, [r]))
  where
    opDeclP = ask >>= \(opsTable, modOpsTables, customTypes) -> (\f l o -> (insertIntoOpsTable opsTable f l o, modOpsTables, customTypes)) <$> fixityP <*> fixityLvl <*> (operatorP <* symbol ";")
    operatorP = lexeme $ takeWhile1P (Just "operator") (\c -> c /= ';' && not (isSpace c))

insertIntoOpsTable :: OpsTable -> Fixity -> Int -> Text -> OpsTable
insertIntoOpsTable opsTable fixity lvl op =
  IntMap.alter
    ( \case
        Nothing -> Just [(fixity, LocalScope, op)]
        Just xs -> Just $ xs ++ [(fixity, LocalScope, op)]
    )
    lvl
    opsTable

modulesParser :: Parser [(ModuleName, OpsTable, [TopLevelDefn (Maybe TCScheme, QQDefinition)])]
modulesParser = do
  symbol "module"
  moduleNm <- ModuleName <$> lexeme variable
  (ops, sigs) <- sigsParser
  let opsQualified = IntMap.map (map (\(fix, _, t) -> (fix, Scope moduleNm, t))) ops
  try
    ( do
        ms <- withReaderT (\(prevOps, modOpsTables, customTypes) -> (IntMap.unionWith (<>) prevOps opsQualified, Map.insert moduleNm ops modOpsTables, customTypes)) modulesParser
        pure $ (moduleNm, ops, sigs) : ms
    )
    <|> pure [(moduleNm, ops, sigs)]

topLevel :: SomeParser r a -> SomeParser r a
topLevel p = sc *> p <* eof
