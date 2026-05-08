{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Inferno.Parse
  ( Comment (..),
    OpsTable,
    TopLevelDefn (..),
    ParsedModule (..),
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

import Control.Applicative (asum, optional, (<|>))
import Control.Arrow ((&&&))
import Control.Monad (foldM, join, void)
import Control.Monad.Combinators.Expr
  ( Operator (InfixL, InfixN, InfixR, Prefix),
    makeExprParser,
  )
import Control.Monad.Reader (ReaderT, ask, asks, withReaderT)
import Control.Monad.State.Strict (StateT, modify')
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Char (isAlphaNum, isSpace)
import Data.Data (Data)
import Data.Foldable (foldl') -- foldl': DO NOT REMOVE; needed for older GHC compat
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tuple.Extra (second3)
import Inferno.Parse.Error (prettyError)
import Inferno.Types.Syntax
  ( Comment (BlockComment, LineComment),
    CustomType,
    Expr
      ( App,
        Array,
        Assert,
        Bracketed,
        Case,
        Empty,
        Enum,
        If,
        Lam,
        Let,
        LetAnnot,
        Lit,
        One,
        Op,
        OpVar,
        OpenModule,
        PreOp,
        Record,
        RenameModule,
        Tuple,
        Var
      ),
    ExtIdent (ExtIdent),
    Fixity (InfixOp, PrefixOp),
    Ident (Ident),
    ImplExpl (Expl, Impl),
    Import (IEnum, IOpVar, IVar),
    InfixFixity (LeftFix, NoFix, RightFix),
    Lit (LDouble, LHex, LInt, LText),
    ModuleName (ModuleName),
    Pat (PArray, PEmpty, PEnum, PLit, POne, PRecord, PTuple, PVar),
    RestOfRecord,
    Scoped (LocalScope, Scope),
    SigVar (SigOpVar, SigVar),
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
import Inferno.Utils.Prettyprinter (renderPretty)
import Text.Megaparsec
  ( MonadParsec (hidden, label, notFollowedBy, takeWhile1P, takeWhileP, try),
    ParseError,
    Parsec,
    ShowErrorComponent (showErrorComponent),
    SourcePos,
    anySingle,
    between,
    customFailure,
    getSourcePos,
    many,
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
  deriving stock (Eq, Show, Ord)

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
  = Signature (Maybe Text) SigVar def
  | EnumDef (Maybe Text) Text [Ident]
  | TypeClassInstance TypeClass
  | Export ModuleName
  deriving stock (Eq, Show, Data)

data ParsedModule a = ParsedModule
  { name :: !ModuleName
  , opsTable :: !OpsTable
  , defs :: [TopLevelDefn a]
  }
  deriving stock (Eq, Show, Data)

type OpsTable = IntMap.IntMap [(Fixity, Scoped ModuleName, Text)]

data QQDefinition
  = QQRawDef String
  | QQToValueDef String
  | InlineDef (Expr () SourcePos)
  deriving stock (Data)

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

-- | The body of an @open@/@let module@ expression: imported names, @in@ position, and body.
data ModuleBody = ModuleBody
  { imports :: [Separated (Import SourcePos)]
  , inPos :: !SourcePos
  , body :: Expr () SourcePos
  }

-- | A single clause in a @match ... with@ expression.
data CaseClause = CaseClause
  { patPos :: !SourcePos
  , pat :: Pat () SourcePos
  , arrPos :: !SourcePos
  , body :: Expr () SourcePos
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

array :: forall a. Parser a -> Parser (Delimited a)
array p =
  label "array\nfor example: [1,2,3,4,5]" . lexeme $ do
    open <- getSourcePos
    void $ symbol "["
    items <- argsE
    close <- getSourcePos
    void $ char ']'
    pure Delimited{open, items, close}
  where
    argsE :: Parser [Separated a]
    argsE = asum [p >>= rest, pure mempty]

    rest :: a -> Parser [Separated a]
    rest e =
      asum
        [ withSourcePos $ \pos ->
            symbol "," *> fmap (Separated{val = e, sep = Just pos} :) argsE
        , pure [Separated{val = e, sep = Nothing}]
        ]

record :: forall a. Parser a -> Parser (Delimited (Field a))
record p =
  label "record\nfor example: {name = \"Zaphod\"; age = 391}" . lexeme $ do
    open <- getSourcePos
    void $ symbol "{"
    items <- argsE
    close <- getSourcePos
    void $ char '}'
    pure Delimited{open, items, close}
  where
    argsE :: Parser [Separated (Field a)]
    argsE = fieldP <|> pure mempty

    fieldP :: Parser [Separated (Field a)]
    fieldP = do
      name <- lexeme $ Ident <$> variable
      void $ symbol "="
      val <- p
      rest Field{name, val}

    rest :: Field a -> Parser [Separated (Field a)]
    rest fld =
      asum
        [ withSourcePos $ \pos ->
            symbol ";" *> fmap (Separated fld (Just pos) :) argsE
        , pure [Separated fld Nothing]
        ]

mkArray :: Delimited (Expr () SourcePos) -> Expr () SourcePos
mkArray del = flip (Array del.open) del.close $ fmap ((.val) &&& (.sep)) del.items

mkRecord :: Delimited (Field (Expr () SourcePos)) -> Expr () SourcePos
mkRecord del = Record del.open (fmap toTriple del.items) del.close
  where
    toTriple ::
      Separated (Field (Expr () SourcePos)) ->
      (Ident, Expr () SourcePos, Maybe SourcePos)
    toTriple s = (s.val.name, s.val.val, s.sep)

mkInterpolatedString :: [Either Text e] -> [Either Text e]
mkInterpolatedString = undefined

funE :: Parser (Expr () SourcePos)
funE = label "a function\nfor example: fun x y -> x + y" $ do
  startPos <- getSourcePos
  hidden $ rword "fun"
  args <- (:|) <$> mExtIdent <*> many mExtIdent
  arrPos <- getSourcePos
  void . label "'->'" $ symbol "->"
  Lam startPos args arrPos <$> expr

renameModE :: Parser (Expr () SourcePos)
renameModE =
  label "a 'let module' expression\nfor example: let module A = Base in A.#true" $ do
    hidden $ rword "let"
    hidden $ rword "module"
    newNmPos <- getSourcePos
    newNm <- label "module name" . lexeme $ ModuleName <$> variable
    void . label "'='" $ symbol "="
    oldNmPos <- getSourcePos
    oldNm <- label "name of an existing module" . lexeme $ ModuleName <$> variable
    env <- ask
    Map.lookup oldNm env.modOps & \case
      Nothing -> customFailure $ ModuleNotFound oldNm
      Just oldTbl -> do
        withSourcePos $ \inPos ->
          fmap (RenameModule newNmPos newNm oldNmPos oldNm inPos)
            . flip withReaderT inExpr
            . const
            $ mkParseEnv ops modOps env.customTypes
        where
          ops :: OpsTable
          ops =
            IntMap.unionWith (<>) env.ops
              . flip IntMap.map oldTbl
              $ fmap (toOpTuple . reScope newNm . fromOpTuple)

          modOps :: Map ModuleName OpsTable
          modOps = Map.insert newNm oldTbl env.modOps

          reScope :: ModuleName -> OpEntry -> OpEntry
          reScope nm e = e{scope = Scope nm}

inExpr :: Parser (Expr () SourcePos)
inExpr = label "_the 'in' keyword" (rword "in") *> expr

openModArgs :: ModuleName -> Parser ModuleBody
openModArgs modNm = do
  void $ symbol "("
  is <- importList
  void . optional $ symbol ","
  void $ symbol ")"
  env <- ask
  ops <- foldM mergeOp env.ops $ fmap (.val) is
  inPos <- getSourcePos
  fmap (ModuleBody is inPos)
    . flip withReaderT inExpr
    . const
    $ mkParseEnv ops env.modOps env.customTypes
  where
    mergeOp :: OpsTable -> Import SourcePos -> Parser OpsTable
    mergeOp tbl = \case
      IOpVar _ op -> IntMap.unionWith (<>) tbl <$> findOp op
      _ -> pure tbl

    findOp :: Ident -> Parser OpsTable
    findOp i@(Ident op) =
      asks (.modOps) >>= \mOps ->
        Map.lookup modNm mOps & \case
          Nothing -> customFailure $ ModuleNotFound modNm
          Just tbl ->
            bool
              (pure filtered)
              (customFailure (InfixOpNotFound modNm i))
              $ IntMap.null filtered
            where
              filtered :: OpsTable
              filtered = IntMap.mapMaybe filterOp tbl

              filterOp ::
                [(Fixity, Scoped ModuleName, Text)] ->
                Maybe [(Fixity, Scoped ModuleName, Text)]
              filterOp xs =
                xs
                  & filter ((== op) . (.name) . fromOpTuple)
                  <&> toOpTuple . reLocal . fromOpTuple
                  & \case
                    [] -> Nothing
                    ys -> Just ys

    importList :: Parser [Separated (Import SourcePos)]
    importList =
      asum
        [ try $
            parseImport >>= \i -> withSourcePos $ \pos ->
              symbol "," *> fmap (Separated i (Just pos) :) importList
        , pure . (`Separated` Nothing) <$> parseImport
        ]

    parseImport :: Parser (Import SourcePos)
    parseImport =
      asum
        [ try $
            IOpVar
              <$> getSourcePos
              <*> lexeme
                ( char '('
                    *> fmap Ident (takeWhile1P Nothing isAlphaNum)
                    <* char ')'
                )
        , try $
            IEnum
              <$> lexeme (getSourcePos <* string "enum")
              <*> getSourcePos
              <*> lexeme (fmap Ident variable)
        , IVar <$> getSourcePos <*> lexeme (fmap Ident variable)
        ]

    reLocal :: OpEntry -> OpEntry
    reLocal e = e{scope = LocalScope}

openModE :: Parser (Expr () SourcePos)
openModE = label "an 'open' module expression\nfor example: open A in ..." $ do
  hidden $ rword "open"
  nmPos <- getSourcePos
  nm <- label "module name" . lexeme $ ModuleName <$> variable
  fmap (mkOpenModule nmPos nm) $ try (openModArgs nm) <|> openAll nm
  where
    openAll :: ModuleName -> Parser ModuleBody
    openAll modNm =
      ask >>= \env ->
        Map.lookup modNm env.modOps & \case
          Nothing -> customFailure $ ModuleNotFound modNm
          Just tbl -> withSourcePos $ \inPos ->
            fmap (ModuleBody mempty inPos)
              . flip withReaderT inExpr
              . const
              $ mkParseEnv
                (IntMap.unionWith (<>) env.ops tbl)
                env.modOps
                env.customTypes

    mkOpenModule :: SourcePos -> ModuleName -> ModuleBody -> Expr () SourcePos
    mkOpenModule nmPos nm mb =
      OpenModule
        nmPos
        ()
        nm
        (fmap ((.val) &&& (.sep)) mb.imports)
        mb.inPos
        mb.body

letE :: Parser (Expr () SourcePos)
letE = label ("a 'let' expression" <> example "x") $ do
  startPos <- getSourcePos
  hidden $ rword "let"
  varPos <- getSourcePos
  x <-
    label "a variable" . lexeme $
      asum
        [ Expl . ExtIdent . Right <$> variable
        , Impl . ExtIdent . Right <$> implicitVariable
        ]
  tPos <- getSourcePos
  mTy <- optional $ symbol ":" *> withReaderT toTyEnv schemeParser
  eqPos <- getSourcePos
  void . label "'='" $ symbol "="
  e1 <- expr <?> (bindMsg . Text.unpack . renderPretty) x
  inPos <- getSourcePos
  e2 <- inExpr
  (x, mTy) & \case
    (Expl y, Just t) ->
      pure $ LetAnnot startPos varPos y tPos t eqPos e1 inPos e2
    (Impl _, Just _) -> customFailure ImplicitVarTypeAnnot
    (_, Nothing) -> pure $ Let startPos varPos x eqPos e1 inPos e2
  where
    example :: String -> String
    example x =
      unwords
        [ "\nfor example: let"
        , x
        , "= 2 * 5 in ...\nor: let"
        , x
        , ": double = 1 + 2 in ..."
        ]

    bindMsg :: String -> String
    bindMsg x =
      unwords
        [ "an expression to bind to"
        , "'" <> x <> "'"
        , example x
        ]

    toTyEnv :: ParseEnv -> TyParseEnv
    toTyEnv env =
      TyParseEnv
        { tyVars = mempty
        , ops = env.ops
        , modOps = env.modOps
        , ctypes = env.customTypes
        }

pat :: Parser (Pat () SourcePos)
pat =
  asum
    [ mkPArray <$> array pat
    , mkPRecord <$> record pat
    , try $ uncurry3 PTuple <$> tuple pat
    , parens pat
    , try $ hexadecimal PLit
    , try $ signedDoubleE PLit
    , signedIntE PLit
    , enumE PEnum
    , uncurry PVar <$> mIdent
    , noneE PEmpty
    , someE POne pat
    , stringE PLit
    ]
  where
    mkPArray ::
      Delimited (Pat () SourcePos) -> Pat () SourcePos
    mkPArray del = PArray del.open (fmap ((.val) &&& (.sep)) del.items) del.close

    mkPRecord ::
      Delimited (Field (Pat () SourcePos)) -> Pat () SourcePos
    mkPRecord del = PRecord del.open (fmap toTriple del.items) del.close
      where
        toTriple ::
          Separated (Field (Pat () SourcePos)) ->
          (Ident, Pat () SourcePos, Maybe SourcePos)
        toTriple s = (s.val.name, s.val.val, s.sep)

caseClause :: Parser CaseClause
caseClause = label "_a pattern match clause\nfor example: #true -> ..." $ do
  patPos <- getSourcePos
  p <- pat
  arrPos <- getSourcePos
  void . label "'->'" $ symbol "->"
  CaseClause patPos p arrPos <$> expr

caseE :: Parser (Expr () SourcePos)
caseE =
  label "a pattern-match expression\nfor example: match x with { 1 -> #true | _ -> #false }"
    . lexeme
    $ do
      startPos <- getSourcePos
      rword "match"
      e <-
        expr
          <?> "an expression to pattern match on\nfor example: match (x, y) with { ... }"
      rword "with"
      brPos <- getSourcePos
      void $ symbol "{"
      cs <- casePatts
      endPos <- getSourcePos
      void $ char '}'
      pure $ mkCase startPos e brPos cs endPos
  where
    casePatts :: Parser (NonEmpty CaseClause)
    casePatts = do
      void . optional . label "'|'" $ symbol "|"
      (:|) <$> caseClause <*> many (label "'|'" (symbol "|") *> caseClause)

    mkCase ::
      SourcePos ->
      Expr () SourcePos ->
      SourcePos ->
      NonEmpty CaseClause ->
      SourcePos ->
      Expr () SourcePos
    mkCase startPos e brPos cs = Case startPos e brPos $ fmap toTuple cs
      where
        toTuple ::
          CaseClause -> (SourcePos, Pat () SourcePos, SourcePos, Expr () SourcePos)
        toTuple c = (c.patPos, c.pat, c.arrPos, c.body)

isHSpace :: Char -> Bool
isHSpace c = isSpace c && c /= '\n' && c /= '\r'

tuple :: ParserOver r a -> ParserOver r (SourcePos, TList (a, Maybe SourcePos), SourcePos)
tuple p = label "a tuple\nfor example: (2, #true, 4.4)" . lexeme $ do
  startPos <- getSourcePos
  void $ symbol "("
  r <-
    tListFromList . fmap ((.val) &&& (.sep)) <$> tupleArgs p
      <|> takeWhileP Nothing isHSpace $> TNil
  endPos <- getSourcePos
  void $ char ')'
  pure (startPos, r, endPos)

tupleArgs :: forall r a. ParserOver r a -> ParserOver r [Separated a]
tupleArgs p = mid <|> end
  where
    mid, end :: ParserOver r [Separated a]
    mid = try $ do
      e <- p
      commaPos <- lexeme $ char ',' *> getSourcePos
      (Separated e (Just commaPos) :) <$> tupleArgs p
    end = do
      e1 <- p
      commaPos <- lexeme $ char ',' *> getSourcePos
      e2 <- p
      pure [Separated e1 $ Just commaPos, Separated e2 Nothing]

assertE :: Parser (Expr () SourcePos)
assertE = label "an assertion\nfor example: assert x > 10 in ..." $ do
  startPos <- getSourcePos
  hidden $ rword "assert"
  e1 <- expr <?> "a boolean expression\nfor example: x > 10 && x <= 25"
  inPos <- getSourcePos
  Assert startPos e1 inPos <$> (label "_the 'in' keyword" (rword "in") *> expr)

ifE :: Parser (Expr () SourcePos)
ifE = do
  ifPos <- getSourcePos
  hidden $ rword "if"
  cond <- hidden expr <?> "_a conditional expression\nfor example: x > 2"
  thenPos <- getSourcePos
  tr <- (rword "then" *> expr) <?> "_the 'then' branch\nfor example: if x > 2 then 1 else 0"
  elsePos <- getSourcePos
  fmap (If ifPos cond thenPos tr elsePos)
    . label "_the 'else' branch\nfor example: if x > 2 then 1 else 0"
    $ rword "else" *> expr

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
          [s] -> Bracketed pos s.val endPos
          _ -> flip (Tuple pos) endPos . tListFromList $ fmap ((.val) &&& (.sep)) es

    -- Parses `a, b, c, ...)` WITHOUT the opening paren but WITH the closing paren.
    tupleElems :: Parser ([Separated (Expr () SourcePos)], SourcePos)
    tupleElems =
      asum
        [ expr >>= \e ->
            asum
              [ char ')' *> withSourcePos (pure . ([Separated e Nothing],))
              , lexeme (char ',' *> getSourcePos) >>= \commaPos ->
                  fmap (first (Separated e (Just commaPos) :)) tupleElems
              ]
        , char ')' *> withSourcePos (pure . (mempty,))
        ]

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
    , try $ mkArray <$> array expr
    , try $ mkRecord <$> record expr
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
          opP ns o >>= \pos -> pure . PreOp pos () prec ns $ Ident o

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
sigVariable =
  lexeme . asum $
    [ tryMany infixOp =<< asks (.infixOps)
    , tryMany (fmap SigVar . string) =<< asks (.prefixOps)
    , SigVar <$> variable
    ]
  where
    infixOp :: Text -> Parser SigVar
    infixOp op = char '(' *> fmap SigOpVar (string op) <* char ')'

exprOrBuiltin :: Parser QQDefinition
exprOrBuiltin = undefined

sigParser :: Parser (TopLevelDefn (Maybe TCScheme, QQDefinition))
sigParser = undefined

fixityP :: Parser Fixity
fixityP =
  lexeme . asum $
    [ try $ rword "infixr" $> InfixOp RightFix
    , try $ rword "infixl" $> InfixOp LeftFix
    , try $ rword "infix" $> InfixOp NoFix
    , rword "prefix" $> PrefixOp
    ]

fixityLvl :: Parser Int
fixityLvl = try $ lexeme Lexer.decimal >>= check
  where
    check :: Int -> Parser Int
    check x =
      bool
        (fail "Fixity level annotation must be between 0 and 19 (inclusive)")
        (pure x)
        $ x >= 0 && x < 20

sigsParser :: Parser (OpsTable, [TopLevelDefn (Maybe TCScheme, QQDefinition)])
sigsParser = go id
  where
    -- Avoids O(n) snoc (previously was `reverse acc`); now O(n) materialization,
    -- O(1) snoc
    go ::
      (
        [TopLevelDefn (Maybe TCScheme, QQDefinition)] ->
        [TopLevelDefn (Maybe TCScheme, QQDefinition)]
      ) ->
      Parser (OpsTable, [TopLevelDefn (Maybe TCScheme, QQDefinition)])
    go f =
      asum
        [ flip withReaderT (go f) . const =<< opDeclP
        , go . (f .) . (:) =<< sigParser
        , asks $ (, f mempty) . (.ops)
        ]

    opDeclP :: Parser ParseEnv
    opDeclP = do
      env <- ask
      f <- fixityP
      l <- fixityLvl
      o <- operatorP <* symbol ";"
      pure $ mkParseEnv (insertIntoOpsTable env.ops f l o) env.modOps env.customTypes

    operatorP :: Parser Text
    operatorP = lexeme $ takeWhile1P (Just "operator") isOpChar

    isOpChar :: Char -> Bool
    isOpChar = (&&) <$> (/= ';') <*> not . isSpace

insertIntoOpsTable :: OpsTable -> Fixity -> Int -> Text -> OpsTable
insertIntoOpsTable tbl f lvl op = IntMap.alter go lvl tbl
  where
    go ::
      Maybe [(Fixity, Scoped ModuleName, Text)] ->
      Maybe [(Fixity, Scoped ModuleName, Text)]
    go = \cases
      Nothing -> Just [(f, LocalScope, op)]
      (Just xs) -> Just $ (f, LocalScope, op) : xs

modulesParser :: Parser [ParsedModule (Maybe TCScheme, QQDefinition)]
modulesParser = do
  void $ symbol "module"
  nm <- ModuleName <$> lexeme variable
  (ops, sigs) <- sigsParser

  let qualOps :: OpsTable
      qualOps = flip IntMap.map ops . fmap . second3 . const $ Scope nm

  (ParsedModule nm ops sigs :)
    <$> asum
      [ flip withReaderT modulesParser . const =<< asks (mergedEnv qualOps nm ops)
      , pure mempty
      ]
  where
    mergedEnv :: OpsTable -> ModuleName -> OpsTable -> ParseEnv -> ParseEnv
    mergedEnv qualOps nm modTbl env =
      mkParseEnv
        (IntMap.unionWith (<>) env.ops qualOps)
        (Map.insert nm modTbl env.modOps)
        env.customTypes

topLevel :: ParserOver r a -> ParserOver r a
topLevel = undefined
