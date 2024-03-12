{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Inferno.Types.Syntax
  ( Ident (..),
    ExtIdent (..),
    ImplExpl (..),
    Import (..),
    ModuleName (..),
    InfixFixity (..),
    Fixity (..),
    Comment (..),
    IStr (..),
    OpsTable,
    SomeIStr (..),
    toEitherList,
    fromEitherList,
    Lit (..),
    Pat (..),
    PatF (..),
    TV (..),
    CustomType,
    BaseType (..),
    RestOfRecord (..),
    InfernoType (..),
    Expr
      ( ..,
        Var_,
        OpVar_,
        TypeRep_,
        Enum_,
        App_,
        Lam_,
        Let_,
        LetAnnot_,
        Lit_,
        InterpolatedString_,
        If_,
        Op_,
        PreOp_,
        Tuple_,
        One_,
        Empty_,
        Assert_,
        Case_,
        Array_,
        ArrayComp_,
        Record_,
        RecordField_,
        Bracketed_,
        RenameModule_,
        OpenModule_
      ),
    BlockUtils (..),
    ElementPosition (..),
    TList (..),
    SigVar (..),
    SourcePos (..),
    Scoped (..),
    Dependencies (..),
    collectArrs,
    extractArgsAndPrettyPrint,
    tListToList,
    tListFromList,
    sigVarToIdent,
    sigVarToExpr,
    patternToExpr,
    incSourceCol,
    fromScoped,
    rws,
    punctuate',
    hideInternalIdents,
    substInternalIdents,
    getIdentifierPositions,
    ImplType (..),
    Namespace (..),
    TCScheme (..),
    TypeClass (..),
    TypeClassShape (..),
    TypeMetadata (..),
    Substitutable (..),
    Subst (..),
    Scheme (..),
    (.->),
    sch,
    var,
    tySig,
    namespaceToIdent,
    typeInt,
    typeBool,
    typeDouble,
    typeWord16,
    typeWord32,
    typeWord64,
    typeText,
    typeResolution,
    typeTimeDiff,
    typeTime,
  )
where

import Control.Applicative (liftA, liftA2, liftA3)
import Control.DeepSeq (NFData (..))
import Control.Monad (replicateM)
import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (FromJSONKeyTextParser), ToJSON (..), ToJSONKey (..))
import Data.Aeson.Types (toJSONKeyText)
import Data.Bifunctor (bimap)
import Data.Bifunctor.TH (deriveBifunctor)
import Data.Data (Constr, Data (..), Typeable, gcast1, mkConstr, mkDataType)
import qualified Data.Data as Data
import Data.Functor.Foldable (ana, cata, project)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Int (Int64)
import qualified Data.IntMap as IntMap
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Serialize (Serialize (..))
import qualified Data.Serialize as Serialize
import qualified Data.Set as Set
import Data.String (IsString)
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import GHC.Generics (Generic)
import Inferno.Utils.Prettyprinter (renderPretty)
import Numeric (showHex)
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    align,
    comma,
    concatWith,
    enclose,
    encloseSep,
    flatAlt,
    group,
    hardline,
    hsep,
    indent,
    lbrace,
    lbracket,
    line,
    line',
    lparen,
    nest,
    rbrace,
    rbracket,
    rparen,
    sep,
    vsep,
    -- sep,
    -- tupled,
    (<+>),
  )
import qualified Prettyprinter.Internal as Pretty
import Text.Megaparsec (Pos, SourcePos (..), mkPos, unPos)
import Text.Read (readMaybe)

newtype TV = TV {unTV :: Int}
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, NFData, Hashable, Serialize)

-- | Custom type names
type CustomType = String

data BaseType
  = TInt
  | TDouble
  | TWord16
  | TWord32
  | TWord64
  | TText
  | TTime
  | TTimeDiff
  | TResolution
  | TEnum Text (Set.Set Ident)
  | TCustom CustomType
  deriving (Show, Eq, Ord, Data, Generic, ToJSON, FromJSON, NFData)

-- Serialize is needed by VPDB to store runtime type reps
instance Serialize BaseType where
  get =
    Serialize.getInt8 >>= \case
      0 -> pure TInt
      1 -> pure TDouble
      2 -> pure TWord16
      3 -> pure TWord32
      4 -> pure TWord64
      5 -> pure TText
      6 -> pure TTime
      7 -> pure TTimeDiff
      8 -> pure TResolution
      9 -> do
        nm <- Serialize.get
        ids <- Serialize.get
        pure $ TEnum (Text.decodeUtf8 nm) $ Set.fromList $ map (Ident . Text.decodeUtf8) ids
      10 -> do
        t <- Serialize.get
        pure $ TCustom t
      _ -> error "Unknown serialization of BaseType"

  put = \case
    TInt -> Serialize.putInt8 0
    TDouble -> Serialize.putInt8 1
    TWord16 -> Serialize.putInt8 2
    TWord32 -> Serialize.putInt8 3
    TWord64 -> Serialize.putInt8 4
    TText -> Serialize.putInt8 5
    TTime -> Serialize.putInt8 6
    TTimeDiff -> Serialize.putInt8 7
    TResolution -> Serialize.putInt8 8
    TEnum nm ids -> do
      Serialize.putInt8 9
      Serialize.put $ Text.encodeUtf8 nm
      Serialize.put $ map (Text.encodeUtf8 . unIdent) $ Set.toList ids
    TCustom t -> do
      Serialize.putInt8 10
      Serialize.put t

-- Hashable is needed by Haxl for requests
instance Hashable BaseType where
  hashWithSalt s TInt = hashWithSalt s (1 :: Int)
  hashWithSalt s TDouble = hashWithSalt s (2 :: Int)
  hashWithSalt s TWord16 = hashWithSalt s (3 :: Int)
  hashWithSalt s TWord32 = hashWithSalt s (4 :: Int)
  hashWithSalt s TWord64 = hashWithSalt s (5 :: Int)
  hashWithSalt s TText = hashWithSalt s (6 :: Int)
  hashWithSalt s TTime = hashWithSalt s (7 :: Int)
  hashWithSalt s TTimeDiff = hashWithSalt s (8 :: Int)
  hashWithSalt s TResolution = hashWithSalt s (9 :: Int)
  hashWithSalt s (TEnum nm cs) = hashWithSalt s (10 :: Int, nm, Set.toList cs)
  hashWithSalt s (TCustom t) = hashWithSalt s (11 :: Int, t)

-- | A row variable: a special kind of type variable that represents zero or more fields
-- of a record type. Technically, it is a map from the set of all remaining field names to
-- @Present(type variable) | RowAbsent@
-- indicating whether the field is present (and if so, what type) or absent.
-- In practice, since all records that we consider have finitely many fields, it is
-- sufficient to encode the remaining fields (rest of the record) as either a row
-- variable @RowVar TV@ (indicating an unknown rest of record)
-- or the special value @RowAbsent@ that indicates that no remaining field is
-- present in the record. For example:
--
-- @
-- {x = 2.2; y = "z"} : {x: double; y: text; RowAbsent}
-- fun r -> r.x + 1 : {x: double; RowVar 'a} -> double
-- @
--
-- The type of the function above indicates that it accepts any record as argument as
-- long as it has at least a field called @x@.
--
-- Reference: https://www.cl.cam.ac.uk/teaching/1415/L28/rows.pdf (Sections 8.2 and 8.4.1)
data RestOfRecord = RowVar TV | RowAbsent
  deriving (Show, Eq, Ord, Data, Generic, ToJSON, FromJSON, NFData, Hashable)
  deriving anyclass (Serialize)

data InfernoType
  = TVar TV
  | TBase BaseType
  | TArr InfernoType InfernoType
  | TArray InfernoType
  | -- | A record type containing *at least* the given fields (with types) and a row variable representing any potential other fields
    TRecord (Map.Map Ident InfernoType) RestOfRecord
  | TSeries InfernoType
  | TOptional InfernoType
  | TTuple (TList InfernoType)
  | TRep InfernoType
  deriving (Show, Eq, Ord, Data, Generic, ToJSON, FromJSON, NFData, Hashable)
  deriving anyclass (Serialize)

punctuate' :: Doc ann -> [Doc ann] -> [Doc ann]
punctuate' _ [] = []
punctuate' _ [d] = [d]
punctuate' p (d : ds) = (d <+> p) : punctuate' p ds

collectArrs :: InfernoType -> [InfernoType]
collectArrs (TArr ty1 ty2) = ty1 : collectArrs ty2
collectArrs t = [t]

-- | ['a', 'b', ..., 'z', 'aa', 'bb', ..., 'zz', ...]
allTypeVars :: [Text]
allTypeVars = map Text.pack $ [1 ..] >>= flip replicateM ['a' .. 'z']

instance Pretty TV where
  pretty (TV i)
    | 0 <= i = "'" <> pretty (allTypeVars !! i)
    | otherwise = "'a" <> pretty (-i)

instance Pretty BaseType where
  pretty = \case
    TInt -> "int"
    TDouble -> "double"
    TWord16 -> "word16"
    TWord32 -> "word32"
    TWord64 -> "word64"
    TText -> "text"
    TTime -> "time"
    TTimeDiff -> "timeDiff"
    TResolution -> "resolution"
    TEnum t cs -> pretty t <> encloseSep lbrace rbrace comma (map (((<>) "#") . pretty . unIdent) $ Set.toList cs)
    TCustom t -> pretty t

instance Pretty InfernoType where
  pretty = \case
    TVar v -> pretty v
    TBase b -> pretty b
    t@(TArr _ _) ->
      let prettyType = align . sep . punctuate' "→"
       in prettyType $
            map (\t' -> case t' of TArr _ _ -> enclose lparen rparen $ pretty t'; _ -> pretty t') $
              collectArrs t
    TArray ty@(TVar _) -> "array of" <+> align (pretty ty)
    TArray ty@(TBase _) -> "array of" <+> align (pretty ty)
    TArray ty@(TTuple _) -> "array of" <+> align (pretty ty)
    TArray ty -> "array of" <+> align (enclose lparen rparen $ pretty ty)
    TRecord tys rowVar -> "{" <> prettyFields <> prettyRest rowVar <> "}"
      where
        prettyFields = sep $ Pretty.punctuate ";" $ map prettyField $ Map.toAscList tys
        prettyField (Ident f, ty) = pretty f <> ":" <+> pretty ty
        semicolon = if Map.null tys then "" else ";"
        prettyRest (RowVar tv) = semicolon <> pretty tv
        prettyRest RowAbsent = mempty
    TSeries ty@(TVar _) -> "series of" <+> align (pretty ty)
    TSeries ty@(TBase _) -> "series of" <+> align (pretty ty)
    TSeries ty@(TTuple _) -> "series of" <+> align (pretty ty)
    TSeries ty -> "series of" <+> align (enclose lparen rparen $ pretty ty)
    TOptional ty@(TVar _) -> "option of" <+> align (pretty ty)
    TOptional ty@(TBase _) -> "option of" <+> align (pretty ty)
    TOptional ty@(TTuple _) -> "option of" <+> align (pretty ty)
    TOptional ty -> "option of" <+> align (enclose lparen rparen $ pretty ty)
    TTuple tys -> Pretty.tupled (map pretty $ tListToList tys)
    TRep ty@(TVar _) -> "rep of" <+> align (pretty ty)
    TRep ty@(TBase _) -> "rep of" <+> align (pretty ty)
    TRep ty@(TTuple _) -> "rep of" <+> align (pretty ty)
    TRep ty -> "rep of" <+> align (enclose lparen rparen $ pretty ty)

data ImplType = ImplType (Map.Map ExtIdent InfernoType) InfernoType
  deriving (Show, Eq, Ord, Data, Generic, ToJSON, FromJSON)

data Scheme = Forall [TV] ImplType
  deriving (Show, Eq, Ord, Data, Generic, ToJSON, FromJSON)

typeInt, typeBool, typeDouble, typeWord16, typeWord32, typeWord64, typeText, typeResolution, typeTimeDiff, typeTime :: InfernoType
typeInt = TBase TInt
typeBool = TBase $ TEnum "bool" $ Set.fromList ["true", "false"]
typeDouble = TBase TDouble
typeWord16 = TBase TWord16
typeWord32 = TBase TWord32
typeWord64 = TBase TWord64
typeText = TBase TText
typeTimeDiff = TBase TTimeDiff
typeTime = TBase TTime
typeResolution = TBase TResolution

sch :: InfernoType -> Scheme
sch = Forall [] . ImplType Map.empty

infixr 3 .->

(.->) :: InfernoType -> InfernoType -> InfernoType
x .-> y = TArr x y

var :: Int -> InfernoType
var x = TVar (TV x)

data TypeClass = TypeClass
  { className :: Text,
    params :: [InfernoType]
  }
  deriving (Show, Eq, Ord, Data, Generic, ToJSON, FromJSON)

data TCScheme = ForallTC [TV] (Set.Set TypeClass) ImplType
  deriving (Show, Eq, Ord, Data, Generic, ToJSON, FromJSON)

tySig :: [Doc ann] -> [Doc ann]
tySig [] = []
tySig [d] = [":" <+> d]
tySig (d : ds) = (":" <+> d) : go ds
  where
    go [] = []
    go (d' : ds') = ("|" <+> d') : go ds'

instance Pretty ImplType where
  pretty (ImplType impl ty)
    | Map.null impl = pretty ty
    | otherwise =
        encloseSep
          lbrace
          rbrace
          comma
          (map (\(ExtIdent idt, t) -> "implicit" <+> case idt of { Left i -> "var$" <> pretty i; Right i -> pretty i } <+> ":" <+> align (pretty t)) $ Map.toList impl)
          <+> "⇒"
          <+> pretty ty

instance Pretty Scheme where
  pretty (Forall _ implType) = pretty implType

instance Pretty TypeClass where
  pretty = \case
    TypeClass nm tys -> pretty nm <+> "on" <+> (hsep $ map bracketPretty tys)
    where
      bracketPretty ty = case ty of
        TVar _ -> pretty ty
        TBase _ -> pretty ty
        _ -> enclose lparen rparen $ pretty ty

newtype TypeClassShape = TypeClassShape TypeClass

instance Pretty TypeClassShape where
  pretty = \case
    TypeClassShape (TypeClass nm tys) -> pretty nm <+> "on" <+> (hsep $ map bracketPretty tys)
    where
      bracketPretty ty = case ty of
        TVar _ -> "_"
        TBase _ -> pretty ty
        _ -> enclose lparen rparen $ pretty ty

instance Pretty TCScheme where
  pretty (ForallTC xs tcs (ImplType impl ty)) =
    prettyXs xs
      <+> prettyPrecondition
      <+> pretty ty
    where
      prettyXs [] = mempty
      prettyXs xs' = "forall" <+> sep (fmap pretty xs') <+> "."
      prettyTcs tcs' = map (("requires" <+>) . pretty) $ Set.toList tcs'
      prettyImpls impls = map (\(ExtIdent idt, t) -> "implicit" <+> case idt of { Left i -> "var$" <> pretty i; Right i -> pretty i } <+> ":" <+> align (pretty t)) $ Map.toList impls
      prettyPrecondition =
        case prettyTcs tcs ++ prettyImpls impl of
          [] -> mempty
          precs -> encloseSep lbrace rbrace comma precs <+> "⇒"

newtype Subst = Subst (Map.Map TV InfernoType)
  deriving stock (Eq, Ord)
  deriving newtype (Semigroup, Monoid)

instance Show Subst where
  show (Subst m) = "[" <> xs <> "]"
    where
      xs = intercalate ", " $ map (\(x, t) -> unpack $ renderPretty x <> " ~> " <> renderPretty t) $ Map.toList m

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TV

instance Substitutable InfernoType where
  apply _ (TBase a) = TBase a
  apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2
  apply s (TArray t) = TArray $ apply s t
  apply (Subst s) (TRecord ts trv) = case trv of
    RowVar tv -> case Map.findWithDefault (TVar tv) tv s of
      TVar tv' -> TRecord ts' $ RowVar tv'
      -- When performing unification, we will create substitutions that map some row vars
      -- to a bunch of fields and a new row var, e.g. tv ~> f1: t1; f2: t2; tv'
      -- We need to add any new fields to the fields of this record, and use the new row
      -- var. E.g. {f0: t0; tv} ~> {f0: t0; f1: t1; f2: t2; tv'} under the above subst
      TRecord ts'' trv' -> TRecord newFields trv'
        where
          newFields =
            Map.unionWithKey
              (\f _ _ -> error $ "TRecord susbstitution with duplicate field " <> show f)
              ts'
              ts''
      t -> error $ "TRecord substitution with unsupported RHS " <> show t
    RowAbsent -> TRecord ts' RowAbsent
    where
      ts' = fmap (apply $ Subst s) ts
  apply s (TSeries t) = TSeries $ apply s t
  apply s (TOptional t) = TOptional $ apply s t
  apply s (TTuple ts) = TTuple $ fmap (apply s) ts
  apply s (TRep t) = TRep $ apply s t

  ftv TBase {} = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2
  ftv (TArray t) = ftv t
  ftv (TRecord ts trv) = foldr (Set.union . ftv) tvs ts
    where
      tvs = case trv of RowVar tv -> Set.singleton tv; RowAbsent -> Set.empty
  ftv (TSeries t) = ftv t
  ftv (TOptional t) = ftv t
  ftv (TTuple ts) = foldr (Set.union . ftv) Set.empty ts
  ftv (TRep t) = ftv t

instance Substitutable ImplType where
  apply s (ImplType impl t) =
    ImplType (Map.map (apply s) impl) $ apply s t
  ftv (ImplType impl t) = (foldr Set.union Set.empty $ map (ftv . snd) $ Map.toList impl) `Set.union` ftv t

instance Substitutable TypeClass where
  apply s (TypeClass n tys) = TypeClass n $ map (apply s) tys
  ftv (TypeClass _ tys) = Set.unions $ map ftv tys

instance Substitutable TCScheme where
  apply (Subst s) (ForallTC as tcs t) = ForallTC as (Set.map (apply s') tcs) (apply s' t)
    where
      s' = Subst $ foldr Map.delete s as
  ftv (ForallTC as tcs t) = ((ftv t) `Set.union` (Set.unions $ Set.elems $ Set.map ftv tcs)) `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv = foldr (Set.union . ftv) Set.empty

data Namespace
  = FunNamespace Ident
  | OpNamespace Ident
  | EnumNamespace Ident
  | ModuleNamespace ModuleName
  | TypeNamespace Ident
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

instance Pretty Namespace where
  pretty = \case
    FunNamespace (Ident i) -> pretty i
    OpNamespace (Ident i) -> pretty i
    EnumNamespace (Ident i) -> "#" <> pretty i
    ModuleNamespace (ModuleName m) -> pretty m
    TypeNamespace (Ident i) -> pretty i

namespaceToIdent :: Namespace -> Ident
namespaceToIdent = \case
  FunNamespace i -> i
  OpNamespace i -> i
  EnumNamespace i -> i
  TypeNamespace i -> i
  ModuleNamespace _ -> error "namespaceToIdent undefined for ModuleNamespace"

incSourceCol :: SourcePos -> Int -> SourcePos
incSourceCol pos 0 = pos
incSourceCol (SourcePos n l c) i = SourcePos n l (c <> mkPos i)

rws :: [Text] -- list of reserved words
rws = ["if", "then", "else", "let", "module", "in", "match", "with", "Some", "None", "assert", "fun", "infixr", "infixl", "infix", "enum", "open", "define", "on", "forall", "requires"]

newtype Ident = Ident {unIdent :: Text}
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString, NFData, Hashable)

instance Serialize Ident where
  put = put . Text.encodeUtf8 . unIdent
  get = Ident . Text.decodeUtf8 <$> get

newtype ModuleName = ModuleName {unModuleName :: Text}
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving newtype (ToJSON, FromJSON, IsString)

class ElementPosition a where
  elementPosition :: SourcePos -> a -> (SourcePos, SourcePos)

instance ElementPosition Ident where
  elementPosition pos (Ident a) = (pos, incSourceCol pos $ Text.length a)

instance ElementPosition ModuleName where
  elementPosition pos (ModuleName a) = (pos, incSourceCol pos $ Text.length a)

instance ElementPosition (Maybe Ident) where
  elementPosition pos = \case
    Just i -> elementPosition pos i
    Nothing -> (pos, incSourceCol pos 1)

-- | An extended identifier; either an internal (e.g., var$4) or a regular variable
newtype ExtIdent = ExtIdent (Either Int Text)
  deriving (Show, Eq, Ord, Data, Generic)
  deriving newtype (ToJSON, FromJSON, NFData)

instance ToJSONKey ExtIdent where
  toJSONKey = toJSONKeyText $ \case
    ExtIdent (Left i) -> "var$" <> (Text.pack $ show i)
    ExtIdent (Right k) -> "reg$" <> k

instance FromJSONKey ExtIdent where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    case Text.take 4 t of
      "var$" -> case readMaybe $ Text.unpack $ Text.drop 4 t of
        Just i -> pure $ ExtIdent $ Left i
        Nothing -> fail "Could not read internal var"
      "reg$" -> pure $ ExtIdent $ Right $ Text.drop 4 t
      _ -> fail "Invalid ExtIdent key"

data ImplExpl = Impl ExtIdent | Expl ExtIdent
  deriving (Show, Eq, Ord, Data, Generic, ToJSON, FromJSON)

instance Pretty ExtIdent where
  pretty (ExtIdent i) = case i of
    Left n -> "var$" <> pretty n
    Right x -> pretty x

instance Pretty ImplExpl where
  pretty = \case
    Impl a -> "?" <> pretty a
    Expl a -> pretty a

instance ElementPosition ImplExpl where
  elementPosition pos = \case
    Impl (ExtIdent (Left _)) -> (pos, pos)
    Impl (ExtIdent (Right a)) -> (pos, incSourceCol pos $ Text.length a + 1)
    Expl (ExtIdent (Left _)) -> (pos, pos)
    Expl (ExtIdent (Right a)) -> (pos, incSourceCol pos $ Text.length a)

data Fixity = InfixOp InfixFixity | PrefixOp
  deriving (Show, Eq, Ord, Data, Generic, ToJSON, FromJSON)

data InfixFixity = NoFix | LeftFix | RightFix
  deriving (Show, Eq, Ord, Data, Generic, ToJSON, FromJSON)

instance ToJSON Pos where
  toJSON = toJSON . unPos

deriving instance ToJSON SourcePos

instance FromJSON Pos where
  parseJSON = fmap mkPos . parseJSON

deriving instance FromJSON SourcePos

data Comment pos
  = LineComment pos Text pos
  | BlockComment pos Text pos
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, ToJSON, FromJSON)

instance Pretty (Comment a) where
  pretty = \case
    LineComment _ str _ -> ("//" <+> pretty str)
    BlockComment _ str _ -> encloseComment $ map pretty $ Text.splitOn "\n" $ Text.strip str
    where
      encloseComment ds = case ds of
        [] -> "/*  */"
        [d] -> "/*" <+> d <+> "*/"
        _ -> hardVcat (zipWith (<>) ("/* " : repeat mempty) ds) <> " */"

      hardVcat :: [Doc ann] -> Doc ann
      hardVcat = concatWith (\x y -> x <> hardline <> y)

data Lit
  = LInt Int64
  | LDouble Double
  | LText Text
  | LHex Word64
  deriving (Show, Eq, Ord, Data, Generic, ToJSON, FromJSON)

instance Pretty Lit where
  pretty = \case
    LInt i -> if i < 0 then "(" <> pretty i <> ")" else pretty i
    LDouble d -> if d < 0 then "(" <> pretty d <> ")" else pretty d
    LText t -> pretty $ show t
    LHex w -> "0x" <> (pretty $ showHex w "")

instance ElementPosition Lit where
  elementPosition pos l = (pos, incSourceCol pos $ length $ show $ pretty l)

data TList a = TNil | TCons a a [a]
  deriving (Show, Eq, Ord, Functor, Foldable, Data, Generic, ToJSON, FromJSON, NFData, Hashable)
  deriving anyclass (Serialize)

instance Traversable TList where
  {-# INLINE traverse #-} -- so that traverse can fuse
  traverse f = \case
    TNil -> pure TNil
    TCons x y zs -> liftA3 TCons (f x) (f y) (traverse f zs)

tListToList :: TList a -> [a]
tListToList = \case
  TNil -> []
  TCons a b cs -> a : b : cs

tListFromList :: [a] -> TList a
tListFromList = \case
  [] -> TNil
  (a : b : cs) -> TCons a b cs
  _ -> error "undefined TList"

data IStr (f :: Bool) e where
  ISEmpty :: IStr 'True e
  ISStr :: Text -> IStr 'True e -> IStr 'False e
  ISExpr :: Typeable f => e -> IStr f e -> IStr 'True e

instance (Typeable f, Data e) => Data (IStr f e) where
  gfoldl _ z ISEmpty = z ISEmpty
  gfoldl k z (ISStr s xs) = z ISStr `k` s `k` xs
  gfoldl k z (ISExpr e xs) = z ISExpr `k` e `k` xs

  gunfold _ _ _ =
    error $
      "Cannot derive a gunfold instance without unsafeCoerce.\n"
        <> "If this function is needed, try uncommenting the lines below. However, this definition might not be correct."

  -- where
  --   gunfold' :: forall c. (forall b r. Data b => c (b -> r) -> c r) -> (forall r. r -> c r) -> Constr -> c (IStr f e)
  --   gunfold' k z c = case constrIndex c of
  --     1 -> unsafeCoerce $ z ISEmpty
  --     2 -> unsafeCoerce $ k (k (z ISStr) :: Typeable f => c (IStr 'True e -> IStr 'False e))
  --     _ -> unsafeCoerce $ k (k (z ISExpr) :: Typeable f => c (IStr f e -> IStr 'True e))

  toConstr ISEmpty = con_ISEmpty
  toConstr (ISStr _ _) = con_ISStr
  toConstr (ISExpr _ _) = con_ISExpr

  dataTypeOf _ = ty_IStr
  dataCast1 f = gcast1 f

con_ISEmpty, con_ISStr, con_ISExpr :: Constr
con_ISEmpty = mkConstr ty_IStr "ISEmpty" [] Data.Prefix
con_ISStr = mkConstr ty_IStr "ISStr" [] Data.Prefix
con_ISExpr = mkConstr ty_IStr "ISExpr" [] Data.Prefix

ty_IStr :: Data.DataType
ty_IStr = mkDataType "Inferno.Syntax.IStr" [con_ISEmpty, con_ISStr, con_ISExpr]

deriving instance Show e => Show (IStr f e)

deriving instance Functor (IStr f)

deriving instance Foldable (IStr f)

instance Traversable (IStr f) where
  {-# INLINE traverse #-} -- so that traverse can fuse
  traverse f = \case
    ISEmpty -> pure ISEmpty
    ISStr s xs -> liftA (ISStr s) (traverse f xs)
    ISExpr e xs -> liftA2 ISExpr (f e) (traverse f xs)

data SomeIStr e = forall f. (Typeable f) => SomeIStr (IStr f e)

instance Data e => Data (SomeIStr e) where
  gfoldl k z (SomeIStr xs) = z SomeIStr `k` xs

  gunfold _ _ _ =
    error $
      "Cannot derive a gunfold instance without unsafeCoerce.\n"
        <> "If this function is needed, try uncommenting the lines below. However, this definition might not be correct."

  -- where
  --   gunfold' :: forall c. (forall b r. Data b => c (b -> r) -> c r) -> (forall r. r -> c r) -> Constr -> c (SomeIStr e)
  --   gunfold' k z _ = k (z SomeIStr :: c (IStr 'False e -> SomeIStr e))

  toConstr _ = con_SomeIStr
  dataTypeOf _ = ty_SomeIStr
  dataCast1 f = gcast1 f

con_SomeIStr :: Constr
con_SomeIStr = mkConstr ty_SomeIStr "SomeIStr" [] Data.Prefix

ty_SomeIStr :: Data.DataType
ty_SomeIStr = mkDataType "Inferno.Syntax.SomeIStr" [con_SomeIStr]

deriving instance Show e => Show (SomeIStr e)

instance Eq e => Eq (SomeIStr e) where
  (SomeIStr ISEmpty) == (SomeIStr ISEmpty) = True
  (SomeIStr (ISStr s1 xs)) == (SomeIStr (ISStr s2 ys)) =
    (s1 == s2) && (SomeIStr xs) == (SomeIStr ys)
  (SomeIStr (ISExpr e1 xs)) == (SomeIStr (ISExpr e2 ys)) =
    (e1 == e2) && (SomeIStr xs) == (SomeIStr ys)
  _ == _ = False

instance Ord e => Ord (SomeIStr e) where
  compare (SomeIStr ISEmpty) (SomeIStr ISEmpty) = EQ
  compare (SomeIStr ISEmpty) _ = LT
  compare _ (SomeIStr ISEmpty) = GT
  compare (SomeIStr (ISStr _ _)) (SomeIStr (ISExpr _ _)) = LT
  compare (SomeIStr (ISExpr _ _)) (SomeIStr (ISStr _ _)) = GT
  compare (SomeIStr (ISStr s xs)) (SomeIStr (ISStr t ys)) = case compare s t of
    EQ -> compare (SomeIStr xs) (SomeIStr ys)
    other -> other
  compare (SomeIStr (ISExpr e xs)) (SomeIStr (ISExpr f ys)) = case compare e f of
    EQ -> compare (SomeIStr xs) (SomeIStr ys)
    other -> other

deriving instance Functor SomeIStr

deriving instance Foldable SomeIStr

toEitherList :: SomeIStr e -> [Either Text e]
toEitherList = \case
  SomeIStr ISEmpty -> []
  SomeIStr (ISStr s ys) -> Left s : toEitherList (SomeIStr ys)
  SomeIStr (ISExpr e ys) -> Right e : toEitherList (SomeIStr ys)

fromEitherList :: [Either Text e] -> SomeIStr e
fromEitherList = \case
  [] -> SomeIStr (ISEmpty :: IStr 'True e)
  Left s : xs -> case fromEitherList xs of
    SomeIStr ISEmpty -> SomeIStr $ ISStr s ISEmpty
    SomeIStr (ISStr s' xs') -> SomeIStr $ ISStr (s <> s') xs'
    SomeIStr rest@(ISExpr _ _) -> SomeIStr $ ISStr s rest
  Right e : xs -> case fromEitherList xs of
    SomeIStr rest -> SomeIStr $ ISExpr e rest

instance FromJSON e => FromJSON (SomeIStr e) where
  parseJSON = fmap fromEitherList . parseJSON

instance ToJSON e => ToJSON (SomeIStr e) where
  toJSON = toJSON . toEitherList

instance Traversable SomeIStr where
  {-# INLINE traverse #-} -- so that traverse can fuse
  traverse f = \case
    SomeIStr (ISEmpty :: IStr f a) -> pure $ SomeIStr (ISEmpty :: IStr f b)
    SomeIStr (ISStr s xs) -> do
      res <- traverse f xs
      pure $ case res of
        ISEmpty -> SomeIStr $ ISStr s ISEmpty
        rest@(ISExpr _ _) -> SomeIStr $ ISStr s rest
    SomeIStr (ISExpr e xs) -> do
      e' <- f e
      res <- traverse f xs
      pure $ SomeIStr $ ISExpr e' res

data Import pos
  = IVar pos Ident
  | IOpVar pos Ident
  | IEnum
      pos -- position of `enum`
      pos -- position of ident
      Ident
  | ICommentAbove (Comment pos) (Import pos)
  | ICommentAfter (Import pos) (Comment pos)
  | ICommentBelow (Import pos) (Comment pos)
  deriving (Show, Eq, Ord, Functor, Foldable, Generic, Data, ToJSON, FromJSON)

makeBaseFunctor ''Import

data Scoped a = LocalScope | Scope a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Data, Generic, ToJSON, FromJSON)

fromScoped :: a -> Scoped a -> a
fromScoped d = \case
  Scope a -> a
  LocalScope -> d

data Expr hash pos
  = Var
      pos
      hash
      (Scoped ModuleName)
      ImplExpl
  | OpVar pos hash (Scoped ModuleName) Ident -- infix ops like `+` used as prefix, i.e. `(+)`
  | TypeRep pos InfernoType
  | Enum pos hash (Scoped ModuleName) Ident
  | App (Expr hash pos) (Expr hash pos)
  | Lam
      pos -- position of `fun`
      ( NonEmpty
          ( pos, -- position of variable,
            Maybe ExtIdent
          )
      )
      pos -- position of `->`
      (Expr hash pos)
  | Let
      pos -- position of `let`
      pos -- position of variable
      ImplExpl
      pos -- position of `=`
      (Expr hash pos)
      pos -- position of `in`
      (Expr hash pos)
  | LetAnnot
      pos -- position of `let`
      pos -- position of variable
      ExtIdent -- We disallow type annotations on implicit vars
      pos -- position of type annotation
      TCScheme
      pos -- position of `=`
      (Expr hash pos)
      pos -- position of `in`
      (Expr hash pos)
  | Lit pos Lit
  | InterpolatedString
      pos -- position of string start
      (SomeIStr (pos, Expr hash pos, pos))
      pos -- position of string end
  | If
      pos -- position of `if`
      (Expr hash pos)
      pos -- position of then`
      (Expr hash pos)
      pos -- position of `else`
      (Expr hash pos)
  | Op
      (Expr hash pos)
      pos -- position of operator
      hash
      (Int, InfixFixity) -- level and fixity of the operaror
      (Scoped ModuleName)
      Ident
      (Expr hash pos)
  | PreOp
      pos -- position of operator
      hash
      Int -- level of the operaror
      (Scoped ModuleName)
      Ident
      (Expr hash pos)
  | Tuple
      pos -- position of `(`
      (TList (Expr hash pos, Maybe pos))
      pos -- position of `)`
      -- NOTE: the frontend syntax is Some/None but internally we use one/empty for legacy reasons
  | One
      pos -- position of `Some`
      (Expr hash pos)
  | Empty pos -- position of `None`
  | Assert
      pos -- position of `assert`
      (Expr hash pos)
      pos -- position of `in`
      (Expr hash pos)
  | Case
      pos -- position of `match`
      (Expr hash pos)
      pos -- position of `{`
      ( NonEmpty
          ( pos, -- position of `|`
            Pat hash pos,
            pos, -- position of `->`
            Expr hash pos
          )
      )
      pos -- position of `}`
  | Record
      pos -- position of `{`
      [ ( Ident, -- field name
          Expr hash pos, -- field value
          Maybe pos -- position of `;`
        )
      ]
      pos -- position of `}`
  | RecordField -- r.f
      pos
      Ident -- the record var name
      Ident -- field name
  | Array
      pos -- position of `[`
      [ ( Expr hash pos,
          Maybe pos -- position of `,`
        )
      ]
      pos -- position of `]`
  | ArrayComp
      pos -- position of `[`
      (Expr hash pos)
      pos -- position of `|`
      ( NonEmpty
          ( pos, -- position of identifier
            Ident,
            pos, -- position of `<-`
            Expr hash pos,
            Maybe pos -- position of `,`
          )
      )
      ( Maybe
          ( pos, -- position of `if`
            Expr hash pos
          )
      )
      pos -- position of `]`
  | CommentAbove
      (Comment pos)
      (Expr hash pos)
  | CommentAfter
      (Expr hash pos)
      (Comment pos)
  | CommentBelow
      (Expr hash pos)
      (Comment pos)
  | Bracketed pos (Expr hash pos) pos
  | RenameModule
      pos -- pos of new name
      ModuleName
      pos -- pos of old name
      ModuleName
      pos -- pos of `in`
      (Expr hash pos)
  | OpenModule
      pos
      hash
      ModuleName
      [(Import pos, Maybe pos)]
      pos -- pos of `in`
      (Expr hash pos)
  deriving (Show, Eq, Ord, Functor, Foldable, Generic, Data, ToJSON, FromJSON)

{-# COMPLETE
  Var_,
  OpVar_,
  TypeRep_,
  Enum_,
  App_,
  Lam_,
  Let_,
  LetAnnot_,
  Lit_,
  InterpolatedString_,
  If_,
  Op_,
  PreOp_,
  Tuple_,
  One_,
  Empty_,
  Assert_,
  Case_,
  Array_,
  ArrayComp_,
  CommentAbove,
  CommentAfter,
  CommentBelow,
  Bracketed_,
  RenameModule_,
  OpenModule_
  #-}

pattern Var_ :: forall hash pos. hash -> Scoped ModuleName -> ImplExpl -> Expr hash pos
pattern Var_ h ns x <- Var _ h ns x

pattern OpVar_ :: forall hash pos. hash -> Scoped ModuleName -> Ident -> Expr hash pos
pattern OpVar_ h ns x <- OpVar _ h ns x

pattern TypeRep_ :: forall hash pos. InfernoType -> Expr hash pos
pattern TypeRep_ ty <- TypeRep _ ty

pattern Enum_ :: forall hash pos. hash -> Scoped ModuleName -> Ident -> Expr hash pos
pattern Enum_ h ns x <- Enum _ h ns x

pattern App_ :: forall hash pos. Expr hash pos -> Expr hash pos -> Expr hash pos
pattern App_ e1 e2 <- App e1 e2

pattern Lam_ :: forall hash pos. NonEmpty (pos, Maybe ExtIdent) -> Expr hash pos -> Expr hash pos
pattern Lam_ xs e <- Lam _ xs _ e

pattern Let_ :: forall hash pos. ImplExpl -> Expr hash pos -> Expr hash pos -> Expr hash pos
pattern Let_ x e1 e2 <- Let _ _ x _ e1 _ e2

pattern LetAnnot_ :: forall hash pos. ExtIdent -> Expr hash pos -> Expr hash pos -> Expr hash pos
pattern LetAnnot_ x e1 e2 <- LetAnnot _ _ x _ _ _ e1 _ e2

pattern Lit_ :: forall hash pos. Lit -> Expr hash pos
pattern Lit_ l <- Lit _ l

pattern InterpolatedString_ :: forall hash pos. SomeIStr (pos, Expr hash pos, pos) -> Expr hash pos
pattern InterpolatedString_ xs <- InterpolatedString _ xs _

pattern If_ :: forall hash pos. Expr hash pos -> Expr hash pos -> Expr hash pos -> Expr hash pos
pattern If_ c t f <- If _ c _ t _ f

pattern Op_ :: forall hash pos. Expr hash pos -> hash -> Scoped ModuleName -> Ident -> Expr hash pos -> Expr hash pos
pattern Op_ e1 h ns op e2 <- Op e1 _ h _ ns op e2

pattern PreOp_ :: forall hash pos. hash -> Scoped ModuleName -> Ident -> Expr hash pos -> Expr hash pos
pattern PreOp_ h ns op e <- PreOp _ h _ ns op e

pattern Tuple_ :: forall hash pos. TList (Expr hash pos, Maybe pos) -> Expr hash pos
pattern Tuple_ xs <- Tuple _ xs _

pattern One_ :: forall hash pos. Expr hash pos -> Expr hash pos
pattern One_ e <- One _ e

pattern Empty_ :: forall hash pos. Expr hash pos
pattern Empty_ <- Empty _

pattern Assert_ :: forall hash pos. Expr hash pos -> Expr hash pos -> Expr hash pos
pattern Assert_ c e <- Assert _ c _ e

pattern Case_ :: forall hash pos. Expr hash pos -> NonEmpty (pos, Pat hash pos, pos, Expr hash pos) -> Expr hash pos
pattern Case_ e xs <- Case _ e _ xs _

pattern Record_ :: forall hash pos. [(Ident, Expr hash pos, Maybe pos)] -> Expr hash pos
pattern Record_ xs <- Record _ xs _

pattern RecordField_ :: forall hash pos. Ident -> Ident -> Expr hash pos
pattern RecordField_ r f <- RecordField _ r f

pattern Array_ :: forall hash pos. [(Expr hash pos, Maybe pos)] -> Expr hash pos
pattern Array_ xs <- Array _ xs _

pattern ArrayComp_ ::
  forall hash pos.
  Expr hash pos ->
  NonEmpty (pos, Ident, pos, Expr hash pos, Maybe pos) ->
  Maybe (pos, Expr hash pos) ->
  Expr hash pos
pattern ArrayComp_ e xs c <- ArrayComp _ e _ xs c _

pattern Bracketed_ :: forall hash pos. Expr hash pos -> Expr hash pos
pattern Bracketed_ e <- Bracketed _ e _

pattern RenameModule_ :: forall hash pos. ModuleName -> ModuleName -> Expr hash pos -> Expr hash pos
pattern RenameModule_ n1 n2 e <- RenameModule _ n1 _ n2 _ e

pattern OpenModule_ :: forall hash pos. ModuleName -> [(Import pos, Maybe pos)] -> Expr hash pos -> Expr hash pos
pattern OpenModule_ n1 ns e <- OpenModule _ _ n1 ns _ e

data Pat hash pos
  = PVar pos (Maybe Ident)
  | PEnum pos hash (Scoped ModuleName) Ident
  | PLit pos Lit
  | POne pos (Pat hash pos)
  | PEmpty pos
  | PArray pos [(Pat hash pos, Maybe pos)] pos
  | PTuple pos (TList (Pat hash pos, Maybe pos)) pos
  | PCommentAbove
      (Comment pos)
      (Pat hash pos)
  | PCommentAfter
      (Pat hash pos)
      (Comment pos)
  | PCommentBelow
      (Pat hash pos)
      (Comment pos)
  deriving (Show, Eq, Ord, Functor, Foldable, Data, Generic, ToJSON, FromJSON)

makeBaseFunctor ''Pat

makeBaseFunctor ''Expr

deriveBifunctor ''Pat

deriveBifunctor ''Expr

patternToExpr :: Pat () () -> Expr () ()
patternToExpr = \case
  PVar _ Nothing -> Var () () LocalScope $ Expl $ ExtIdent $ Right "_"
  PVar _ (Just (Ident i)) -> Var () () LocalScope $ Expl $ ExtIdent $ Right i
  PEnum _ _ modNm i -> Enum () () modNm i
  PLit _ l -> Lit () l
  POne _ p -> One () $ patternToExpr p
  PEmpty _ -> Empty ()
  PArray _ ps _ -> Array () (fmap (\(pat, pos) -> (patternToExpr pat, pos)) ps) ()
  PTuple _ ps _ -> Tuple () (fmap (\(pat, pos) -> (patternToExpr pat, pos)) ps) ()
  PCommentAbove c p -> CommentAbove c $ patternToExpr p
  PCommentAfter p c -> CommentAfter (patternToExpr p) c
  PCommentBelow p c -> CommentBelow (patternToExpr p) c

getIdentifierPositions :: Ident -> Expr a SourcePos -> [(SourcePos, SourcePos)]
getIdentifierPositions (Ident i) = cata go
  where
    go :: ExprF a SourcePos [(SourcePos, SourcePos)] -> [(SourcePos, SourcePos)]
    go = \case
      VarF pos _ _ v@(Expl (ExtIdent (Right a))) -> if i == a then let (sPos, ePos) = elementPosition pos v in [(sPos, ePos)] else []
      rest -> foldr (++) [] rest

class BlockUtils f where
  blockPosition :: f SourcePos -> (SourcePos, SourcePos)
  removeComments :: f pos -> f pos
  hasLeadingComment :: f pos -> Bool
  hasTrailingComment :: f pos -> Bool
  renameModule :: Scoped ModuleName -> f pos -> f pos

instance BlockUtils Comment where
  blockPosition = \case
    LineComment s _ e -> (s, e)
    BlockComment s _ e -> (s, e)

  removeComments = id
  hasLeadingComment _ = True
  hasTrailingComment _ = True
  renameModule _ = id

instance BlockUtils Import where
  blockPosition p = cata go p
    where
      go = \case
        IVarF pos v -> elementPosition pos v
        IOpVarF pos (Ident i) -> (pos, incSourceCol pos $ Text.length i + 2)
        IEnumF pos1 pos2 (Ident i) -> (pos1, incSourceCol pos2 $ Text.length i)
        ICommentAboveF c (_, pos2) -> let (pos1, _) = blockPosition c in (pos1, pos2)
        ICommentAfterF (pos1, _) c -> let (_, pos2) = blockPosition c in (pos1, pos2)
        ICommentBelowF (pos1, _) c -> let (_, pos2) = blockPosition c in (pos1, pos2)

  removeComments = ana $ \case
    ICommentAbove _ p -> project $ removeComments p
    ICommentAfter p _ -> project $ removeComments p
    ICommentBelow p _ -> project $ removeComments p
    other -> project other
  renameModule _ = id

  hasLeadingComment = head . cata go
    where
      go = \case
        ICommentAboveF _ _ -> [True]
        rest -> foldr (++) [False] rest

  hasTrailingComment = last . cata go
    where
      go = \case
        ICommentAfterF _ _ -> [True]
        ICommentBelowF _ _ -> [True]
        rest -> foldl (++) [False] rest

instance BlockUtils (Pat hash) where
  blockPosition p = cata go p
    where
      go :: PatF hash SourcePos (SourcePos, SourcePos) -> (SourcePos, SourcePos)
      go = \case
        PVarF pos v -> elementPosition pos v
        PEnumF pos _ ns (Ident i) -> (pos, incSourceCol pos $ Text.length i + 1 + (fromScoped 0 $ (+ 1) . Text.length . unModuleName <$> ns))
        PLitF pos l -> elementPosition pos l
        PEmptyF pos -> (pos, incSourceCol pos 5)
        POneF pos1 (_, pos2) -> (pos1, pos2)
        PArrayF pos1 _ pos2 -> (pos1, incSourceCol pos2 1)
        PTupleF pos1 _ pos2 -> (pos1, incSourceCol pos2 1)
        PCommentAboveF c (_, pos2) -> let (pos1, _) = blockPosition c in (pos1, pos2)
        PCommentAfterF (pos1, _) c -> let (_, pos2) = blockPosition c in (pos1, pos2)
        PCommentBelowF (pos1, _) c -> let (_, pos2) = blockPosition c in (pos1, pos2)

  removeComments = ana $ \case
    PCommentAbove _ p -> project $ removeComments p
    PCommentAfter p _ -> project $ removeComments p
    PCommentBelow p _ -> project $ removeComments p
    other -> project other

  renameModule newNs = ana $ \case
    PEnum pos hash _ns i -> project $ PEnum pos hash newNs i
    other -> project other

  hasLeadingComment = head . cata go
    where
      go :: PatF hash pos [Bool] -> [Bool]
      go = \case
        PCommentAboveF _ _ -> [True]
        rest -> foldr (++) [False] rest

  hasTrailingComment = last . cata go
    where
      go :: PatF hash pos [Bool] -> [Bool]
      go = \case
        PCommentAfterF _ _ -> [True]
        PCommentBelowF _ _ -> [True]
        rest -> foldl (++) [False] rest

instance BlockUtils (Expr hash) where
  blockPosition e = cata go e
    where
      go :: ExprF hash SourcePos (SourcePos, SourcePos) -> (SourcePos, SourcePos)
      go = \case
        VarF pos _ ns v -> let (sPos, ePos) = elementPosition pos v in (sPos, incSourceCol ePos $ fromScoped 0 $ (+ 1) . Text.length . unModuleName <$> ns)
        OpVarF pos _ ns v -> let (sPos, ePos) = elementPosition pos v in (sPos, incSourceCol ePos $ fromScoped 2 $ (+ 3) . Text.length . unModuleName <$> ns)
        EnumF pos _ ns (Ident i) -> (pos, incSourceCol pos $ Text.length i + 1 + (fromScoped 0 $ (+ 1) . Text.length . unModuleName <$> ns))
        AppF (pos1, _) (_, pos2) -> (pos1, pos2)
        LamF pos1 _ _ (_, pos2) -> (pos1, pos2)
        LetF pos1 _ _ _ _ _ (_, pos2) -> (pos1, pos2)
        LetAnnotF pos1 _ _ _ _ _ _ _ (_, pos2) -> (pos1, pos2)
        LitF pos l -> (pos, incSourceCol pos $ length $ show $ pretty l)
        InterpolatedStringF pos1 _ pos2 -> (pos1, pos2)
        IfF pos1 _ _ _ _ (_, pos2) -> (pos1, pos2)
        OpF (pos1, _) _ _ _ _ _ (_, pos2) -> (pos1, pos2)
        PreOpF pos1 _ _ _ _ (_, pos2) -> (pos1, pos2)
        TupleF pos1 _ pos2 -> (pos1, incSourceCol pos2 1)
        OneF pos1 (_, pos2) -> (pos1, pos2)
        EmptyF pos -> (pos, incSourceCol pos 5)
        AssertF pos1 _ _ (_, pos2) -> (pos1, pos2)
        CaseF pos1 _ _ _ pos2 -> (pos1, incSourceCol pos2 1)
        RecordF pos1 _ pos2 -> (pos1, incSourceCol pos2 1)
        RecordFieldF pos1 (Ident r) (Ident f) ->
          (pos1, incSourceCol pos1 $ Text.length r + Text.length f + 1)
        ArrayF pos1 _ pos2 -> (pos1, incSourceCol pos2 1)
        ArrayCompF pos1 _ _ _ _ pos2 -> (pos1, incSourceCol pos2 1)
        CommentAboveF c (_, pos2) -> let (pos1, _) = blockPosition c in (pos1, pos2)
        CommentAfterF (pos1, _) c -> let (_, pos2) = blockPosition c in (pos1, pos2)
        CommentBelowF (pos1, _) c -> let (_, pos2) = blockPosition c in (pos1, pos2)
        BracketedF pos1 _ pos2 -> (pos1, pos2)
        RenameModuleF pos1 _ _ _ _ (_, pos2) -> (pos1, pos2)
        OpenModuleF pos1 _ _ _ _ (_, pos2) -> (pos1, pos2)
        TypeRepF pos _ -> (pos, pos)

  removeComments = ana $ \case
    CommentAbove _ p -> project $ removeComments p
    CommentAfter p _ -> project $ removeComments p
    CommentBelow p _ -> project $ removeComments p
    Case p1 e1 p2 xs p3 ->
      project $
        Case
          p1
          (removeComments e1)
          p2
          (fmap (\(p4, pat, p5, e2) -> (p4, removeComments pat, p5, removeComments e2)) xs)
          p3
    other -> project other

  renameModule newNs = ana $ \e -> project $ case e of
    Var pos hash _ns i -> Var pos hash newNs i
    OpVar pos hash _ns i -> OpVar pos hash newNs i
    Enum pos hash _ns i -> Enum pos hash newNs i
    Op e1 p1 hash meta _ns op e2 -> Op e1 p1 hash meta newNs op e2
    Case p1 e1 p2 xs p3 ->
      Case
        p1
        (renameModule newNs e1)
        p2
        (fmap (\(p4, pat, p5, e2) -> (p4, renameModule newNs pat, p5, renameModule newNs e2)) xs)
        p3
    other -> other
  hasLeadingComment = head . cata go
    where
      go :: ExprF hash pos [Bool] -> [Bool]
      go = \case
        CommentAboveF _ _ -> [True]
        rest -> foldr (++) [False] rest

  hasTrailingComment = last . cata go
    where
      go :: ExprF hash pos [Bool] -> [Bool]
      go = \case
        CommentAfterF _ _ -> [True]
        CommentBelowF _ _ -> [True]
        rest -> foldl (++) [False] rest

collectApps :: Expr hash pos -> [Expr hash pos]
collectApps (App x@(App _ _) y) = collectApps x ++ [y]
collectApps (App x y) = [x, y]
collectApps _ = undefined

-- | Filter out any var$n/?var$n variables and their let/lambda bindings
-- This is used when pretty printing for the front-end, as we don't want the
-- users to see these auto-generated internal variables.
hideInternalIdents :: Expr hash pos -> Expr hash pos
hideInternalIdents = ana $ \case
  App e (Var _ _ _ (Impl (ExtIdent (Left _)))) -> project $ hideInternalIdents e
  App e (Var _ _ _ (Expl (ExtIdent (Left _)))) -> project $ hideInternalIdents e
  App e (TypeRep _ _) -> project $ hideInternalIdents e
  Let _ _ (Impl (ExtIdent (Left _))) _ _ _ e -> project $ hideInternalIdents e
  Let _ _ (Expl (ExtIdent (Left _))) _ _ _ e -> project $ hideInternalIdents e
  Lam p1 xs p2 e ->
    let filteredXs = flip NonEmpty.filter xs $ \case
          (_, Just (ExtIdent (Left _))) -> False
          _ -> True
     in case filteredXs of
          [] -> project $ hideInternalIdents e
          (x' : xs') -> project $ Lam p1 (x' NonEmpty.:| xs') p2 $ hideInternalIdents e
  other -> project other

-- | Extract the arguments of a script and pretty print the script body.
-- This hides the internal variable arguments.
extractArgsAndPrettyPrint :: Expr hash pos -> ([Maybe Ident], Text)
extractArgsAndPrettyPrint expr =
  extract False [] (hideInternalIdents expr)
  where
    extract foundLam args = \case
      Lam _ (x :| xs) _ e -> extract True (args <> map snd (x : xs)) e
      e | foundLam -> (mapMaybe extIdentToIdent args, renderPretty e)
      e ->
        error $
          "Corrupted script. Expected a Lam but got "
            ++ take 20 (show $ bimap (const ()) (const ()) e)
            ++ "..."
    -- Strip the runtime type rep arguments, and convert others to Ident
    extIdentToIdent = \case
      (Just (ExtIdent (Left _))) -> Nothing
      (Just (ExtIdent (Right i))) -> Just $ Just $ Ident {unIdent = i}
      Nothing -> Just Nothing

-- | Substitute every variable occurrence of `?var$i` with `var$j`
-- if `(i, Left j)` is in in the supplied map.
-- otherwise replace `?var$i` with `@t` if (i, Right t) \in m`
substInternalIdents :: Map.Map Int (Either Int InfernoType) -> Expr hash pos -> Expr hash pos
substInternalIdents m = ana $ \case
  Var pos h ns (Impl (ExtIdent (Left i))) ->
    project $ case Map.lookup i m of
      Just (Left j) -> Var pos h ns (Expl (ExtIdent (Left j)))
      Just (Right t) -> TypeRep pos t
      Nothing -> Var pos h ns (Impl (ExtIdent (Left i)))
  other -> project other

instance Pretty (Import a) where
  pretty = \case
    IVar _ (Ident x) -> pretty x
    IOpVar _ (Ident x) -> "(" <> pretty x <> ")"
    IEnum _ _ (Ident x) -> "enum" <+> pretty x
    ICommentAbove c e -> pretty c <> hardline <> pretty e
    ICommentAfter e c -> pretty e <+> pretty c
    ICommentBelow e c -> pretty e <> line <> pretty c

instance Pretty (Pat hash a) where
  pretty = \case
    PVar _ (Just (Ident x)) -> pretty x
    PVar _ Nothing -> "_"
    PEnum _ _ ns (Ident n) -> (fromScoped mempty $ (<> ".") . pretty . unModuleName <$> ns) <> "#" <> pretty n
    PLit _ l -> pretty l
    PArray _ [] _ -> "[]"
    PArray _ ps _ -> group $ (flatAlt "[ " "[") <> prettyElems True "]" ps
    PTuple _ TNil _ -> "()"
    PTuple _ ps _ -> group $ (flatAlt "( " "(") <> prettyElems True ")" (tListToList ps)
    POne _ e -> "Some" <+> align (pretty e)
    PEmpty _ -> "None"
    PCommentAbove c e -> pretty c <> hardline <> pretty e
    PCommentAfter e c -> pretty e <+> pretty c
    PCommentBelow e c -> pretty e <> line <> pretty c
    where
      prettyElems firstElement closingParen = \case
        [] -> mempty
        [(e, _)] ->
          align (pretty e)
            <> ( if hasTrailingComment e
                   then hardline <> ")"
                   else flatAlt " " "" <> closingParen
               )
        (e, _) : es ->
          (if not firstElement && hasLeadingComment e then line else mempty)
            <> align (pretty e)
            <> (if hasTrailingComment e then hardline else line')
            <> ", "
            <> prettyElems False closingParen es

instance Pretty (Expr hash pos) where
  pretty = prettyPrec False 0

prettyPrec :: Bool -> Int -> Expr hash pos -> Doc ann
prettyPrec isBracketed prec expr =
  case expr of
    Var _ _ ns x -> (fromScoped mempty $ (<> ".") . pretty . unModuleName <$> ns) <> pretty x
    TypeRep _ ty -> "@" <> pretty ty
    OpVar _ _ ns (Ident x) -> (fromScoped mempty $ (<> ".") . pretty . unModuleName <$> ns) <> "(" <> pretty x <> ")"
    Enum _ _ ns (Ident n) -> (fromScoped mempty $ (<> ".") . pretty . unModuleName <$> ns) <> "#" <> pretty n
    App _ _ -> group $ nest 2 $ prettyApp $ collectApps expr
      where
        prettyAppAux m p = case m of
          Var _ _ _ _ -> p
          OpVar _ _ _ _ -> p
          Enum _ _ _ _ -> p
          Lit _ _ -> p
          InterpolatedString _ _ _ -> p
          Tuple _ _ _ -> p
          Empty _ -> p
          -- TODO test that these do the right thing!
          Record _ _ _ -> p
          RecordField _ _ _ -> p
          Array _ _ _ -> p
          ArrayComp _ _ _ _ _ _ -> p
          Bracketed _ _ _ -> p
          _ -> enclose lparen rparen $ if hasTrailingComment m then p <> hardline else p

        prettyApp = \case
          [] -> mempty
          [x] -> prettyAppAux x $ prettyPrec True 0 x
          (x : xs) -> (prettyAppAux x $ prettyPrec True 0 x) <> (if hasTrailingComment x then hardline else line) <> prettyApp xs
    Lam _ xs _ e ->
      let fun = "fun" <+> align (sep $ map (fromMaybe "_" . fmap pretty . snd) $ toList xs) <+> "->"
          body = align $ prettyPrec False 0 e
       in group $ nest 2 $ vsep [fun, body]
    Let _ _ x _ e1 _ e2 ->
      let letPretty = "let" <+> align (pretty x <+> "=" <+> align (prettyPrec False 0 e1))
          body = "in" <+> prettyPrec False 0 e2
       in letPretty <> (if hasTrailingComment e1 then hardline else line) <> body
    LetAnnot _ _ x _ t _ e1 _ e2 ->
      let letPretty = "let" <+> align (pretty x <+> ":" <+> pretty t <+> "=" <+> align (prettyPrec False 0 e1))
          body = "in" <+> prettyPrec False 0 e2
       in letPretty <> (if hasTrailingComment e1 then hardline else line) <> body
    Lit _ l -> pretty l
    InterpolatedString _ istr _ -> enclose "`" "`" $
      group $
        cat' $
          case istr of
            SomeIStr ISEmpty -> []
            SomeIStr xs@(ISStr _ _) -> prettyISStr xs
            SomeIStr xs@(ISExpr _ _) -> "${" : prettyISExpr xs
      where
        prettyISExpr :: IStr 'True (a, Expr hash a, a) -> [Doc ann]
        prettyISExpr = \case
          ISEmpty -> []
          ISExpr (_, e, _) ISEmpty -> (indentE $ prettyPrec False 0 e) : if hasTrailingComment e then [hardline, "}"] else ["}"]
          ISExpr (_, e, _) xs@(ISExpr _ _) ->
            ((indentE $ prettyPrec False 0 e) : if hasTrailingComment e then [hardline, "}${"] else ["}${"]) ++ prettyISExpr xs
          ISExpr (_, e, _) (ISStr str xs@(ISExpr _ _)) ->
            let str' = vsepHard $ addToLast $ addToFirst $ map pretty $ Text.splitOn "\n" str
             in ((indentE $ prettyPrec False 0 e) : if hasTrailingComment e then [hardline, str'] else [str']) ++ prettyISExpr xs
          ISExpr (_, e, _) (ISStr str xs) ->
            let str' = vsepHard $ addToFirst $ map pretty $ Text.splitOn "\n" str
             in ((indentE $ prettyPrec False 0 e) : if hasTrailingComment e then [hardline, str'] else [str']) ++ prettyISExpr xs

        prettyISStr :: IStr 'False (a, Expr hash a, a) -> [Doc ann]
        prettyISStr = \case
          ISStr str ISEmpty -> [pretty str]
          ISStr str xs ->
            let str' = vsepHard $ addToLast $ map pretty $ Text.splitOn "\n" str
             in str' : prettyISExpr xs

        addToLast, addToFirst :: [Doc ann] -> [Doc ann]
        addToLast = \case
          [] -> []
          [s] -> [s <> "${"]
          s : xs -> s : addToLast xs
        addToFirst = \case
          [] -> []
          s : xs -> ("}" <> s) : xs
    If _ c _ t _ f ->
      let ifPretty = "if" <+> align (prettyPrec False 0 c)
          thenPretty = "then" <+> align (prettyPrec False 0 t)
          elsePretty = "else" <+> align (prettyPrec False 0 f)
       in nest 2 $
            ifPretty
              <> (if hasTrailingComment c then hardline else line)
              <> thenPretty
              <> (if hasTrailingComment t then hardline else line)
              <> elsePretty
    Op e1 _ _ (n, NoFix) ns (Ident op) e2 ->
      bracketWhen e2 (prec > n) $
        prettyOpAux (n + 1) e1
          <> (if hasTrailingComment e1 then hardline else mempty)
          <+> prettyOp ns op
          <+> (if hasLeadingComment e2 then line else mempty)
            <> prettyOpAux (n + 1) e2
    Op e1 _ _ (n, LeftFix) ns (Ident op) e2 ->
      bracketWhen e2 (prec > n) $
        prettyOpAux n e1
          <> (if hasTrailingComment e1 then hardline else mempty)
          <+> prettyOp ns op
          <+> (if hasLeadingComment e2 then line else mempty)
            <> prettyOpAux (n + 1) e2
    Op e1 _ _ (n, RightFix) ns (Ident op) e2 ->
      bracketWhen e2 (prec > n) $
        prettyOpAux (n + 1) e1
          <> (if hasTrailingComment e1 then hardline else mempty)
          <+> prettyOp ns op
          <+> (if hasLeadingComment e2 then line else mempty)
            <> prettyOpAux n e2
    PreOp _ _ n ns (Ident op) e ->
      bracketWhen e (prec > n) $
        prettyOp ns op
          <+> (if hasLeadingComment e then line else mempty)
            <> prettyOpAux (n + 1) e
    Tuple _ TNil _ -> "()"
    Tuple _ xs _ -> group $ (flatAlt "( " "(") <> prettyTuple True (tListToList xs)
      where
        prettyTuple firstElement = \case
          [] -> mempty
          [(e, _)] ->
            align (prettyPrec False 0 e)
              <> (if hasTrailingComment e then hardline <> ")" else flatAlt " )" ")")
          (e, _) : es ->
            (if not firstElement && hasLeadingComment e then line else mempty)
              <> align (prettyPrec False 0 e)
              <> (if hasTrailingComment e then hardline else line')
              <> ", "
              <> prettyTuple False es
    One _ e -> "Some" <+> align (prettyPrec False 0 e)
    Empty _ -> "None"
    Assert _ c _ e ->
      let assertPretty = "assert" <+> align (prettyPrec False 0 c)
          body = (flatAlt "    in" "in") <+> align (prettyPrec False 0 e)
       in assertPretty <> (if hasTrailingComment c then hardline else line) <> body
    Case _ e_case _ patExprs _ ->
      group $
        nest 2 $
          vsep
            [ "match" <+> align (prettyPrec False 0 e_case <> if hasTrailingComment e_case then hardline else mempty) <+> "with" <+> "{",
              (align $ prettyCase True $ toList patExprs) <> flatAlt " }" "}"
            ]
      where
        prettyCase :: Bool -> [(a, Pat hash a, a, Expr hash a)] -> Doc ann
        prettyCase firstElement = \case
          [] -> mempty
          [(_, pat, _, e)] ->
            group
              ( "|"
                  <+> align
                    ( pretty pat
                        <> (if hasTrailingComment pat then hardline else mempty)
                        <+> "->"
                          <> line
                          <> (prettyPrec False 0 e)
                    )
              )
              <> (if hasTrailingComment e then hardline else mempty)
          (_, pat, _, e) : es ->
            (if not firstElement && hasLeadingComment pat then hardline else mempty)
              <> group ("|" <+> align (pretty pat <> (if hasTrailingComment pat then hardline else mempty) <+> "->" <> line <> (prettyPrec False 0 e)))
              <> (if hasTrailingComment e then hardline else line)
              <> prettyCase False es
    Record _ [] _ -> "{}"
    Record _ xs _ -> group $ flatAlt "{ " "{" <> prettyRecord True xs
      where
        prettyRecord firstElement = \case
          [] -> mempty
          [(Ident f, e, _)] ->
            pretty f
              <+> "="
              <+> align (prettyPrec False 0 e)
                <> (if hasTrailingComment e then hardline <> "}" else flatAlt " }" "}")
          (Ident f, e, _) : es ->
            (if not firstElement && hasLeadingComment e then line else mempty)
              <> pretty f
              <+> "="
              <+> align (prettyPrec False 0 e)
                <> (if hasTrailingComment e then hardline else line')
                <> "; "
                <> prettyRecord False es
    RecordField _ (Ident r) (Ident f) ->
      pretty r <> "." <> pretty f
    Array _ [] _ -> "[]"
    Array _ xs _ -> group $ (flatAlt "[ " "[") <> prettyArray True xs
      where
        prettyArray firstElement = \case
          [] -> mempty
          [(e, _)] ->
            align (prettyPrec False 0 e)
              <> (if hasTrailingComment e then hardline <> "]" else flatAlt " ]" "]")
          (e, _) : es ->
            (if not firstElement && hasLeadingComment e then line else mempty)
              <> align (prettyPrec False 0 e)
              <> (if hasTrailingComment e then hardline else line')
              <> ", "
              <> prettyArray False es
    ArrayComp _ e_body _ args e_cond _ ->
      enclose lbracket rbracket $
        align $
          (align $ prettyPrec False 0 e_body <> if hasTrailingComment e_body then hardline else mempty) <+> align ("|" <+> (argsPretty $ toList args))
      where
        argsPretty = \case
          [] -> mempty
          [(_, Ident n, _, e, _)] ->
            pretty n
              <+> "<-"
              <+> align (prettyPrec False 0 e)
                <> case e_cond of
                  Just (_, c) -> (if hasTrailingComment e then hardline else line') <> "," <+> "if" <+> align (prettyPrec False 0 c) <> (if hasTrailingComment c then hardline else mempty)
                  Nothing -> if hasTrailingComment e then hardline else mempty
          (_, Ident n, _, e, _) : xs ->
            pretty n
              <+> "<-"
              <+> align (prettyPrec False 0 e)
                <> (if hasTrailingComment e then hardline else line')
                <> ", "
                <> argsPretty xs
    CommentAbove c e -> pretty c <> hardline <> prettyPrec isBracketed prec e
    CommentAfter e c -> prettyPrec isBracketed prec e <+> pretty c
    CommentBelow e c -> prettyPrec isBracketed prec e <> line <> pretty c
    Bracketed _ e _ -> enclose lparen rparen $ if hasTrailingComment e then prettyPrec True prec e <> hardline else prettyPrec True prec e
    RenameModule _ (ModuleName nNew) _ (ModuleName nOld) _ e ->
      let letPretty = "let" <+> align ("module" <+> pretty nNew <+> "=" <+> pretty nOld)
          body = (flatAlt " in" "in") <+> align (prettyPrec False 0 e)
       in letPretty <> line <> body
    OpenModule _ _ (ModuleName n) ns _ e ->
      "open"
        <+> pretty n
          <> ( case ns of
                 [] -> line
                 _ -> (align $ group $ (flatAlt "( " "(") <> prettyImports True (map fst ns)) <> (if hasTrailingComment $ fst (last ns) then hardline else line)
             )
          <> (flatAlt "  in" "in")
        <+> align (prettyPrec False 0 e)
      where
        prettyImports firstElement = \case
          [] -> mempty
          [i] ->
            align (pretty i)
              <> (if hasTrailingComment i then hardline <> ")" else flatAlt " )" ")")
          i : is ->
            (if not firstElement && hasLeadingComment i then line else mempty)
              <> align (pretty i)
              <> (if hasTrailingComment i then hardline else line')
              <> ", "
              <> prettyImports False is
  where
    indentE e = flatAlt (indent 2 e) e

    vsepHard :: [Doc ann] -> Doc ann
    vsepHard = concatWith (\x y -> x <> hardline <> y)

    cat' :: [Doc ann] -> Doc ann
    cat' [] = mempty
    cat' [x] = x
    cat' (x : Pretty.Line : xs) = x <> hardline <> cat' xs
    cat' (x : xs) = x <> line' <> cat' xs

    bracketWhen e b =
      if isBracketed
        then id
        else
          if b
            then (\x -> enclose lparen rparen $ x <> if hasTrailingComment e then hardline else mempty)
            else id

    prettyOp ns op = (fromScoped mempty $ (<> ".") . pretty . unModuleName <$> ns) <> pretty op

    prettyOpAux n e = case e of
      Var _ _ _ _ -> prettyPrec False n e
      OpVar _ _ _ _ -> prettyPrec False n e
      Enum _ _ _ _ -> prettyPrec False n e
      Lit _ _ -> prettyPrec False n e
      InterpolatedString _ _ _ -> prettyPrec False n e
      Tuple _ _ _ -> prettyPrec False n e
      Empty _ -> prettyPrec False n e
      Array _ _ _ -> prettyPrec False n e
      ArrayComp _ _ _ _ _ _ -> prettyPrec False n e
      Bracketed _ _ _ -> prettyPrec False n e
      Op _ _ _ _ _ _ _ -> prettyPrec False n e
      PreOp _ _ _ _ _ _ -> prettyPrec False n e
      _ -> enclose lparen rparen $ if hasTrailingComment e then prettyPrec False n e <> hardline else prettyPrec False n e

data TypeMetadata ty = TypeMetadata
  { identExpr :: Expr () (),
    docs :: Maybe Text,
    ty :: ty
  }
  deriving (Eq, Show, Generic, Data, ToJSON, FromJSON)

data SigVar = SigVar Text | SigOpVar Text deriving (Eq, Show, Data)

sigVarToIdent :: SigVar -> Ident
sigVarToIdent x = Ident $ case x of
  SigVar i -> i
  SigOpVar i -> i

sigVarToExpr :: Scoped ModuleName -> SigVar -> Expr () ()
sigVarToExpr modNm = \case
  SigVar i -> Var () () modNm $ Expl $ ExtIdent $ Right i
  SigOpVar i -> OpVar () () modNm $ Ident i

type OpsTable = IntMap.IntMap [(Fixity, Scoped ModuleName, Text)]

class Dependencies f hash where
  getDependencies :: Ord hash => f -> Set.Set hash

instance Dependencies (Pat hash pos) hash where
  getDependencies = cata $ \case
    PEnumF _ h _ _ -> Set.singleton h
    rest -> foldr Set.union mempty rest

instance Dependencies (Expr hash pos) hash where
  getDependencies = cata $ \case
    VarF _ h _ _ -> Set.singleton h
    OpVarF _ h _ _ -> Set.singleton h
    EnumF _ h _ _ -> Set.singleton h
    OpF _ _ h _ _ _ _ -> Set.singleton h
    rest -> foldr Set.union mempty rest
