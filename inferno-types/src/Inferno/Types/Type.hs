{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Inferno.Types.Type
  ( BaseType (..),
    ImplType (..),
    Namespace (..),
    TCScheme (..),
    TV (..),
    InfernoType (..),
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
    typeTimeDiff,
    typeTime,
    punctuate',
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.List (intercalate)
import Data.Map (Map, empty)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
-- import Data.String (IsString)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Inferno.Types.Syntax (BaseType (..), Expr, ExtIdent (..), Ident (..), InfernoType (..), ModuleName (..), TV (..), punctuate')
import Inferno.Utils.Prettyprinter (renderPretty)
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    align,
    comma,
    enclose,
    encloseSep,
    hsep,
    lbrace,
    lparen,
    rbrace,
    rparen,
    -- sep,
    -- tupled,
    (<+>),
  )

data ImplType = ImplType (Map ExtIdent InfernoType) InfernoType
  deriving (Show, Eq, Ord, Data, Generic, ToJSON, FromJSON)

data Scheme = Forall [TV] ImplType
  deriving (Show, Eq, Ord, Data, Generic, ToJSON, FromJSON)

typeInt, typeBool, typeDouble, typeWord16, typeWord32, typeWord64, typeText, typeTimeDiff, typeTime :: InfernoType
typeInt = TBase TInt
typeBool = TBase $ TEnum "bool" $ Set.fromList ["true", "false"]
typeDouble = TBase TDouble
typeWord16 = TBase TWord16
typeWord32 = TBase TWord32
typeWord64 = TBase TWord64
typeText = TBase TText
typeTimeDiff = TBase TTimeDiff
typeTime = TBase TTime

sch :: InfernoType -> Scheme
sch = Forall [] . ImplType empty

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

data TCScheme = ForallTC [TV] (Set TypeClass) ImplType
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
  pretty (ForallTC _ tcs (ImplType impl ty))
    | Map.null impl && null tcs = pretty ty
    | otherwise =
        encloseSep
          lbrace
          rbrace
          comma
          ( (map (("requires" <+>) . pretty) $ Set.toList tcs)
              ++ (map (\(ExtIdent idt, t) -> "implicit" <+> case idt of { Left i -> "var$" <> pretty i; Right i -> pretty i } <+> ":" <+> align (pretty t)) $ Map.toList impl)
          )
          <+> "⇒"
          <+> pretty ty

newtype Subst = Subst (Map.Map TV InfernoType)
  deriving stock (Eq, Ord)
  deriving newtype (Semigroup, Monoid)

instance Show Subst where
  show (Subst m) = intercalate "\n" $ map (\(x, t) -> unpack $ renderPretty x <> " ~> " <> renderPretty t) $ Map.toList m

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TV

instance Substitutable InfernoType where
  apply _ (TBase a) = TBase a
  apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2
  apply s (TArray t) = TArray $ apply s t
  apply s (TSeries t) = TSeries $ apply s t
  apply s (TOptional t) = TOptional $ apply s t
  apply s (TTuple ts) = TTuple $ fmap (apply s) ts
  apply s (TRep t) = TRep $ apply s t

  ftv TBase {} = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2
  ftv (TArray t) = ftv t
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

data TypeMetadata ty = TypeMetadata
  { identExpr :: Expr () (),
    docs :: Maybe Text,
    ty :: ty
  }
  deriving (Eq, Show, Generic, Data, ToJSON, FromJSON)
