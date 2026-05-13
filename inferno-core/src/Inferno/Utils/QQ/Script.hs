{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Inferno.Utils.QQ.Script where

import Control.Monad ((>=>))
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (runStateT)
import qualified Crypto.Hash as Crypto
import Data.ByteArray (convert)
import qualified Data.ByteString as ByteString
import Data.Data (cast)
import Data.Generics.Aliases (extQ)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NEList
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Inferno.Infer (inferExpr)
import Inferno.Infer.Pinned (pinExpr)
import Inferno.Module.Prelude
  ( ModuleMap,
    baseOpsTable,
    builtinModules,
    builtinModulesOpsTable,
    builtinModulesPinMap,
  )
import Inferno.Parse (InfernoParsingError, ParseEnv, expr, mkParseEnv, topLevel)
import Inferno.Parse.Commented (insertCommentsIntoExpr)
import Inferno.Types.Syntax (Comment, Expr, TCScheme)
import Inferno.Types.VersionControl (Pinned, VCObjectHash)
import Inferno.Utils.QQ.Common
  ( liftText,
    location',
    mkParseErrorStr,
  )
import qualified Language.Haskell.TH.Lib as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..), dataToExpQ)
import Language.Haskell.TH.Syntax (Exp (AppE, VarE), Lift (lift), Q)
import Prettyprinter (Pretty)
import Text.Megaparsec
  ( ParseErrorBundle (ParseErrorBundle),
    Parsec,
    PosState (PosState),
    SourcePos,
    State (State),
    attachSourcePos,
    defaultTabWidth,
    errorOffset,
    runParser',
  )

inferno ::
  forall m c.
  (MonadIO m, MonadThrow m, MonadCatch m, Pretty c, Eq c) =>
  QuasiQuoter
inferno =
  QuasiQuoter
    { quoteExp = \str ->
        location' >>= \l ->
          let env :: ParseEnv
              env =
                mkParseEnv
                  (baseOpsTable @_ @c builtins)
                  (builtinModulesOpsTable @_ @c builtins)
                  mempty

              run :: Parsec InfernoParsingError Text (Expr () SourcePos, [Comment SourcePos])
              run = flip runStateT mempty . flip runReaderT env $ topLevel expr

              res ::
                Either
                  (ParseErrorBundle Text InfernoParsingError)
                  (Expr () SourcePos, [Comment SourcePos])
              (_, res) =
                runParser' run $
                  State
                    (Text.pack str)
                    0
                    (PosState (Text.pack str) 0 l defaultTabWidth mempty)
                    mempty
           in either parseFail (pinAndInfer >=> liftToTH) res
    , quotePat = error "inferno: Invalid use of this quasi-quoter in pattern context."
    , quoteType = error "inferno: Invalid use of this quasi-quoter in type context."
    , quoteDec = error "inferno: Invalid use of this quasi-quoter in top-level declaration context."
    }
  where
    builtins :: ModuleMap m c
    builtins = builtinModules @m @c

    parseFail :: ParseErrorBundle Text InfernoParsingError -> Q Exp
    parseFail (ParseErrorBundle errs pos) =
      fail
        . intercalate "\n\n"
        . fmap mkParseErrorStr
        . NEList.toList
        . fst
        $ attachSourcePos errorOffset errs pos

    pinAndInfer ::
      (Expr () SourcePos, [Comment SourcePos]) -> Q (Expr (Pinned VCObjectHash) SourcePos, TCScheme)
    pinAndInfer (ast, comments) = do
      pinned <-
        either (fail . ("Pinning expression failed:\n" <>) . show) pure $
          pinExpr (builtinModulesPinMap @_ @c builtins) ast
      (pinnedAST, t, _) <-
        either (fail . ("Inference failed:\n" <>) . show) pure $
          inferExpr builtins pinned
      pure (insertCommentsIntoExpr (reverse comments) pinnedAST, t)

    liftToTH :: (Expr (Pinned VCObjectHash) SourcePos, TCScheme) -> Q Exp
    liftToTH = dataToExpQ ((fmap liftText . cast) `extQ` vcObjectHashToValue)

    vcObjectHashToValue :: Crypto.Digest Crypto.SHA256 -> Maybe TH.ExpQ
    vcObjectHashToValue h =
      Just $
        AppE (VarE 'Maybe.fromJust)
          . AppE (VarE 'Crypto.digestFromByteString)
          <$> lift (ByteString.unpack (convert h))
