{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Inferno.Utils.QQ.Module where

import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (runStateT)
import Data.Data (Proxy (..), cast)
import Data.Generics.Aliases (extQ)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NEList
import Data.Text (Text)
import qualified Data.Text as Text
import Inferno.Infer (closeOverType)
import Inferno.Module (buildPinnedQQModules)
import Inferno.Parse
  ( InfernoParsingError,
    ParseEnv,
    ParsedModule,
    QQDefinition (..),
    mkParseEnv,
    modulesParser,
    topLevel,
  )
import Inferno.Types.Syntax (Comment, CustomType, TCScheme)
import Inferno.Utils.QQ.Common
  ( liftText,
    location',
    mkParseErrorStr,
  )
import qualified Language.Haskell.TH.Lib as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..), dataToExpQ)
import Language.Haskell.TH.Syntax (Exp, Q, mkName)
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

mkProxy :: a -> Proxy a
mkProxy _ = Proxy

metaToValue :: (Maybe TCScheme, QQDefinition) -> Maybe TH.ExpQ
metaToValue = \case
  (Just sch, QQToValueDef x) -> Just [|Left ($(dataToExpQ (fmap liftText . cast) sch), toValue $(TH.varE (mkName x)))|]
  (Nothing, QQToValueDef x) ->
    Just [|Left (closeOverType (toType (mkProxy $(TH.varE (mkName x)))), toValue $(TH.varE (mkName x)))|]
  (Just sch, QQRawDef x) -> Just [|Left ($(dataToExpQ (fmap liftText . cast) sch), $(TH.varE (mkName x)))|]
  (Nothing, QQRawDef _) -> error "QQRawDef must have an explicit type"
  (sch, InlineDef e) ->
    Just [|Right ($(dataToExpQ (fmap liftText . cast) sch), $(dataToExpQ (fmap liftText . cast) e))|]

-- | QuasiQuoter for builtin Inferno modules. TH dictates that QQs have to be imported,
-- not defined locally, so this instantiation is done in this module.
infernoModules :: QuasiQuoter
infernoModules = moduleQuoter []

moduleQuoter :: [CustomType] -> QuasiQuoter
moduleQuoter customTypes =
  QuasiQuoter
    { quoteExp = \(Text.pack -> str) -> do
        location' >>= \l ->
          let env :: ParseEnv
              env = mkParseEnv mempty mempty customTypes

              run ::
                Parsec
                  InfernoParsingError
                  Text
                  ([ParsedModule (Maybe TCScheme, QQDefinition)], [Comment SourcePos])
              run = flip runStateT mempty . flip runReaderT env $ topLevel modulesParser

              res ::
                Either
                  (ParseErrorBundle Text InfernoParsingError)
                  ([ParsedModule (Maybe TCScheme, QQDefinition)], [Comment SourcePos])
              (_, res) =
                runParser' run $
                  State str 0 (PosState str 0 l defaultTabWidth mempty) mempty
          in either parseFail liftModules res
    , quotePat = error "moduleQuoter: Invalid use of this quasi-quoter in pattern context."
    , quoteType = error "moduleQuoter: Invalid use of this quasi-quoter in type context."
    , quoteDec = error "moduleQuoter: Invalid use of this quasi-quoter in top-level declaration context."
    }
  where
    parseFail :: ParseErrorBundle Text InfernoParsingError -> Q Exp
    parseFail (ParseErrorBundle errs pos) =
      fail
        . intercalate "\n\n"
        . fmap mkParseErrorStr
        . NEList.toList
        . fst
        $ attachSourcePos errorOffset errs pos

    liftModules :: ([ParsedModule (Maybe TCScheme, QQDefinition)], [Comment SourcePos]) -> Q Exp
    liftModules (modules, _) =
      [|buildPinnedQQModules $(dataToExpQ ((fmap liftText . cast) `extQ` metaToValue) modules)|]
