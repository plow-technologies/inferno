{-# LANGUAGE TemplateHaskell #-}

module Inferno.Utils.QQ.Module where

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Writer (WriterT (..))
import Data.Data (Proxy (..), cast)
import Data.Generics.Aliases (extQ)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NEList
import Data.Text (pack)
import Inferno.Infer (closeOverType)
import Inferno.Module (buildPinnedQQModules)
import Inferno.Parse
  ( QQDefinition (..),
    modulesParser,
    topLevel,
  )
import Inferno.Types.Syntax (CustomType)
import qualified Inferno.Types.Type as Type
import Inferno.Utils.QQ.Common
  ( liftText,
    location',
    mkParseErrorStr,
  )
import qualified Language.Haskell.TH.Lib as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..), dataToExpQ)
import Language.Haskell.TH.Syntax (mkName)
import Text.Megaparsec
  ( ParseErrorBundle (ParseErrorBundle),
    PosState (PosState),
    State (State),
    attachSourcePos,
    defaultTabWidth,
    errorOffset,
    runParser',
  )

mkProxy :: a -> Proxy a
mkProxy _ = Proxy

metaToValue :: (Maybe Type.TCScheme, QQDefinition) -> Maybe TH.ExpQ
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
    { quoteExp = \str -> do
        l <- location'
        let (_, res) =
              runParser' (runWriterT $ flip runReaderT (mempty, mempty, customTypes) $ topLevel modulesParser) $
                State
                  (pack str)
                  0
                  (PosState (pack str) 0 l defaultTabWidth "")
                  []
        case res of
          Left (ParseErrorBundle errs pos) ->
            let errs' = map mkParseErrorStr $ NEList.toList $ fst $ attachSourcePos errorOffset errs pos
             in fail $ intercalate "\n\n" errs'
          Right (modules, _comments) ->
            [|buildPinnedQQModules $(dataToExpQ ((fmap liftText . cast) `extQ` metaToValue) modules)|],
      quotePat = error "moduleQuoter: Invalid use of this quasi-quoter in pattern context.",
      quoteType = error "moduleQuoter: Invalid use of this quasi-quoter in type context.",
      quoteDec = error "moduleQuoter: Invalid use of this quasi-quoter in top-level declaration context."
    }
