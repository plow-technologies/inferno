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
  (Just sch, QQToValueDef x) -> Just [|Left ($(dataToExpQ (\a -> liftText <$> cast a) sch), toValue $(TH.varE (mkName x)))|]
  (Nothing, QQToValueDef x) ->
    Just [|Left (closeOverType (toType (mkProxy $(TH.varE (mkName x)))), toValue $(TH.varE (mkName x)))|]
  (Just sch, QQRawDef x) -> Just [|Left ($(dataToExpQ (\a -> liftText <$> cast a) sch), pure $(TH.varE (mkName x)))|]
  (Nothing, QQRawDef _) -> error "QQRawDef must have an explicit type"
  (sch, InlineDef e) ->
    Just [|Right ($(dataToExpQ (\a -> liftText <$> cast a) sch), $(dataToExpQ (\a -> liftText <$> cast a) e))|]

infernoModules :: QuasiQuoter
infernoModules =
  QuasiQuoter
    { quoteExp = \str -> do
        l <- location'
        let (_, res) =
              runParser' (runWriterT $ flip runReaderT (mempty, mempty) $ topLevel $ modulesParser) $
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
            [|buildPinnedQQModules $(dataToExpQ ((\a -> liftText <$> cast a) `extQ` metaToValue) modules)|],
      quotePat = error "infernoModule: Invalid use of this quasi-quoter in pattern context.",
      quoteType = error "infernoModule: Invalid use of this quasi-quoter in type context.",
      quoteDec = error "infernoModule: Invalid use of this quasi-quoter in top-level declaration context."
    }
