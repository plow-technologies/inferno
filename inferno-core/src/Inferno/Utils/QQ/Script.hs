{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Inferno.Utils.QQ.Script where

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Writer (WriterT (..), appEndo)
import qualified Crypto.Hash as Crypto
import Data.ByteArray (convert)
import Data.ByteString (ByteString, unpack)
import Data.Data (cast)
import Data.Generics.Aliases (extQ)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NEList
import qualified Data.Maybe as Maybe
import Data.Text (pack)
import Inferno.Eval.Error (EvalError)
import Inferno.Infer (inferExpr)
import Inferno.Infer.Pinned (pinExpr)
import Inferno.Module.Prelude (baseOpsTable, builtinModules, builtinModulesOpsTable, builtinModulesPinMap)
import Inferno.Parse (expr, topLevel)
import Inferno.Parse.Commented (insertCommentsIntoExpr)
import Inferno.Utils.QQ.Common
  ( liftText,
    location',
    mkParseErrorStr,
  )
import qualified Language.Haskell.TH.Lib as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..), dataToExpQ)
import Language.Haskell.TH.Syntax (Exp (AppE, VarE), Lift (lift))
import Prettyprinter (Pretty)
import Text.Megaparsec
  ( ParseErrorBundle (ParseErrorBundle),
    PosState (PosState),
    State (State),
    attachSourcePos,
    defaultTabWidth,
    errorOffset,
    runParser',
  )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (MonadError)

inferno :: forall m c. (MonadIO m, MonadError EvalError m, Pretty c, Eq c) => QuasiQuoter
inferno =
  QuasiQuoter
    { quoteExp = \str -> do
        l <- location'
        let (_, res) =
              runParser' (runWriterT $ flip runReaderT (baseOpsTable @_ @c builtins, builtinModulesOpsTable @_ @c builtins) $ topLevel $ expr) $
                State
                  (pack str)
                  0
                  (PosState (pack str) 0 l defaultTabWidth "")
                  []
        case res of
          Left (ParseErrorBundle errs pos) ->
            let errs' = map mkParseErrorStr $ NEList.toList $ fst $ attachSourcePos errorOffset errs pos
             in fail $ intercalate "\n\n" errs'
          Right (ast, comments) ->
            case pinExpr (builtinModulesPinMap @_ @c builtins) ast of
              Left err -> fail $ "Pinning expression failed:\n" <> show err
              Right pinnedAST ->
                case inferExpr builtins pinnedAST of
                  Left err -> fail $ "Inference failed:\n" <> show err
                  Right (pinnedAST', t, _tyMap) -> do
                    let final = insertCommentsIntoExpr (appEndo comments []) pinnedAST'
                    dataToExpQ ((\a -> liftText <$> cast a) `extQ` vcObjectHashToValue) (final, t),
      quotePat = error "inferno: Invalid use of this quasi-quoter in pattern context.",
      quoteType = error "inferno: Invalid use of this quasi-quoter in type context.",
      quoteDec = error "inferno: Invalid use of this quasi-quoter in top-level declaration context."
    }
  where
    builtins = builtinModules @(m) @c
    vcObjectHashToValue :: Crypto.Digest Crypto.SHA256 -> Maybe TH.ExpQ
    vcObjectHashToValue h =
      let str = (convert h) :: ByteString
       in Just $
            ( AppE (VarE 'Maybe.fromJust)
                <$> (AppE (VarE 'Crypto.digestFromByteString) <$> lift (unpack str))
            )
