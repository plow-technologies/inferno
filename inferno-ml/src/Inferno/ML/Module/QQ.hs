{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Inferno.ML.Module.QQ (preludeQuoter) where

import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class (MonadIO)
import Inferno.ML.Types.Value (MlValue, mlTypes)
import Inferno.Module (Prelude (..), buildPinnedQQModules)
import Inferno.Module.Prelude (builtinPrelude)
import Inferno.Parse (OpsTable, TopLevelDefn (..))
import Inferno.Types.Syntax
  ( Expr,
    ModuleName,
    TCScheme (..),
  )
import Inferno.Types.Value (ImplEnvM, Value)
import Inferno.Utils.QQ.Module (modulesToExpQ, parseAndMakePrelude)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Prettyprinter (Pretty)
import Text.Megaparsec (SourcePos)

buildPrelude ::
  (MonadIO m, MonadThrow m, MonadCatch m, Pretty c, Eq c) =>
  [(ModuleName, OpsTable, [TopLevelDefn (Either (TCScheme, Value c (ImplEnvM m c)) (Maybe TCScheme, Expr () SourcePos))])] ->
  Prelude m c
buildPrelude = buildPinnedQQModules builtinPrelude

-- | QuasiQuoter for building a prelude that builds on top of the builtin Inferno prelude.
preludeQuoter :: QuasiQuoter
preludeQuoter =
  QuasiQuoter
    { quoteExp = \str -> do
        modules <- parseAndMakePrelude @MlValue (builtinPrelude @IO @MlValue) mlTypes str
        [|buildPrelude $(modulesToExpQ modules)|],
      quotePat = error "preludeQuoter: Invalid use of this quasi-quoter in pattern context.",
      quoteType = error "preludeQuoter: Invalid use of this quasi-quoter in type context.",
      quoteDec = error "preludeQuoter: Invalid use of this quasi-quoter in top-level declaration context."
    }
