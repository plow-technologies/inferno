{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Catch (MonadCatch, SomeException, try)
import Data.Bifunctor (bimap)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as Text
import Inferno.Eval (TermEnv, runEvalIO)
import Inferno.Eval.Error (EvalError)
import Inferno.Infer (inferExpr, inferTypeReps)
import Inferno.Infer.Pinned (pinExpr)
import Inferno.Module (Module (..))
import Inferno.Module.Builtin (builtinModule)
import Inferno.Module.Prelude (ModuleMap, baseOpsTable, builtinModules, builtinModulesOpsTable, builtinModulesPinMap, builtinModulesTerms)
import Inferno.Parse (parseExpr)
import Inferno.Types.Syntax (Expr (..), Lit (..), collectArrs, Ident, ModuleName (ModuleName), Scoped (LocalScope), ImplExpl (..), ExtIdent (..))
import Inferno.Types.Type (ImplType (ImplType), TCScheme (..), Namespace (FunNamespace))
import Inferno.Types.Value (ImplEnvM, Value (..), runImplEnvM)
import Inferno.Types.VersionControl (VCObjectHash (..), pinnedToMaybe)
import Inferno.Utils.Prettyprinter (showPretty)
import System.Environment (getArgs)
import qualified Language.Haskell.TH as TH
import qualified GHC as GHC
import GHC.Driver.Session (defaultFatalMessager, defaultFlushOut, PackageDBFlag (..), PkgDbRef (..), PackageFlag (ExposePackage), PackageArg (..), ModRenaming (..))
import GHC.Unit.Types (Unit (..), stringToUnit)
import GHC.Paths (libdir)
import Unsafe.Coerce (unsafeCoerce)
import System.Directory (doesFileExist)
import Data.Typeable as Typ
import System.IO (openFile, hPutStr, hFlush, hClose, IOMode (WriteMode))
import Control.Monad.IO.Class (liftIO)

-- | Compile an Inferno expression to a TH expression
compile :: Expr hash pos -> TH.ExpQ
compile = \case
    Lit p (LInt i) -> [| i |]
    Lit p (LDouble i) -> [| i |]
    -- TODO translate Mod.foo
    -- Var _ _ (Scope s) v ->
    -- Var _ _ LocalScope (Impl i) -> -- TODO compile before type reps introduced?
    -- Var _ _ LocalScope (Expl (Left i)) ->
    Var _ _ LocalScope (Expl (ExtIdent (Right x))) -> [| x |]
    -- OpVar _ _ m i ->
    TypeRep _ _ -> error "type reps unsupported by compilation"
    -- Enum _ _ m i -> -- pre-pass extract enums, make ADT defs, keep mapping in context
    App e1 e2 -> do
      e1' <- compile e1
      e2' <- compile e2
      return $ TH.AppE e1' e2'
    -- Lam _ args _ e ->

    -- LetTuple p1 ((p, x) NEList.:| []) p2 e1 p3 e2 ->
    -- LetTuple p1 xs p2 e1 p3 e2 ->
    -- Let p1 p2 i p3 e1 p4 e2 ->
    -- If p1 e1 p2 e2 p3 e3 ->
    -- Op e1 p1 h f m i e2 ->
    -- PreOp p1 h f m i e ->
    -- Tuple p1 es p2 ->
    -- One p e ->
    -- Assert p1 e1 p2 e2 ->
    -- Case p1 e1 p2 cases p3 ->
    -- Array p1 elems p2 ->
    -- ArrayComp p1 e1 p2 froms ifE p3 ->
    -- CommentAbove c e ->
    -- CommentAfter e c ->
    -- CommentBelow e c ->
    -- Bracketed p1 e p2 ->
    -- RenameModule p1 m1 p2 m2 p3 e ->
    -- OpenModule p1 h m is p2 e ->
    -- TODO non-recursive constructors
    -- TODO InterpolatedString?
    -- e -> [| () |]
    e -> error $ "Unsupported expression: " ++ show (bimap (const ()) (const ()) e)

main :: IO ()
main = do
  foo <- compileHaskell "Test.hs" "Test" "foo" (Proxy :: Proxy String)
  print foo

  -- (doesFileExist "Test.hs") >>= \case
  --   -- False -> do
  --   _ -> do
  --     infernoFile <- head <$> getArgs
  --     let haskellFile = "Test.hs"
  --     let modName = "Test"
  --     let funcName = "myFunc" -- TODO make arg of compileInferno

  --     compileInferno infernoFile haskellFile
  --     func <- compileHaskell haskellFile modName funcName (Proxy :: Proxy Double)
  --       -- func "Hello"
  --     print func
  --   -- True -> error "TODO just run the compiled code here"


compileInferno :: FilePath -> FilePath -> IO ()
compileInferno sourceFile targetFile = do
  src <- Text.readFile sourceFile

  -- Parse, typecheck, and get inferno expr
  expr <- case parseExpr (baseOpsTable prelude) (builtinModulesOpsTable prelude) src of
    Left err -> error $ show err
    Right (ast, _comments) -> do
      showPretty ast
      -- pin free variables to builtin prelude function hashes
      case pinExpr (builtinModulesPinMap prelude) ast of
        Left err -> error $ show err
        Right pinnedAST -> do
          -- typecheck
          case inferExpr prelude pinnedAST of
            Left err -> error $ show err
            Right (pinnedAST', sch@(ForallTC _ _ (ImplType _ typ)), _tyMap) -> do
              let sig = collectArrs typ
              let outTy = last sig
              let inTys = init sig
              -- infer runtime type-reps
              case inferTypeReps allClasses sch inTys outTy of
                Left err -> error $ show err
                Right runtimeReps -> do
                  let finalAst =
                        foldl
                          App
                          (bimap pinnedToMaybe (const ()) pinnedAST')
                          [TypeRep () ty | ty <- runtimeReps]
                  return finalAst
              -- return pinnedAST'

  -- Compile inferno to haskell and save to targetFile
  showPretty expr
  let thExpr = compile expr
  thExpr' <- TH.runQ thExpr
  let interpStr = TH.pprint thExpr'
      -- typeTypeRep = Typ.typeOf (undefined :: a)
      typeTypeRep = Typ.typeOf (undefined :: Double) -- TODO
  h <- openFile targetFile WriteMode
  hPutStr h (unlines
        [ "module Test where"
        -- , "import Prelude"
        -- , "import Language.Haskell.TH"
        -- , "import GHC.Num"
        -- , "import GHC.Base"
        , ""
        , "myFunc :: " ++ show typeTypeRep
        , "myFunc = " ++ interpStr] )
  hFlush h
  hClose h

  where
    prelude :: ModuleMap (ExceptT EvalError IO) ()
    prelude = builtinModules

    allClasses = Set.unions $ moduleTypeClasses builtinModule : [cls | Module {moduleTypeClasses = cls} <- Map.elems prelude]

compileHaskell :: forall a. Typeable a => String -> String -> String -> Proxy a -> IO a -- (Either String a)
compileHaskell fileName modName funcName _ = do
  GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    GHC.runGhc (Just libdir) $ do
      dflags <- GHC.getSessionDynFlags
      let mkPackage = \p -> ExposePackage p (PackageArg p) (ModRenaming True [])
      let dflags' = dflags {
        GHC.packageDBFlags = reverse $ map (PackageDB . PkgDbPath) [
            "/Users/sid/.cabal/store/ghc-9.2.5/package.db"
          -- , "/Users/sid/code/inferno/dist-newstyle/packagedb/ghc-9.2.5"
          -- , "/Users/sid/code/inferno/dist-newstyle/build/x86_64-osx/ghc-9.2.5/inferno-core-0.1.3/x/inferno/package.conf.inplace"
        ]
        , GHC.packageFlags = reverse [
            (\p -> ExposePackage p (PackageArg p) (ModRenaming True [])) "base-4.16.4.0"
          , (\p -> ExposePackage p (UnitIdArg (stringToUnit p)) (ModRenaming True [])) "txt-2.0.2-8d2cc470"
        ]
        -- , GHC.packageFlags = reverse $ map (mkPackage) [
        --     "base-4.16.4.0"
        --   -- , "containers-0.6.5.1"
        --   -- , "directory-1.3.6.2"
        --   -- , "exceptions-0.10.4"
        --   -- , "ghc-9.2.5"
        --   -- , "ghc-pths-0.1.0.12-ec67cc55"
        --   -- , "inferno-core-0.1.3-inplace"
        --   -- , "inferno-types-0.1.2-inplace"
        --   -- , "mtl-2.2.2"
        --   -- , "template-haskell-2.18.0.0"
        --   -- , "txt-2.0.2-8d2cc470"
        -- ]
      }
      GHC.setSessionDynFlags dflags'
      target <- GHC.guessTarget fileName Nothing
      GHC.setTargets [target]
      f <- GHC.load GHC.LoadAllTargets
      case f of
        GHC.Failed -> error "Compilation failed"
        GHC.Succeeded -> do
          GHC.setContext  
            -- import qualified Module
            [ GHC.IIDecl $ 
              (GHC.simpleImportDecl . GHC.mkModuleName $ modName)
              {GHC.ideclQualified = GHC.QualifiedPre}
            ]
          fetched <- GHC.compileExpr (modName ++ "." ++ funcName)
          -- TODO try GHC.dynCompileExpr instead, so that you can avoid the unsafeCoerce. You must add a qualified import for Data.Dynamic in the context for it to work, but a Data.Dynamic.Dynamic value is generally nicer to work with, since you can handle type errors more gracefully.
          -- fetched <- GHC.dynCompileExpr $ moduleName ++ "." ++ externalFuncName
          -- return . fromDynamic (error "Illegal type cast") $ fetched
          -- return (unsafeCoerce fetched :: String -> IO ())
          return (unsafeCoerce fetched :: a)

-- Attempt to find the primitive definitions at runtime, but has almost as much (?)
-- overhead as interpreting?
  -- v <- foo
  -- showPretty v
  -- where
  --   prelude :: ModuleMap (ExceptT EvalError IO) ()
  --   prelude = builtinModules

  --   modules = map moduleObjects $ Map.elems prelude

  --   lookupFun :: Ident -> IO (Value () IO)
  --   lookupFun f = do
  --     let h = nameToHash Map.! (FunNamespace f)
  --     x <- try $ runExceptT $ runImplEnvM mempty $ implEnvM >>= \env -> lookupH env h
  --     case x of
  --       Left (e :: SomeException) -> error $ show e
  --       Right res -> case res of 
  --         Left err -> error $ show err
  --         Right res' -> return res'
  --     where
  --       (nameToHash, hashToType, implEnvM) = moduleObjects $ prelude Map.! "Base"
  --       lookupH env@(localEnv, pinnedEnv) h =
  --         case Map.lookup h pinnedEnv of
  --           Just v -> return v
  --           Nothing -> error $ "couldn't find in pinnedEnv: " <> show h

  --   foo :: IO (Value () IO)
  --   foo = do
  --     case prelude Map.! "Base" of
  --       VFun f -> do
  --         r <- f $ VTuple []
  --         return r
  --       _ -> error "expected VFun"

_main :: IO ()
_main = do
  file <- head <$> getArgs
  src <- Text.readFile file

  -- parse
  case parseExpr (baseOpsTable prelude) (builtinModulesOpsTable prelude) src of
    Left err -> print err
    Right (ast, _comments) -> do
      -- pin free variables to builtin prelude function hashes
      case pinExpr (builtinModulesPinMap prelude) ast of
        Left err -> print err
        Right pinnedAST -> do
          -- typecheck
          case inferExpr prelude pinnedAST of
            Left err -> print err
            Right (pinnedAST', sch@(ForallTC _ _ (ImplType _ typ)), _tyMap) -> do
              let sig = collectArrs typ
              let outTy = last sig
              let inTys = init sig
              -- infer runtime type-reps
              case inferTypeReps allClasses sch inTys outTy of
                Left err -> print err
                Right runtimeReps -> do
                  let finalAst =
                        foldl
                          App
                          (bimap pinnedToMaybe (const ()) pinnedAST')
                          [TypeRep () ty | ty <- runtimeReps]
                  -- evaluate
                  runEvalIO mkEnv mempty finalAst >>= \case
                    Left err -> print err
                    Right res -> showPretty res
  where
    prelude :: ModuleMap (ExceptT EvalError IO) ()
    prelude = builtinModules

    allClasses = Set.unions $ moduleTypeClasses builtinModule : [cls | Module {moduleTypeClasses = cls} <- Map.elems prelude]

    mkEnv :: ImplEnvM (ExceptT EvalError IO) () (TermEnv VCObjectHash () (ImplEnvM (ExceptT EvalError IO) ()))
    mkEnv = do
      pinnedEnv <- snd <$> (builtinModulesTerms builtinModules)
      pure (mempty, pinnedEnv)
