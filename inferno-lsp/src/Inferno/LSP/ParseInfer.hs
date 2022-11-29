{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Inferno.LSP.ParseInfer where

import Control.Monad (forM_)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Either (isLeft)
import Data.List (find, findIndices, nub, sort)
import qualified Data.List.NonEmpty as NEList
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Inferno.Eval.Error (EvalError)
import Inferno.Infer (TypeError (..), closeOverType, findTypeClassWitnesses, inferExpr, inferPossibleTypes, inferTypeReps)
import Inferno.Infer.Env (Namespace (..))
import Inferno.Infer.Pinned (pinExpr)
import Inferno.Module (Module (..))
import Inferno.Module.Builtin (builtinModule)
import Inferno.Module.Prelude (ModuleMap, baseOpsTable, builtinModulesOpsTable, builtinModulesPinMap)
import Inferno.Parse (InfernoParsingError, parseExpr, parseType)
import Inferno.Parse.Commented (insertCommentsIntoExpr)
import Inferno.Parse.Error (prettyError)
import Inferno.Types.Syntax (Comment, Expr (..), ExtIdent (..), Ident (..), ModuleName (..), Scoped (..), getIdentifierPositions, hideInternalIdents)
import Inferno.Types.Type
  ( BaseType (..),
    ImplType (ImplType),
    InfernoType (..),
    TCScheme (..),
    TypeClass (..),
    TypeClassShape (..),
    TypeMetadata (..),
    apply,
    ftv,
    punctuate',
  )
import Inferno.Types.VersionControl (Pinned (..), VCObjectHash)
import Inferno.Utils.Prettyprinter (renderDoc, renderPretty)
import Language.LSP.Types
  ( Diagnostic (..),
    DiagnosticSeverity (DsError),
    MarkupContent (..),
    MarkupKind (MkMarkdown),
    Range,
    mkRange,
  )
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    align,
    hardline,
    indent,
    sep,
    vsep,
    (<+>),
  )
import qualified Safe
import Text.Megaparsec.Error (ParseError, ShowErrorComponent)
import Text.Megaparsec.Pos (SourcePos (..), unPos)

parseExprInBaseModule ::
  forall m c.
  (MonadError EvalError m, Pretty c, Eq c) =>
  ModuleMap m c ->
  Text ->
  Either
    (NEList.NonEmpty (ParseError Text InfernoParsingError, SourcePos))
    (Expr () SourcePos, [Comment SourcePos])
parseExprInBaseModule prelude =
  parseExpr (baseOpsTable prelude) (builtinModulesOpsTable prelude)

errorDiagnostic :: Int -> Int -> Int -> Int -> Maybe Text -> Text -> Diagnostic
errorDiagnostic s_line s_col e_line e_col source message =
  Diagnostic
    { _range = mkRange (fromIntegral s_line - 2) (fromIntegral s_col - 1) (fromIntegral e_line - 2) (fromIntegral e_col - 1),
      _severity = Just DsError,
      _code = Nothing,
      _source = source,
      _message = message,
      _tags = Nothing,
      _relatedInformation = Nothing
    }

parseErrorDiagnostic :: ShowErrorComponent e => (ParseError Text e, SourcePos) -> Diagnostic
parseErrorDiagnostic (err, SourcePos _ l c) =
  errorDiagnostic
    0
    0
    (unPos l - 1)
    (unPos c)
    (Just "inferno.parser")
    (pack $ prettyError err)

errorDiagnosticInfer :: Int -> Int -> Int -> Int -> Text -> Diagnostic
errorDiagnosticInfer s_line s_col e_line e_col = errorDiagnostic s_line s_col e_line e_col (Just "inferno.infer")

inferErrorDiagnostic :: TypeError SourcePos -> [Diagnostic]
inferErrorDiagnostic = \case
  UnificationFail _ t1 t2 (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ vsep
          [ "Could not match the type",
            indent 2 (pretty $ closeOverType t1),
            "with",
            indent 2 (pretty $ closeOverType t2)
          ]
    ]
  ExpectedFunction _ t1 t2 (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ vsep
          [ "Expected a function here of type",
            indent 2 (pretty $ closeOverType t1),
            "but instead found",
            indent 2 (pretty $ closeOverType t2)
          ]
    ]
  InfiniteType tv t (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ "Could not unify" <+> pretty tv <+> "~" <+> (pretty $ closeOverType t)
    ]
  UnboundExtIdent modNm v (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ "Unbound variable '"
          <> ( case modNm of
                 LocalScope -> ""
                 Scope (ModuleName nm) -> pretty nm <> "."
             )
          <> pretty v
          <> "'"
    ]
  UnboundNameInNamespace modNm (Right n) (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ case n of
          FunNamespace (Ident v) ->
            "Unbound variable '"
              <> ( case modNm of
                     LocalScope -> ""
                     Scope (ModuleName nm) -> nm <> "."
                 )
              <> v
              <> "'"
          OpNamespace (Ident v) ->
            "Unbound operator '"
              <> ( case modNm of
                     LocalScope -> ""
                     Scope (ModuleName nm) -> nm <> "."
                 )
              <> v
              <> "'"
          ModuleNamespace (ModuleName v) ->
            "Module '"
              <> ( case modNm of
                     LocalScope -> ""
                     Scope (ModuleName nm) -> nm <> "."
                 )
              <> v
              <> "' could not be found."
          TypeNamespace (Ident v) ->
            "Type '"
              <> ( case modNm of
                     LocalScope -> ""
                     Scope (ModuleName nm) -> nm <> "."
                 )
              <> v
              <> "' could not be found."
          EnumNamespace (Ident c) ->
            renderDoc $
              vsep
                [ "Could not find the enum constructor '#"
                    <> ( case modNm of
                           LocalScope -> mempty
                           Scope (ModuleName nm) -> pretty nm <> "."
                       )
                    <> pretty c
                    <> "'.",
                  "Make sure the enum you are trying to use has been imported"
                ]
    ]
  UnboundNameInNamespace _modNm (Left h) (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ "Object with has '"
          <> Text.pack (show h)
          <> "' could not be found."
    ]
  ImplicitVarTypeOverlap _ (ExtIdent ident) t1 t2 (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ vsep
          [ "The implicit variable '" <> case ident of { Left i -> "var$" <> pretty i; Right i -> pretty i } <> "' has multiple types:",
            indent 2 (pretty $ closeOverType $ t1),
            "and",
            indent 2 (pretty $ closeOverType $ t2)
          ]
    ]
  VarMultipleOccurrence (Ident x) (s2, e2) (s1, e1) ->
    let message =
          renderDoc $
            vsep
              [ "Duplicate declarations of '" <> pretty x <> "' in the pattern match",
                "at line" <+> pretty (unPos $ sourceLine s1) <> "," <+> "column" <+> pretty (unPos $ sourceColumn s1),
                "and line" <+> pretty (unPos $ sourceLine s2) <> "," <+> "column" <+> pretty (unPos $ sourceColumn s2)
              ]
     in [ errorDiagnosticInfer
            (unPos $ sourceLine s1)
            (unPos $ sourceColumn s1)
            (unPos $ sourceLine e1)
            (unPos $ sourceColumn e1)
            $ message,
          errorDiagnosticInfer
            (unPos $ sourceLine s2)
            (unPos $ sourceColumn s2)
            (unPos $ sourceLine e2)
            (unPos $ sourceColumn e2)
            $ message
        ]
  IfConditionMustBeBool _ t (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ vsep
          [ "The type of the if condition was expected to be a bool, instead I found",
            indent 2 (pretty $ closeOverType t)
          ]
    ]
  AssertConditionMustBeBool _ t (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ vsep
          [ "The type of the assert condition was expected to be a bool, instead I found",
            indent 2 (pretty $ closeOverType t)
          ]
    ]
  IfBranchesMustBeEqType _ t1 t2 (s1, e1) (s2, e2) ->
    let message =
          renderDoc $
            vsep
              [ "The type of both branches of the if statement must be the same, however I found two different types:",
                indent 2 (pretty $ closeOverType t1),
                "and",
                indent 2 (pretty $ closeOverType t2)
              ]
     in [ errorDiagnosticInfer
            (unPos $ sourceLine s1)
            (unPos $ sourceColumn s1)
            (unPos $ sourceLine e1)
            (unPos $ sourceColumn e1)
            $ message,
          errorDiagnosticInfer
            (unPos $ sourceLine s2)
            (unPos $ sourceColumn s2)
            (unPos $ sourceLine e2)
            (unPos $ sourceColumn e2)
            $ message
        ]
  CaseBranchesMustBeEqType _ t1 t2 (s1, e1) (s2, e2) ->
    let message =
          renderDoc $
            vsep
              [ "The type of all case branches must be the same, but I found two different types:",
                indent 2 (pretty $ closeOverType t1),
                "and",
                indent 2 (pretty $ closeOverType t2)
              ]
     in [ errorDiagnosticInfer
            (unPos $ sourceLine s1)
            (unPos $ sourceColumn s1)
            (unPos $ sourceLine e1)
            (unPos $ sourceColumn e1)
            $ message,
          errorDiagnosticInfer
            (unPos $ sourceLine s2)
            (unPos $ sourceColumn s2)
            (unPos $ sourceLine e2)
            (unPos $ sourceColumn e2)
            $ message
        ]
  PatternUnificationFail tPat tE p (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ vsep
          [ "The type of the pattern does not match the case expression",
            "expected",
            indent 2 (pretty $ closeOverType tE),
            "but instead found",
            indent 2 (pretty p <+> ":" <+> align (pretty $ closeOverType tPat))
          ]
    ]
  PatternsMustBeEqType _ t1 t2 p1 p2 (s1, e1) (s2, e2) ->
    let message =
          renderDoc $
            vsep
              [ "The type of all case patterns must be the same, but I found two different types:",
                indent 2 (pretty p1 <+> ":" <+> align (pretty $ closeOverType t1)),
                "and",
                indent 2 (pretty p2 <+> ":" <+> align (pretty $ closeOverType t2))
              ]
     in [ errorDiagnosticInfer
            (unPos $ sourceLine s1)
            (unPos $ sourceColumn s1)
            (unPos $ sourceLine e1)
            (unPos $ sourceColumn e1)
            $ message,
          errorDiagnosticInfer
            (unPos $ sourceLine s2)
            (unPos $ sourceColumn s2)
            (unPos $ sourceLine e2)
            (unPos $ sourceColumn e2)
            $ message
        ]
  NonExhaustivePatternMatch pat (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ vsep
          [ "The patterns in this case expression are non-exhaustive.",
            "For example, the following pattern is missing:",
            indent 2 (pretty $ pat)
          ]
    ]
  UselessPattern (Just pat) (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ vsep
          [ "This case is unreachable, since it is subsumed by the previous pattern",
            indent 2 (pretty $ pat)
          ]
    ]
  UselessPattern Nothing (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ "This case is unreachable, since it is subsumed by the previous patterns"
    ]
  -- TypeClassUnificationError t1 t2 tcs (s, e) ->
  --   [ errorDiagnosticInfer
  --       (unPos $ sourceLine s)
  --       (unPos $ sourceColumn s)
  --       (unPos $ sourceLine e)
  --       (unPos $ sourceColumn e)
  --       $ renderDoc $
  --         vsep
  --           [ "Could not match the type",
  --             indent 2 (pretty $ closeOverType t1),
  --             "with",
  --             indent 2 (pretty $ closeOverType t2),
  --             "arising from this definition constraint:",
  --             indent 2 (pretty $ tcs)
  --           ]
  --   ]
  TypeClassNotFoundError _ tcls (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ vsep
          [ "Could not find any definitions for",
            indent 2 (pretty $ TypeClassShape tcls)
          ]
    ]
  TypeClassNoPartialMatch tcls (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ vsep
          [ "Could not find any matching definitions for",
            indent 2 (pretty $ TypeClassShape tcls)
          ]
    ]
  ModuleNameTaken (ModuleName m) (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ vsep
          [ "The chosen module name already exists:",
            indent 2 (pretty m)
          ]
    ]
  ModuleDoesNotExist (ModuleName m) (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ vsep
          [ "The following module does not exist:",
            indent 2 (pretty m),
            "make sure you have imported the module"
          ]
    ]
  NameInModuleDoesNotExist (ModuleName m) (Ident i) (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ vsep
          [ "The module",
            indent 2 (pretty m),
            "does not contain:",
            indent 2 (pretty i)
          ]
    ]
  AmbiguousName (ModuleName m) i (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ vsep
          [ "The name",
            indent 2 (pretty i),
            "you are trying to import from",
            indent 2 (pretty m),
            "already exists in local scope and would be overshadowed. Consider using:",
            indent 2 $ (pretty m) <> "." <> (pretty i)
          ]
    ]
  CouldNotFindTypeclassWitness tyCls (s, e) ->
    [ errorDiagnosticInfer
        (unPos $ sourceLine s)
        (unPos $ sourceColumn s)
        (unPos $ sourceLine e)
        (unPos $ sourceColumn e)
        $ renderDoc
        $ vsep
        $ "Could not find any matching assignment of types which satisfies the type class constraints:"
          : [ indent 2 (pretty c) | c <- Set.toList tyCls
            ]
    ]

allClasses :: forall m c. (MonadError EvalError m, Pretty c, Eq c) => ModuleMap m c -> Set.Set TypeClass
allClasses prelude = Set.unions $ moduleTypeClasses builtinModule : [cls | Module {moduleTypeClasses = cls} <- Map.elems prelude]

parseAndInfer :: forall m1 m2 c. (MonadError EvalError m1, Pretty c, Eq c, MonadIO m2) => ModuleMap m1 c -> [Maybe Ident] -> Text -> (InfernoType -> Either Text ()) -> m2 (Either [Diagnostic] (Expr (Pinned VCObjectHash) (), TCScheme, [(Range, MarkupContent)]))
parseAndInfer prelude idents txt validateInput = do
  let input = case idents of
        [] -> "\n" <> txt
        ids -> "fun " <> (Text.intercalate " " $ map (maybe "_" (\(Ident i) -> i)) ids) <> " -> \n" <> txt
  -- AppConfig _ _ tracer <- ask
  -- let trace = const $ pure () --traceWith tracer
  case (parseExprInBaseModule prelude) input of
    Left err -> do
      -- liftIO $ debugM "reactor.handle" $ "Parsing error: " ++ show err
      return $ Left $ fmap parseErrorDiagnostic $ NEList.toList err
    Right (ast, comments) -> do
      -- liftIO $ debugM "reactor.handle" $ "Finished parsing"
      case pinExpr (builtinModulesPinMap prelude) ast of
        Left err -> do
          -- liftIO $ debugM "reactor.handle" $ "Pinning error: " ++ show err
          return $ Left $ concatMap inferErrorDiagnostic $ Set.toList $ Set.fromList err
        Right pinnedAST -> do
          -- liftIO $ debugM "reactor.handle" $ "Finished pinning"
          case inferExpr prelude pinnedAST of
            Left err -> do
              -- liftIO $ debugM "reactor.handle" $ "Infer error: " ++ show err
              return $ Left $ concatMap inferErrorDiagnostic $ Set.toList $ Set.fromList err
            Right (pinnedAST', tcSch@(ForallTC _ currentClasses (ImplType _ typSig)), tyMap) -> do
              liftIO $ print typSig
              let possibleInputErrors = validateInputs typSig
              case find isLeft possibleInputErrors of
                Just (Left err) -> do
                  -- Get the idents that correspond to the indices
                  let indices = findIndices isLeft possibleInputErrors
                  let badIdent = listToMaybe $ catMaybes (mapMaybe (Safe.atMay idents) indices)
                  case badIdent of
                    Nothing -> return $ Left [errorDiagnosticInfer 0 0 0 0 $ renderPretty ["Could not find the input that caused the error. " `Text.append` err]]
                    Just ident@(Ident _i) -> do
                      let mIdentPos = listToMaybe $ getIdentifierPositions ident pinnedAST'
                      case mIdentPos of
                        Nothing -> return $ Left [errorDiagnosticInfer 0 0 0 2 $ renderDoc $ vsep ["Could not find the input that caused the error"]]
                        Just (s, e) -> return $ Left [errorDiagnosticInfer (unPos $ sourceLine s) (unPos $ sourceColumn s) (unPos $ sourceLine e) (unPos $ sourceColumn e) err]
                _ -> do
                  -- liftIO $ debugM "reactor.handle" $ "Finished inferring"
                  let final = insertCommentsIntoExpr comments pinnedAST'
                  return $
                    Right
                      ( fmap (const ()) final,
                        tcSch,
                        Map.foldrWithKey (\k v xs -> (mkHover prelude currentClasses k v) : xs) mempty tyMap
                      )
  where
    -- Example: The type signature has the format for 3 inputs:
    -- TArr
    --   Input1
    --   TArr
    --     Input2
    --     TArr
    --       Input3
    --       Output
    --
    -- Therefore we need to check the type of the first value in each TArr to see if it is a valid input type
    -- The final output value will be ignored
    validateInputs :: InfernoType -> [Either Text ()]
    validateInputs = \case
      -- Ignore validation if the final output is an array
      typSig | isOutputAnArray typSig -> [Right ()]
      -- Head is input, tail *could* be other inputs
      TArr h t | isTArr t -> validateInput h : validateInputs t
      -- Tail of this would be the output
      TArr h _t | not (isTArr h) -> [validateInput h]
      -- Top level is not an arr, assume it is correct to prevent errors
      _ -> [Right ()]

    isTArr :: InfernoType -> Bool
    isTArr = \case
      TArr _ _ -> True
      _ -> False

    isOutputAnArray :: InfernoType -> Bool
    isOutputAnArray = \case
      TArr _h t -> isOutputAnArray t
      TArray _ -> True
      _ -> False

parseAndInferPretty :: forall m c. (MonadError EvalError m, Pretty c, Eq c) => ModuleMap m c -> Text -> IO ()
parseAndInferPretty prelude txt =
  (parseAndInfer @m @_ @c prelude) [] txt (const $ Right ()) >>= \case
    Left err -> print err
    Right (expr, typ, _hovers) -> do
      putStrLn $ Text.unpack $ "internal: " <> renderPretty expr

      putStrLn $ Text.unpack $ "\nhidden: " <> renderPretty (hideInternalIdents expr)

      putStrLn $ Text.unpack $ "\ntype: " <> renderPretty typ

      putStrLn $ "\ntype (pretty)" <> (Text.unpack $ renderDoc $ mkPrettyTy prelude mempty typ)

parseAndInferTypeReps :: forall m c. (MonadError EvalError m, Pretty c, Eq c) => ModuleMap m c -> Text -> [Text] -> Text -> IO ()
parseAndInferTypeReps prelude expr inTys outTy =
  (parseAndInfer @m @_ @c prelude) [] expr (const $ Right ()) >>= \case
    Left err -> print err
    Right (_expr, typ, _hovers) -> do
      putStrLn $ Text.unpack $ "\ntype: " <> renderPretty typ
      putStrLn $ "\ntype (pretty)" <> (Text.unpack $ renderDoc $ mkPrettyTy prelude mempty typ)

      case traverse parseType inTys of
        Left errs -> print errs
        Right inTysParsed -> case parseType outTy of
          Left errTy -> print errTy
          Right outTyParsed ->
            case inferTypeReps (allClasses prelude) typ inTysParsed outTyParsed of
              Left errTy' -> do
                print errTy'
                forM_ errTy' $ \e -> print $ inferErrorDiagnostic e
              Right res -> do
                putStrLn ("type reps:" :: String)
                print res

parseAndInferPossibleTypes :: forall m c. (MonadError EvalError m, Pretty c, Eq c) => ModuleMap m c -> Text -> [Maybe Text] -> Maybe Text -> IO ()
parseAndInferPossibleTypes prelude expr inTys outTy =
  (parseAndInfer @m @_ @c prelude) [] expr (const $ Right ()) >>= \case
    Left err -> print err
    Right (_expr, typ, _hovers) -> do
      putStrLn $ Text.unpack $ "\ntype: " <> renderPretty typ
      putStrLn $ "\ntype (pretty)" <> (Text.unpack $ renderDoc $ mkPrettyTy prelude mempty typ)

      case traverse (maybe (pure Nothing) ((Just <$>) . parseType)) inTys of
        Left errs -> print errs
        Right inTysParsed -> case (maybe (pure Nothing) ((Just <$>) . parseType)) outTy of
          Left errTy -> print errTy
          Right outTyParsed ->
            case inferPossibleTypes (allClasses prelude) typ inTysParsed outTyParsed of
              Left errTy' -> do
                print errTy'
                forM_ errTy' $ \e -> print $ inferErrorDiagnostic e
              Right res -> do
                putStrLn ("possible types:" :: String)
                print res

-- putStrLn $ show hovers

mkHover :: forall m c. (MonadError EvalError m, Pretty c, Eq c) => ModuleMap m c -> Set.Set TypeClass -> (SourcePos, SourcePos) -> TypeMetadata TCScheme -> (Range, MarkupContent)
mkHover prelude currentClasses (s, e) meta@TypeMetadata {identExpr = expr, ty = tcSchTy} =
  let prettyTy = mkPrettyTy prelude currentClasses tcSchTy
   in ( mkRange ((fromIntegral $ unPos $ sourceLine s) - 2) ((fromIntegral $ unPos $ sourceColumn s) - 1) ((fromIntegral $ unPos $ sourceLine e) - 2) ((fromIntegral $ unPos $ sourceColumn e) - 1),
        MarkupContent MkMarkdown $
          "**Type**\n"
            <> "~~~inferno\n"
            <> (renderDoc $ pretty expr <+> align prettyTy)
            <> "\n~~~"
            <> (maybe "" ("\n" <>) (getTypeMetadataText meta))
      )

mkPrettyTy :: forall m c ann. (MonadError EvalError m, Pretty c, Eq c) => ModuleMap m c -> Set.Set TypeClass -> TCScheme -> Doc ann
mkPrettyTy prelude currentClasses (ForallTC _tvs cls typ) =
  let ftvTy = ftv typ
   in if Set.null ftvTy
        then -- if the body of the type contains no type variables, simply pretty print it
          ":" <+> align (pretty typ)
        else -- otherwise get a union type by running findTypeClassWitnesses.
        -- things to note:
        --   * we need to filter out the "rep" typeclass, since it is always defined and thererfore pointless to pass to the solver
        --   * we only want to pass in the fvs of typ to the solver, as these are the only type variables we care about displaying
        case findTypeClassWitnesses (allClasses prelude) (Just 11) (Set.filter (\case TypeClass "rep" _ -> False; _ -> True) $ Set.union cls currentClasses) ftvTy of
          [] -> error "we must always have at least one witness!"
          subs ->
            let prettyList = map pretty $ nub $ sort $ map (flip apply $ filterOutImplicitTypeReps typ) subs
                prettyListMax10 = take 10 prettyList
             in if length prettyListMax10 == length prettyList
                  then (sep $ unionTySig prettyList)
                  else (sep $ unionTySig $ prettyList <> ["..."])
  where
    unionTySig [] = []
    unionTySig (t : ts) = (":" <+> t) : go ts
    go [] = []
    go (t : ts) = ("|" <+> t) : go ts

    filterOutImplicitTypeReps (ImplType impls ty) =
      ImplType (Map.filter (\case TRep _ -> False; _ -> True) impls) ty

getTypeMetadataText :: TypeMetadata TCScheme -> Maybe Text
getTypeMetadataText TypeMetadata {docs = tcsDocs, ty = ForallTC _ _ (ImplType _ tcsTy)} =
  renderDoc
    <$> case (tcsDocs, tcsTy) of
      (Nothing, _) -> Nothing
      (_, TBase (TEnum nm cs)) ->
        Just $
          pretty (fromMaybe "" tcsDocs)
            <> hardline
            <> "~~~inferno"
            <> hardline
            <> "enum"
            <+> pretty nm
            <+> align (sep $ "=" : (punctuate' "|" $ map (("#" <>) . pretty . unIdent) $ Set.toList cs))
              <> hardline
              <> "~~~"
      _ -> Just $ pretty (fromMaybe "" tcsDocs)
