{-# LANGUAGE TypeFamilies #-}

module Inferno.Parse.Commented where

import Data.List.NonEmpty (fromList, toList)
import Inferno.Types.Syntax

insertCommentIntoImport :: Comment SourcePos -> Import SourcePos -> Import SourcePos
insertCommentIntoImport comment i =
  let (startE, endE) = blockPosition i
   in if endC <= startE
        then ICommentAbove comment i
        else -- if the comment starts after the current block then, either

          if endE <= startC
            then
              let SourcePos {sourceLine = eLine} = endE
                  SourcePos {sourceLine = cLine} = startC
               in -- it is on the same line as the block
                  if eLine == cLine
                    then ICommentAfter i comment
                    else -- otherwise it is below the block
                      ICommentBelow i comment
            else -- if the comment is neither before nor after the block, it must be within the expression
            case i of
              ICommentAfter i1 c -> ICommentAfter (insertCommentIntoImport comment i1) c
              ICommentBelow i1 c -> ICommentBelow (insertCommentIntoImport comment i1) c
              _ -> i
  where
    (startC, endC) = blockPosition comment

insertCommentIntoPat :: Comment SourcePos -> Pat hash SourcePos -> Pat hash SourcePos
insertCommentIntoPat comment e =
  let (startE, endE) = blockPosition e
   in if endC <= startE
        then PCommentAbove comment e
        else -- if the comment starts after the current block then, either

          if endE <= startC
            then
              let SourcePos {sourceLine = eLine} = endE
                  SourcePos {sourceLine = cLine} = startC
               in -- it is on the same line as the block
                  if eLine == cLine
                    then PCommentAfter e comment
                    else -- otherwise it is below the block
                      PCommentBelow e comment
            else -- if the comment is neither before nor after the block, it must be within the expression
            case e of
              PTuple p1 es1 p2 -> PTuple p1 (tListFromList $ insertTuple $ tListToList es1) p2
              POne p e1 -> POne p $ insertCommentIntoPat comment e1
              PCommentAfter e1 c -> PCommentAfter (insertCommentIntoPat comment e1) c
              PCommentBelow e1 c -> PCommentBelow (insertCommentIntoPat comment e1) c
              _ -> e
  where
    (startC, endC) = blockPosition comment

    insertTuple = \case
      [] -> []
      [(e1, mp)] -> [(insertCommentIntoPat comment e1, mp)]
      x@(e1, Just commaPos) : xs ->
        if endC <= commaPos
          then (insertCommentIntoPat comment e1, Just commaPos) : xs
          else x : insertTuple xs
      -- this case should be unreachable
      x : xs -> x : insertTuple xs

insertCommentIntoExpr :: Comment SourcePos -> Expr hash SourcePos -> Expr hash SourcePos
insertCommentIntoExpr comment = go'
  where
    (startC, endC) = blockPosition comment
    commentIsWithin s e = s <= startC && endC <= e
    commentIsBefore p = endC <= p

    istrGo' :: IStr f (SourcePos, Expr hash SourcePos, SourcePos) -> IStr f (SourcePos, Expr hash SourcePos, SourcePos)
    istrGo' ISEmpty = ISEmpty
    istrGo' (ISStr x xs) = ISStr x $ istrGo' xs
    istrGo' (ISExpr (p1, e, p2) xs) =
      if commentIsWithin p1 p2
        then ISExpr (p1, go' e, p2) xs
        else ISExpr (p1, e, p2) $ istrGo' xs

    tupleGo' :: [(Expr hash SourcePos, Maybe SourcePos)] -> [(Expr hash SourcePos, Maybe SourcePos)]
    tupleGo' = \case
      [] -> []
      [(e, mp)] -> [(go' e, mp)]
      x@(e, Just commaPos) : xs ->
        if commentIsBefore commaPos
          then (go' e, Just commaPos) : xs
          else x : tupleGo' xs
      _ -> error "unreachable"

    caseGo' ::
      [(SourcePos, Pat hash SourcePos, SourcePos, Expr hash SourcePos)] ->
      [(SourcePos, Pat hash SourcePos, SourcePos, Expr hash SourcePos)]
    caseGo' = \case
      [] -> []
      [(ofPos, pat, arrPos, e)] ->
        if commentIsBefore arrPos
          then [(ofPos, insertCommentIntoPat comment pat, arrPos, e)]
          else [(ofPos, pat, arrPos, go' e)]
      x@(ofPos, pat, arrPos, e) : xs@((ofPos2, _, _, _) : _) ->
        if commentIsBefore arrPos
          then (ofPos, insertCommentIntoPat comment pat, arrPos, e) : xs
          else
            if commentIsWithin arrPos ofPos2
              then (ofPos, pat, arrPos, go' e) : xs
              else x : caseGo' xs

    arrayCompGo' ::
      Maybe (a1, Expr hash SourcePos) ->
      [(a0, b0, c0, Expr hash SourcePos, Maybe SourcePos)] ->
      ([(a0, b0, c0, Expr hash SourcePos, Maybe SourcePos)], Maybe (a1, Expr hash SourcePos))
    arrayCompGo' mcond = \case
      [] -> ([], case mcond of Just (p, e) -> Just (p, go' e); Nothing -> Nothing)
      [(p1, ident, p2, e, Nothing)] -> ([(p1, ident, p2, go' e, Nothing)], mcond)
      x@(p1, ident, p2, e, Just commaPos) : xs ->
        if commentIsBefore commaPos
          then ((p1, ident, p2, go' e, Just commaPos) : xs, mcond)
          else let (xs', mcond') = arrayCompGo' mcond xs in (x : xs', mcond')
      _ -> error "unreachable"

    importsGo' = \case
      [] -> []
      [(i, mp)] -> [(insertCommentIntoImport comment i, mp)]
      x@(i, Just commaPos) : xs ->
        if commentIsBefore commaPos
          then (insertCommentIntoImport comment i, Just commaPos) : xs
          else x : importsGo' xs
      _ -> error "unreachable"

    go' :: Expr hash SourcePos -> Expr hash SourcePos
    go' x = head $ go [x]

    go :: [Expr hash SourcePos] -> [Expr hash SourcePos]
    go =
      \case
        [] -> []
        (e : es) ->
          let (startE, endE) = blockPosition e
           in if endC <= startE
                then CommentAbove comment e : es
                else -- if the comment starts after the current block then, either

                  if endE <= startC
                    then
                      let SourcePos {sourceLine = eLine} = endE
                          SourcePos {sourceLine = cLine} = startC
                       in -- it is on the same line as the block
                          if eLine == cLine
                            then CommentAfter e comment : es
                            else -- otherwise it is below the block

                            -- in case `e` is the last element, we attach the comment below `e`

                            case es of
                              [] -> [CommentBelow e comment]
                              _ ->
                                -- in case we have more blocks in the list, we instead proceed to attach the comment lower down
                                e : go es
                    else -- if the comment is neither before nor after the block, it must be within the expression

                      ( case e of
                          App e1 e2 -> let res = go [e1, e2] in App (res !! 0) (res !! 1)
                          Lam p1 xs p2 body -> Lam p1 xs p2 $ go' body
                          Let p1 p2 v p3 e1 posOfIn e2 ->
                            if commentIsBefore posOfIn
                              then Let p1 p2 v p3 (go' e1) posOfIn e2
                              else Let p1 p2 v p3 e1 posOfIn $ go' e2
                          InterpolatedString p1 (SomeIStr xs) p2 -> InterpolatedString p1 (SomeIStr $ istrGo' xs) p2
                          If ifPos c thenPos t elsePos f ->
                            if commentIsWithin ifPos thenPos
                              then If ifPos (go' c) thenPos t elsePos f
                              else
                                if commentIsWithin thenPos elsePos
                                  then If ifPos c thenPos (go' t) elsePos f
                                  else If ifPos c thenPos t elsePos (go' f)
                          Op e1 posOfOp hash opMeta ns op e2 ->
                            if commentIsBefore posOfOp
                              then Op (go' e1) posOfOp hash opMeta ns op e2
                              else Op e1 posOfOp hash opMeta ns op $ go' e2
                          PreOp posOfOp hash opMeta ns op e1 ->
                            PreOp posOfOp hash opMeta ns op $ go' e1
                          Tuple p1 es1 p2 -> Tuple p1 (tListFromList $ tupleGo' $ tListToList es1) p2
                          One p e1 -> One p $ go' e1
                          Assert p1 c inPos e1 ->
                            if commentIsBefore inPos
                              then Assert p1 (go' c) inPos e1
                              else Assert p1 c inPos $ go' e1
                          Case p1 e_c brPos cases p2 ->
                            if commentIsBefore brPos
                              then Case p1 (go' e_c) brPos cases p2
                              else Case p1 e_c brPos (fromList $ caseGo' $ toList cases) p2
                          Array p1 es1 p2 -> Array p1 (tupleGo' es1) p2
                          ArrayComp p1 e1 posOfBar args mcond p2 ->
                            if commentIsBefore posOfBar
                              then ArrayComp p1 (go' e1) posOfBar args mcond p2
                              else
                                let (args', mcond') = arrayCompGo' mcond $ toList args
                                 in ArrayComp p1 e1 posOfBar (fromList args') mcond' p2
                          CommentAfter e1 c -> CommentAfter (go' e1) c
                          CommentBelow e1 c -> CommentBelow (go' e1) c
                          Bracketed p1 e1 p2 -> Bracketed p1 (go' e1) p2
                          RenameModule p1 m1 p2 m2 p3 e1 -> RenameModule p1 m1 p2 m2 p3 $ go' e1
                          OpenModule p1 hash mn [] inPos e1 -> OpenModule p1 hash mn [] inPos $ go' e1
                          OpenModule p1 hash mn is inPos e1 ->
                            if commentIsBefore inPos
                              then OpenModule p1 hash mn (importsGo' is) inPos e1
                              else OpenModule p1 hash mn is inPos $ go' e1
                          _ -> e
                      )
                        : es

insertCommentsIntoExpr :: [Comment SourcePos] -> Expr hash SourcePos -> Expr hash SourcePos
insertCommentsIntoExpr = flip (foldr insertCommentIntoExpr)
