{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE KindSignatures        #-}
-- | Provides a 'astConstrs' and 'astNodes' that return
--   the consturctor or the types of the AST in a given line range.
module HsInfo.ASTInfo
  ( HsModule
  , hsParseModule , hsTokenize
  , LineRegion(..)
  , TyCon(..)
  , tyconString
  , Choice
  , stackIn , conStackIn , typeStackIn
  , firstTypeIn , firstConIn , allInfoIn
  , allConsIn , allTypesIn , firstConParentOf
  , astInfo
  ) where

import Language.Haskell.Exts.Syntax hiding (ConName)
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.SrcLoc
import Control.Monad.State
import Control.Applicative
import GHC.Generics
import Data.Proxy
import Data.List(group)
import GHC.Real
import Debug.Trace

-- |A Haskell module type synonym.
type HsModule = Module SrcSpanInfo

-- |First we provide a renamed parser for Haskell files.
--  This is convenient as the parseModule exported by
--  Language.Haskell.Exts.Parser seems to fail too often if some
--  extensions are not manually enabled.
hsParseModule :: String -> Either String HsModule
hsParseModule c
  = case parseModuleWithMode mode c of
      ParseFailed l err -> Left err
      ParseOk mod       -> Right mod
  where
    mode :: ParseMode
    mode = defaultParseMode { baseLanguage = Haskell2010
                            , ignoreLanguagePragmas = False
                            , ignoreLinePragmas = False
                            , extensions = usefullExts
                            }
    -- Some extensions
    usefullExts :: [Extension]
    usefullExts = map EnableExtension
                [ MultiParamTypeClasses
                , FlexibleContexts
                , DefaultSignatures
                , TypeOperators
                -- , UndecidableInstances
                -- , FlexibleInstances
                ]
                
hsTokenize :: [String] -> [String] -> Either String ([Token] , [Token])
hsTokenize ins del
  = let tks = ((,) <$> lexTokenStream (unlines ins)
                   <*> lexTokenStream (unlines del))
    in case tks of
      ParseFailed _ s     -> Left $ s
      ParseOk (tkI , tkD) -> Right (map unLoc tkI , map unLoc tkD)

-- |A LineRegion is a starting line with an optional
--  starting column and an endline.
data LineRegion
  = LR { _lrStartLine :: Int
       , _lrStartCol  :: Maybe Int
       , _lrEndLine   :: Int
       }
  deriving (Eq , Show)

-- |Given a LineRegion and a SrcSpanInfo, we can check
--  whether that source span is in the region or not.
--
inRegion :: LineRegion -> SrcSpanInfo -> Bool
inRegion (LR min col max) si
  = let si'         = srcInfoSpan si
        siStartLine = srcSpanStartLine   si'
        siStartCol  = srcSpanStartColumn si'
        siEndLine   = srcSpanEndLine     si'
     in min               <= siStartLine
     && (maybe 0 id col)  <= siStartCol
     && siEndLine         <= max

-- |Either the type of an AST node, or
--  the constructor it was used.
data TyCon
  = TyName  String
  | ConName String
  deriving (Show, Eq)

tyconString :: TyCon -> String
tyconString (TyName  s) = 't':s
tyconString (ConName s) = 'c':s

-- | The function that given the stack of Type and Constructor
--   names decides what to do.
type Choice m a = [TyCon] -> m a

-- * Simple Tmp to control the visited constructors.
type Tmp = State [TyCon]

-- | Holds a TyCon in the stack locally for a computation.
--   pops it as soon as we are done.
hold :: TyCon -> Tmp a -> Tmp a
hold s t = modify (s:) >> t <* modify tail

-- | Finally, some haskell magic to get the constructors
--   or the types of the AST in a given source position.
--   The method is parametrized by a choice function,
--   which gets one alternative from a stack of names.
--
--   As we keep traversing the AST, we push type names
--   and constructor names into a stack. Whenever we reach
--   a point where the SrcSpanInfo is inside our region,
--   we call the choice function to get something from this stack.
class ASTInfo f where
  astInfoCtx :: (Alternative m)
          => Choice m a -> LineRegion -> f -> Tmp (m a)
  default astInfoCtx :: (Alternative m, Generic f , GASTInfo (Rep f))
                     => Choice m a -> LineRegion -> f -> Tmp (m a)
  astInfoCtx ch lr x = gastInfoCtx ch lr (from x)

  astInfo :: (Alternative m)
          => Choice m a -> LineRegion -> f -> m a
  astInfo ch lr x = evalState (astInfoCtx ch lr x) empty

-- * Generic definition
class GASTInfo r where
  gastInfoCtx :: (Alternative m) => Choice m a -> LineRegion -> r k -> Tmp (m a)

instance GASTInfo U1 where
  gastInfoCtx ch _ _ = return empty

instance {-# OVERLAPPING #-} GASTInfo (K1 R SrcSpanInfo) where
  gastInfoCtx ch lr (K1 x)
    | inRegion lr x = ch <$> get
    | otherwise     = return empty
    
instance {-# OVERLAPPABLE #-} ASTInfo f => GASTInfo (K1 i f) where
  gastInfoCtx ch lr (K1 x) = astInfoCtx ch lr x

instance (Datatype d , GASTInfo x)
         => GASTInfo (M1 D d x) where
  gastInfoCtx ch lr all@(M1 x) = hold (TyName $ datatypeName all) (gastInfoCtx ch lr x)

instance (Constructor d , GASTInfo x)
         => GASTInfo (M1 C d x) where
  gastInfoCtx ch lr all@(M1 x) = hold (ConName $ conName all) (gastInfoCtx ch lr x)

instance {-# OVERLAPPABLE #-} (GASTInfo x)
         => GASTInfo (M1 i c x) where
  gastInfoCtx ch lr (M1 x) = gastInfoCtx ch lr x

instance (GASTInfo x , GASTInfo y) => GASTInfo (x :*: y) where
  gastInfoCtx ch lr (x :*: y) = (<|>) <$> gastInfoCtx ch lr x <*> gastInfoCtx ch lr y
  
instance (GASTInfo x , GASTInfo y) => GASTInfo (x :+: y) where
  gastInfoCtx ch lr (L1 x) = gastInfoCtx ch lr x
  gastInfoCtx ch lr (R1 x) = gastInfoCtx ch lr x

instance {-# OVERLAPPING #-} ASTInfo String where
  astInfoCtx _ _ _ = return empty
instance ASTInfo Char where
  astInfoCtx _ _ _ = return empty
instance ASTInfo Int where
  astInfoCtx _ _ _ = return empty
instance ASTInfo Integer where
  astInfoCtx _ _ _ = return empty
instance ASTInfo Tool where
  astInfoCtx _ _ _ = return empty
instance ASTInfo Bool where
  astInfoCtx _ _ _ = return empty
instance ASTInfo Boxed where
  astInfoCtx _ _ _ = return empty
instance ASTInfo (Ratio Integer) where
  astInfoCtx _ _ _ = return empty
instance ASTInfo a => ASTInfo [a] where
  astInfoCtx ch lr x = mapM (astInfoCtx ch lr) x >>= return . (foldr (<|>) empty)
instance ASTInfo (a , b) where
  astInfoCtx _ _ _ = return empty
instance ASTInfo a => ASTInfo (Maybe a) where
  astInfoCtx ch lr (Just x) = astInfoCtx ch lr x
  astInfoCtx ch lr Nothing  = return empty
instance ASTInfo (Module SrcSpanInfo) 
instance ASTInfo (XName SrcSpanInfo)
instance ASTInfo (ModuleName SrcSpanInfo)
instance ASTInfo (ModulePragma SrcSpanInfo)
instance ASTInfo (ModuleHead SrcSpanInfo)
instance ASTInfo (ExportSpecList SrcSpanInfo)
instance ASTInfo (ImportSpecList SrcSpanInfo)
instance ASTInfo (ImportSpec SrcSpanInfo)
instance ASTInfo (ExportSpec SrcSpanInfo)
instance ASTInfo (CName SrcSpanInfo)
instance ASTInfo (QName SrcSpanInfo)
instance ASTInfo (Namespace SrcSpanInfo)
instance ASTInfo (EWildcard SrcSpanInfo)
instance ASTInfo (SpecialCon SrcSpanInfo)
instance ASTInfo (WarningText SrcSpanInfo)
instance ASTInfo (ImportDecl SrcSpanInfo)
instance ASTInfo (XAttr SrcSpanInfo)
instance ASTInfo (Sign SrcSpanInfo)
instance ASTInfo (Pat SrcSpanInfo)
instance ASTInfo (PXAttr SrcSpanInfo)
instance ASTInfo (PatField SrcSpanInfo)
instance ASTInfo (RPat SrcSpanInfo)
instance ASTInfo (RPatOp SrcSpanInfo)
instance ASTInfo (Exp SrcSpanInfo)
instance ASTInfo (Alt SrcSpanInfo)
instance ASTInfo (Rhs SrcSpanInfo)
instance ASTInfo (Bracket SrcSpanInfo)
instance ASTInfo (FieldUpdate SrcSpanInfo)
instance ASTInfo (GuardedRhs SrcSpanInfo)
instance ASTInfo (IPName SrcSpanInfo)
instance ASTInfo (Literal SrcSpanInfo)
instance ASTInfo (QOp SrcSpanInfo)
instance ASTInfo (QualStmt SrcSpanInfo)
instance ASTInfo (Splice SrcSpanInfo)
instance ASTInfo (Stmt SrcSpanInfo)
instance ASTInfo (Binds SrcSpanInfo)
instance ASTInfo (IPBind SrcSpanInfo)
instance ASTInfo (Unpackedness SrcSpanInfo)
instance ASTInfo (Type SrcSpanInfo)
instance ASTInfo (BangType SrcSpanInfo)
instance ASTInfo (Asst SrcSpanInfo)
instance ASTInfo (Role SrcSpanInfo)
instance ASTInfo (RuleVar SrcSpanInfo)
instance ASTInfo (Context SrcSpanInfo)
instance ASTInfo (Promoted SrcSpanInfo)
instance ASTInfo (TypeEqn SrcSpanInfo)
instance ASTInfo (Kind SrcSpanInfo)
instance ASTInfo (TyVarBind SrcSpanInfo)
instance ASTInfo (Safety SrcSpanInfo)
instance ASTInfo (Decl SrcSpanInfo)
instance ASTInfo (Assoc SrcSpanInfo)
instance ASTInfo (BooleanFormula SrcSpanInfo)
instance ASTInfo (CallConv SrcSpanInfo)
instance ASTInfo (DeclHead SrcSpanInfo)
instance ASTInfo (ClassDecl SrcSpanInfo)
instance ASTInfo (DataOrNew SrcSpanInfo)
instance ASTInfo (GadtDecl SrcSpanInfo)
instance ASTInfo (InjectivityInfo SrcSpanInfo)
instance ASTInfo (InstDecl SrcSpanInfo)
instance ASTInfo (FunDep SrcSpanInfo)
instance ASTInfo (Deriving SrcSpanInfo)
instance ASTInfo (InstRule SrcSpanInfo)
instance ASTInfo (InstHead SrcSpanInfo)
instance ASTInfo (Match SrcSpanInfo)
instance ASTInfo (Op SrcSpanInfo)
instance ASTInfo (Overlap SrcSpanInfo)
instance ASTInfo (PatternSynDirection SrcSpanInfo)
instance ASTInfo (QualConDecl SrcSpanInfo)
instance ASTInfo (ConDecl SrcSpanInfo)
instance ASTInfo (FieldDecl SrcSpanInfo)
instance ASTInfo (Activation SrcSpanInfo)
instance ASTInfo (ResultSig SrcSpanInfo)
instance ASTInfo (Rule SrcSpanInfo)
instance ASTInfo (Name SrcSpanInfo)
instance ASTInfo (Annotation SrcSpanInfo)

-- * Convenient predicates over TyCon

isCon :: TyCon -> Bool
isCon (TyName _) = False
isCon (ConName _) = True

-- * A few interesting navigation functions:
--
-- To exemplify, imagine the following piece of code:
--
--       0123456789abcdefghijklmnopqrstuvwxyz
--    --------------------------------
-- 01 |  module Fib where
-- 02 |
-- 03 |  import SomeModule
-- 04 |
-- 05 |  fib :: Int -> Int
-- 06 |  fib 0 = 1
-- 07 |  fib 1 = 1
-- 08 |  fib n = fib (n - 1) + fib (n - 2)
--
--
--
-- Imagine we have parsed it into the variable c.

c = let Right r = hsParseModule
                $ unlines ["module Fib where"
                          ,""
                          ,"import SomeModule"
                          ,""
                          ,"fib :: Int -> Int"
                          ,"fib 0 = 1"
                          ,"fib 1 = 1"
                          ,"fib n = fib (n - 1) + fib (n - 2)"
                          ]
     in r


-- | stackIn returns the FIRST stack of TyCon's found within a LineRegion.
--
--   stackIn (LineRegion (LR 5 (Just 8) 5)) c
--      == Just [ConName "TyFun",TyName "Type",ConName "TypeSig"
--              ,TyName "Decl",ConName "Module",TyName "Module"]
--
--  Note that it will be same as @stackIn (LineRegion (LR 5 (Just 8) 431)) c@ 
stackIn :: LineRegion -> HsModule -> Maybe [TyCon]
stackIn = astInfo stackInCHOICE
  where
    stackInCHOICE :: (Alternative m) => Choice m [TyCon]
    stackInCHOICE [] = empty
    stackInCHOICE xs = pure xs

-- | Returns only the constructors from stackIn
conStackIn :: LineRegion -> HsModule -> Maybe [String]
conStackIn lr c = (map tyconString . filter isCon) <$> stackIn lr c

-- | Returns only the types from stackIn
typeStackIn :: LineRegion -> HsModule -> Maybe [String]
typeStackIn lr c = (map tyconString . filter (not . isCon)) <$> stackIn lr c

-- | Returns the name of the first type found in the region.
firstTypeIn :: LineRegion -> HsModule -> Maybe String
firstTypeIn = astInfo (choice . filter (not . isCon))
  where
    choice (c:_) = Just $ tyconString c
    choice _     = Nothing

-- | Returns the name of the first constructor found in the region.
firstConIn :: LineRegion -> HsModule -> Maybe String
firstConIn = astInfo (choice . filter isCon)
  where
    choice (c:_) = Just $ tyconString c
    choice _     = Nothing

-- | Returns all tycons found in the region, as a list
--   of (constructorName , typeName)
--
--  In line8, col27 of c we have: "(n - 2)"
--
-- allInfoIn (LR 8 (Just 27) 8) c
--   = [ ("cParen","tExp") , ("cInfixApp","tExp") , ("cVar","tExp")
--     , ("cUnQual","tQName") , ("cIdent","tName") , ("cQVarOp","tQOp")
--     , ("cUnQual","tQName") , ("cSymbol","tName") , ("cLit","tExp")
--     , ("cInt","tLiteral")]
--
--
allInfoIn :: LineRegion -> HsModule -> [(String , String)]
allInfoIn = astInfo choice
  where
    choice (c:t:_) = [ (tyconString c , tyconString t) ]
    choice _     = []

-- | Only the constructors of allInfoIn
allConsIn :: LineRegion -> HsModule -> [String]
allConsIn lr = map fst . allInfoIn lr

-- | Only the types of allInfoIn
allTypesIn :: LineRegion -> HsModule -> [String]
allTypesIn lr = map snd . allInfoIn lr

-- | Returns the constructor that is the immediate parent
--   of the first constructor found in the region.
--
--  @parentOf (LR 5 (Just 8) 5) c == Just "cTypeSig"@
--
--  Even though in line 5, column 8, we have the beginning
--  of an identifier ("Int"), it is placed within a "cTypeSig".
firstConParentOf :: LineRegion -> HsModule -> Maybe String
firstConParentOf = astInfo choice
  where
    choice xs = case tail (filter isCon xs) of
                []    -> Nothing
                (h:_) -> Just (tyconString h)
