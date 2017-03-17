{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE KindSignatures        #-}
module GetConstrs where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.SrcLoc
import Control.Monad.State
import Control.Applicative
import GHC.Generics
import Data.Proxy
import Data.List(group)
import GHC.Real

data LineInfo
  = LineSpan (Int , Int)
  | LineCol  (Int , Int , Int)
  deriving (Eq , Show)

class GetConstrs f where
  constrs :: (Alternative m) => LineInfo -> f -> m String
  default constrs :: (Alternative m, Generic f , GGetConstrs (Rep f))
                   => LineInfo -> f -> m String
  constrs p x = evalState (gconstrs p (from x)) empty

{-
instance (Annotated f , Generic (f x) , GGetConstrs (Rep (f x)) x)
         => GetConstrs f x where
  constrs p x = evalState (gconstrs p (from x)) Nothing
-}

inLine :: (SrcInfo si) => LineInfo -> si -> Bool
inLine (LineCol (line , col , max)) si
  = line <= startLine si
   && col  <= startColumn si
   && startLine si <= max
inLine (LineSpan (lo , hi))   si = lo   <= startLine si
                                && hi   <= startLine si

-- * Simple Tmp to control the visited constructors.
type Tmp = State [String]

hold :: String -> Tmp ()
hold s = modify (s:)

drop :: Tmp ()
drop = put []

choose :: (Alternative m) => [String] -> m String
choose [] = empty
choose ss = pure (last ss)

-- * Generic definition
class GGetConstrs r where
  gconstrs :: (Alternative m) => LineInfo -> r k -> Tmp (m String)

instance GGetConstrs U1 where
  gconstrs p _ = return empty

instance {-# OVERLAPPING #-} GGetConstrs (K1 R SrcSpanInfo) where
  gconstrs p (K1 x)
    | inLine p x = choose <$> get
    | otherwise  = return empty
    
instance {-# OVERLAPPABLE #-} GetConstrs f => GGetConstrs (K1 i f) where
  gconstrs p (K1 x) = return (constrs p x)

instance (Datatype d , GGetConstrs x)
         => GGetConstrs (M1 D d x) where
  gconstrs p all@(M1 x) = hold (datatypeName all) >> gconstrs p x

instance {-# OVERLAPPABLE #-} (GGetConstrs x)
         => GGetConstrs (M1 i c x) where
  gconstrs p (M1 x) = gconstrs p x

instance (GGetConstrs x , GGetConstrs y) => GGetConstrs (x :*: y) where
  gconstrs p (x :*: y) = (<|>) <$> gconstrs p x <*> gconstrs p y
  
instance (GGetConstrs x , GGetConstrs y) => GGetConstrs (x :+: y) where
  gconstrs p (L1 x) = gconstrs p x
  gconstrs p (R1 x) = gconstrs p x

instance {-# OVERLAPPING #-} GetConstrs String where
  constrs _ _ = empty
instance GetConstrs Char where
  constrs _ _ = empty
instance GetConstrs Int where
  constrs _ _ = empty
instance GetConstrs Integer where
  constrs _ _ = empty
instance GetConstrs Tool where
  constrs _ _ = empty
instance GetConstrs Bool where
  constrs _ _ = empty
instance GetConstrs Boxed where
  constrs _ _ = empty
instance GetConstrs (Ratio Integer) where
  constrs _ _ = empty
instance GetConstrs a => GetConstrs [a] where
  constrs p x = foldr (<|>) empty (map (constrs p) x)
instance GetConstrs (a , b) where
  constrs _ _ = empty
instance GetConstrs a => GetConstrs (Maybe a) where
  constrs p (Just x) = constrs p x
  constrs p Nothing  = empty
instance GetConstrs (Module SrcSpanInfo) 
instance GetConstrs (XName SrcSpanInfo)
instance GetConstrs (ModuleName SrcSpanInfo)
instance GetConstrs (ModulePragma SrcSpanInfo)
instance GetConstrs (ModuleHead SrcSpanInfo)
instance GetConstrs (ExportSpecList SrcSpanInfo)
instance GetConstrs (ImportSpecList SrcSpanInfo)
instance GetConstrs (ImportSpec SrcSpanInfo)
instance GetConstrs (ExportSpec SrcSpanInfo)
instance GetConstrs (CName SrcSpanInfo)
instance GetConstrs (QName SrcSpanInfo)
instance GetConstrs (Namespace SrcSpanInfo)
instance GetConstrs (EWildcard SrcSpanInfo)
instance GetConstrs (SpecialCon SrcSpanInfo)
instance GetConstrs (WarningText SrcSpanInfo)
instance GetConstrs (ImportDecl SrcSpanInfo)
instance GetConstrs (XAttr SrcSpanInfo)
instance GetConstrs (Sign SrcSpanInfo)
instance GetConstrs (Pat SrcSpanInfo)
instance GetConstrs (PXAttr SrcSpanInfo)
instance GetConstrs (PatField SrcSpanInfo)
instance GetConstrs (RPat SrcSpanInfo)
instance GetConstrs (RPatOp SrcSpanInfo)
instance GetConstrs (Exp SrcSpanInfo)
instance GetConstrs (Alt SrcSpanInfo)
instance GetConstrs (Rhs SrcSpanInfo)
instance GetConstrs (Bracket SrcSpanInfo)
instance GetConstrs (FieldUpdate SrcSpanInfo)
instance GetConstrs (GuardedRhs SrcSpanInfo)
instance GetConstrs (IPName SrcSpanInfo)
instance GetConstrs (Literal SrcSpanInfo)
instance GetConstrs (QOp SrcSpanInfo)
instance GetConstrs (QualStmt SrcSpanInfo)
instance GetConstrs (Splice SrcSpanInfo)
instance GetConstrs (Stmt SrcSpanInfo)
instance GetConstrs (Binds SrcSpanInfo)
instance GetConstrs (IPBind SrcSpanInfo)
instance GetConstrs (Unpackedness SrcSpanInfo)
instance GetConstrs (Type SrcSpanInfo)
instance GetConstrs (BangType SrcSpanInfo)
instance GetConstrs (Asst SrcSpanInfo)
instance GetConstrs (Role SrcSpanInfo)
instance GetConstrs (RuleVar SrcSpanInfo)
instance GetConstrs (Context SrcSpanInfo)
instance GetConstrs (Promoted SrcSpanInfo)
instance GetConstrs (TypeEqn SrcSpanInfo)
instance GetConstrs (Kind SrcSpanInfo)
instance GetConstrs (TyVarBind SrcSpanInfo)
instance GetConstrs (Safety SrcSpanInfo)
instance GetConstrs (Decl SrcSpanInfo)
instance GetConstrs (Assoc SrcSpanInfo)
instance GetConstrs (BooleanFormula SrcSpanInfo)
instance GetConstrs (CallConv SrcSpanInfo)
instance GetConstrs (DeclHead SrcSpanInfo)
instance GetConstrs (ClassDecl SrcSpanInfo)
instance GetConstrs (DataOrNew SrcSpanInfo)
instance GetConstrs (GadtDecl SrcSpanInfo)
instance GetConstrs (InjectivityInfo SrcSpanInfo)
instance GetConstrs (InstDecl SrcSpanInfo)
instance GetConstrs (FunDep SrcSpanInfo)
instance GetConstrs (Deriving SrcSpanInfo)
instance GetConstrs (InstRule SrcSpanInfo)
instance GetConstrs (InstHead SrcSpanInfo)
instance GetConstrs (Match SrcSpanInfo)
instance GetConstrs (Op SrcSpanInfo)
instance GetConstrs (Overlap SrcSpanInfo)
instance GetConstrs (PatternSynDirection SrcSpanInfo)
instance GetConstrs (QualConDecl SrcSpanInfo)
instance GetConstrs (ConDecl SrcSpanInfo)
instance GetConstrs (FieldDecl SrcSpanInfo)
instance GetConstrs (Activation SrcSpanInfo)
instance GetConstrs (ResultSig SrcSpanInfo)
instance GetConstrs (Rule SrcSpanInfo)
instance GetConstrs (Name SrcSpanInfo)
instance GetConstrs (Annotation SrcSpanInfo)


constrsL :: LineInfo -> Module SrcSpanInfo -> [String]
constrsL = constrs

constrsM :: LineInfo -> Module SrcSpanInfo -> Maybe String
constrsM = constrs

runTest :: FilePath -> LineInfo -> IO ()
runTest f l = do c <- readFile f
                 case parseModuleWithMode myMode c of
                   ParseFailed l err -> putStrLn ("fail at " ++ show l)
                                     >> putStrLn err
                                     >> return ()
                   ParseOk r         -> putStrLn (show $ constrsM l r)
                                     >> putStrLn (show $ constrsL l r)
  where
    usefullExts :: [Extension]
    usefullExts = map EnableExtension
                [ MultiParamTypeClasses
                , FlexibleContexts
                , DefaultSignatures
                , TypeOperators
                ]

    myMode :: ParseMode
    myMode = defaultParseMode { baseLanguage = Haskell2010
                              , ignoreLanguagePragmas = False
                              , ignoreLinePragmas = False
                              , extensions = usefullExts
                              }

testGetConstrs :: LineInfo -> IO ()
testGetConstrs = runTest "./hsFileHistCrunch_src/GetConstrs.hs"

testMain :: LineInfo -> IO ()
testMain = runTest "./hsFileHistCrunch_src/Main.hs"
{-  
allLineTypes :: FilePath -> IO [(Int , Maybe String)]
allLineTypes file
  = do
    c <- readFile file
    let nlines      = length (lines c)
    let (ParseOk r) = parseModuleWithMode myMode c
    let cstrs = map (aux r) [1..nlines]
    return (map (\l -> (length l , head l)) (group cstrs))
-}
