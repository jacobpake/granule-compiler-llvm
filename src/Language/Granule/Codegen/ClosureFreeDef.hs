{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.Granule.Codegen.ClosureFreeDef where
import Language.Granule.Codegen.NormalisedDef
import Language.Granule.Codegen.MarkGlobals
import Language.Granule.Syntax.Expr
import Language.Granule.Syntax.Type
import Language.Granule.Syntax.Def
import Language.Granule.Syntax.Identifiers
import Language.Granule.Syntax.Pretty
import Language.Granule.Syntax.Span
import Language.Granule.Syntax.Pattern
import GHC.Generics

import qualified Prettyprinter as P

newtype ClosureEnvironmentType =
    TyClosureEnvironment [Type]
    deriving (Show, Eq)

type NamedClosureEnvironmentType = (String, ClosureEnvironmentType)

data ClosureEnvironmentInit =
    ClosureEnvironmentInit String [ClosureVariableInit]
    deriving (Show, Eq)

data ClosureVariableInit =
    FromParentEnv Id Type Int
    | FromLocalScope Id Type
    deriving (Show, Eq, Ord)

closureVariableInitType :: ClosureVariableInit -> Type
closureVariableInitType (FromParentEnv _ ty _) = ty
closureVariableInitType (FromLocalScope _ ty) = ty

data ClosureFreeFunctionDef = ClosureFreeFunctionDef {
    closureFreeDefSpan :: Span,
    closureFreeDefIdentifier :: Id,
    closureFreeDefEnvironment :: Maybe NamedClosureEnvironmentType,
    closureFreeDefBody :: Expr (Either GlobalMarker ClosureMarker) Type,
    closureFreeDefArgument :: Pattern Type,
    closureFreeDefTypeScheme :: TypeScheme }
    deriving (Generic, Eq, Show)

closureFreeDefType :: ClosureFreeFunctionDef -> Type
closureFreeDefType ClosureFreeFunctionDef { closureFreeDefTypeScheme = ts } =
    ty where (Forall _ _ _ ty) = ts

type ClosureFreeExpr = Expr (Either GlobalMarker ClosureMarker) Type
type ClosureFreeValue = Value (Either GlobalMarker ClosureMarker) Type
type ClosureFreeValueDef = ValueDef (Either GlobalMarker ClosureMarker) Type

data ClosureMarker =
    CapturedVar Type Id Int
    | MakeClosure Id ClosureEnvironmentInit
    | MakeTrivialClosure Id
    deriving (Show, Eq)

data ClosureFreeAST =
    ClosureFreeAST [DataDecl] [ClosureFreeFunctionDef] [ClosureFreeValueDef]
    deriving (Show, Eq)

instance Pretty ClosureFreeAST where
    wlpretty (ClosureFreeAST dataDecls functionDefs valueDefs) =
        pretty' dataDecls <> "\n\n" <> pretty' functionDefs <> "\n\n" <> pretty' valueDefs
        where
            pretty' :: Pretty l => [l] -> P.Doc Annotation
            pretty' = mconcat . P.punctuate "\n\n" . map wlpretty

instance Pretty ClosureFreeFunctionDef where
    wlpretty (ClosureFreeFunctionDef _ v env e ps t) = wlpretty v <> " : " <> wlpretty t <> "\n" <>
                              wlpretty v <> " " <> wlpretty ps <> " = " <> wlpretty e

instance Pretty ClosureMarker where
    wlpretty (CapturedVar _ty ident _n) =
        "env(" <> wlpretty ident <> ")"
    wlpretty (MakeClosure ident env) =
        "make-closure(" <> wlpretty ident <> ", " <> wlpretty env <> ")"
    wlpretty (MakeTrivialClosure ident) = wlpretty ident

instance Pretty ClosureVariableInit where
    wlpretty (FromParentEnv ident ty _) = 
        "parent-env(" <> wlpretty ident <> ") : " <> wlpretty ty
    wlpretty (FromLocalScope ident ty) = 
        wlpretty ident <> " : " <> wlpretty ty
    
instance Pretty ClosureEnvironmentInit where
    wlpretty (ClosureEnvironmentInit envName varInits) =
        "env(ident = \"" <> P.pretty envName <> "\", " <> mconcat (P.punctuate ", " (map wlpretty varInits))