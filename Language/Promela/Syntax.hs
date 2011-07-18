module Language.Promela.Syntax where

data Module
    = ProcType
      { proctypeActive :: Maybe (Maybe Integer)
      , proctypeName :: String
      , proctypeArguments :: [Declaration]
      , proctypePriority :: Maybe Integer
      , proctypeProvided :: Maybe Expression
      , proctypeSteps :: [Step]
      }
    | Decl Declaration
    | Typedef String [Declaration]
    | CState String String (Maybe String)
    | CDecl String
    | CCode String
    | Init (Maybe Integer) [Step]
    | Never [Step]
    | LTL (Maybe String) LTLExpr
    deriving (Show,Eq,Ord)

data Declaration = Declaration
    { declarationVisible :: Maybe Bool
    , declarationType :: Typename
    , declarationVariables :: [(String,Maybe Integer,Maybe AnyExpression)]
    } deriving (Show,Eq,Ord)

data Typename
    = TypeBit
    | TypeBool
    | TypeByte
    | TypeShort
    | TypeInt
    | TypeMType
    | TypeChan
    | TypeCustom String
    deriving (Show,Eq,Ord)

data AnyExpression
    = BinExpr BinOp AnyExpression AnyExpression
    | UnExpr UnOp AnyExpression
    | RefExpr VarRef
    | RunExpr String [AnyExpression] (Maybe Integer)
    | ConstExpr Constant
    deriving (Show,Eq,Ord)

data VarRef = VarRef String (Maybe Integer) (Maybe VarRef)
            deriving (Show,Eq,Ord)

data Expression
    = ExprAny AnyExpression
    deriving (Show,Eq,Ord)

data BinOp
    = BinPlus
    | BinMinus
    | BinMult
    | BinDiv
    | BinMod
    | BinBitAnd
    | BinExp
    | BinBitOr
    | BinGT
    | BinLT
    | BinGTE
    | BinLTE
    | BinEquals
    | BinNotEquals
    | BinShiftL
    | BinShiftR
    | BinAnd
    | BinOr
    deriving (Show,Eq,Ord)

data UnOp
     = UnLNot
     | UnBNot
     | UnNeg
     deriving (Show,Eq,Ord)

data Step = StepStmt Statement (Maybe Statement)
          | StepDecl Declaration
          deriving (Show,Eq,Ord)

data Statement
    = StmtIf [[Step]]
    | StmtDo [[Step]]
    | StmtAtomic [Step]
    | StmtDStep [Step]
    | StmtSequence [Step]
    | StmtElse
    | StmtBreak
    | StmtGoto String
    | StmtLabel String Statement
    | StmtExpr Expression
    | StmtCCode String
    | StmtCExpr (Maybe String) String
    | StmtAssign VarRef AnyExpression
    | StmtReceive String [RecvArg]
    | StmtPrintf String [AnyExpression]
    | StmtPrintm Expression
    | StmtRun String [AnyExpression]
    | StmtSkip
    deriving (Show,Eq,Ord)

data Constant
    = ConstBool Bool
    | ConstInt Integer
    | ConstSkip
    deriving (Show,Eq,Ord)

data RecvArg = RecvVar VarRef
             | RecvEval VarRef
             | RecvConst Constant
             deriving (Show,Eq,Ord)

data LTLBinOp = LTLUntil
              | LTLWeakUntil
              | LTLUntilOp
              | LTLAnd
              | LTLOr
              | LTLImplication
              | LTLEquivalence
              deriving (Show,Eq,Ord)

data LTLUnOp = LTLAlways
             | LTLEventually
             | LTLNot
             | LTLNext
             deriving (Show,Eq,Ord)

data LTLExpr = LTLNormalExpr AnyExpression
             | LTLBin LTLBinOp LTLExpr LTLExpr
             | LTLUn LTLUnOp LTLExpr
             deriving (Show,Eq,Ord)

prIf :: ToStep a => [[a]] -> Statement
prIf stps = StmtIf (fmap (fmap toStep) stps)

prAtomic :: ToStep a => [a] -> Statement
prAtomic stps = StmtAtomic (fmap toStep stps)

prDStep :: ToStep a => [a] -> Statement
prDStep stps = StmtDStep (fmap toStep stps)

prNever :: ToStep a => [a] -> Module
prNever stps = Never (fmap toStep stps)

prSequence :: ToStep a => [a] -> Statement
prSequence stps = StmtSequence (fmap toStep stps)

prDo :: ToStep a => [[a]] -> Statement
prDo stps = StmtDo (fmap (fmap toStep) stps)

prInit :: ToStep a => [a] -> Module
prInit stps = Init Nothing (fmap toStep stps)

class ToStep a where
  toStep :: a -> Step

instance ToStep Statement where
  toStep stmt = StepStmt stmt Nothing

instance ToStep Step where
  toStep = id