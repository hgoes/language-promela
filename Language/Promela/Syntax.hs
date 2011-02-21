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
    | Init (Maybe Integer) [Step]
    | Never [Step]
    deriving Show

data Declaration = Declaration
    { declarationVisible :: Maybe Bool
    , declarationType :: Typename
    , declarationVariables :: [(String,Maybe Integer,Maybe AnyExpression)]
    } deriving Show

data Typename
    = TypeBit
    | TypeBool
    | TypeByte
    | TypeShort
    | TypeInt
    | TypeMType
    | TypeChan
    | TypeCustom String
    deriving Show

data AnyExpression
    = BinExpr BinOp AnyExpression AnyExpression
    | RefExpr VarRef
    | RunExpr String [AnyExpression] (Maybe Integer)
    | ConstExpr Constant
    deriving Show

data VarRef = VarRef String (Maybe Integer) (Maybe VarRef)
            deriving Show

data Expression
    = ExprAny AnyExpression
    deriving Show

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
    deriving Show

data Step = StepStmt Statement (Maybe Statement)
          | StepDecl Declaration
          deriving Show

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
    deriving Show

data Constant
    = ConstBool Bool
    | ConstInt Integer
    | ConstSkip
    deriving Show

data RecvArg = RecvVar VarRef
             | RecvEval VarRef
             | RecvConst Constant
             deriving Show

prIf :: ToStep a => [[a]] -> Statement
prIf stps = StmtIf (fmap (fmap toStep) stps)

prAtomic :: ToStep a => [a] -> Statement
prAtomic stps = StmtAtomic (fmap toStep stps)

prDStep :: ToStep a => [a] -> Statement
prDStep stps = StmtDStep (fmap toStep stps)

class ToStep a where
  toStep :: a -> Step

instance ToStep Statement where
  toStep stmt = StepStmt stmt Nothing

instance ToStep Step where
  toStep = id