module Language.Promela.Pretty where

import Language.Promela.Syntax
import Text.PrettyPrint

prettyPromela :: [Module] -> Doc
prettyPromela = vcat . map prettyModule

prettyModule :: Module -> Doc
prettyModule (ProcType { proctypeActive = active
                       , proctypeName = name
                       , proctypeArguments = args
                       , proctypePriority = prio
                       , proctypeProvided = prov
                       , proctypeSteps = steps
                       }) = (case active of
                                Nothing -> empty
                                Just Nothing -> text "active"
                                Just (Just n) -> text "active" <+> brackets (integer n)) <+>
                            (text "proctype") <+>
                            (text name) <> parens (prettyDeclarations args) <+>
                            (case prio of
                                Nothing -> empty
                                Just n -> text "priority" <+> integer n) <+>
                            (case prov of
                                Nothing -> empty
                                Just expr -> text "provided" <+> parens (prettyExpression 0 expr)) <+>
                            lbrace $+$ (nest 2 $ prettySequence steps) $$ rbrace
prettyModule (CDecl decl) = text "c_decl" <+> lbrace $$ nest 2 (vcat $ map text $ lines decl) $$ rbrace
prettyModule (CState decl loc opt) = text "c_state" <+> text (show decl) <+> text (show loc) <+> (maybe empty (text.show) opt) <> semi
prettyModule (Init prio steps) = text "init" <+> (case prio of
                                                     Nothing -> empty
                                                     Just n -> text "priority" <+> integer n) <+>
                                 lbrace $$ nest 2 (prettySequence steps) $$ rbrace
prettyModule (Decl decl) = prettyDeclaration decl
prettyModule (Never steps) = text "never" <+> lbrace $$ nest 2 (prettySequence steps) $$ rbrace

prettyDeclarations :: [Declaration] -> Doc
prettyDeclarations = hsep . punctuate semi . map prettyDeclaration

prettyDeclaration :: Declaration -> Doc
prettyDeclaration decl = (case declarationVisible decl of
                             Nothing -> empty
                             Just True -> text "visible"
                             Just False -> text "hidden") <+>
                         prettyTypename (declarationType decl) <+>
                         (hsep $ punctuate comma (map (\(name,arrdecl,def) -> text name <>
                                                                             (case arrdecl of
                                                                                 Nothing -> empty
                                                                                 Just n -> brackets (integer n)) <+>
                                                                             (case def of
                                                                                 Nothing -> empty
                                                                                 Just expr -> equals <+> prettyAnyExpression 0 expr)) (declarationVariables decl)))

prettyExpression :: Int -> Expression -> Doc
prettyExpression prec (ExprAny expr) = prettyAnyExpression prec expr


prettyAnyExpression :: Int -> AnyExpression -> Doc
prettyAnyExpression prec (BinExpr op lhs rhs) = if opPrec op < prec
                                                then parens p
                                                else p
  where
    p = prettyAnyExpression (opPrec op) lhs <+> prettyBinOp op <+> prettyAnyExpression (opPrec op) rhs
prettyAnyExpression prec (RefExpr ref) = prettyVarRef ref
prettyAnyExpression prec (RunExpr name args prio) = text "run" <+> text name <> parens (hsep $ punctuate comma $ map (prettyAnyExpression 0) args) <+> 
                                                    (case prio of
                                                        Nothing -> empty
                                                        Just p -> text "priority" <+> integer p)
prettyAnyExpression prec (ConstExpr const) = prettyConst const

prettyConst :: Constant -> Doc
prettyConst (ConstBool x) = if x
                            then text "true"
                            else text "false"
prettyConst (ConstInt x) = integer x
  
prettyVarRef :: VarRef -> Doc
prettyVarRef (VarRef name index next) = text name <> 
                                        (case index of
                                            Nothing -> empty
                                            Just i -> brackets (integer i))<>
                                        (case next of
                                            Nothing -> empty
                                            Just n -> char '.' <> prettyVarRef n)

prettySequence :: [Step] -> Doc
prettySequence = vcat . punctuate semi . map prettyStep

prettyStep :: Step -> Doc
prettyStep (StepStmt stmt unless) = prettyStatement stmt <+> 
                                    (case unless of
                                        Nothing -> empty
                                        Just estmt -> text "unless" <+> prettyStatement estmt)
prettyStep (StepDecl decl) = prettyDeclaration decl

prettyStatement :: Statement -> Doc
prettyStatement (StmtIf cases) = text "if" $+$
                                 nest 2 (vcat (map (\c -> colon <> colon $$ nest 3 (prettySequence c)) cases)) $+$
                                 text "fi"
prettyStatement (StmtDo cases) = text "do" $+$
                                 nest 2 (vcat (map (\c -> colon <> colon $$ nest 3 (prettySequence c)) cases)) $+$
                                 text "od"
prettyStatement (StmtAtomic seq) = text "atomic" <+>
                                   lbrace $$ nest 2 (prettySequence seq) $$ rbrace
prettyStatement (StmtDStep seq) = text "d_step" <+>
                                   lbrace $$ nest 2 (prettySequence seq) $$ rbrace
prettyStatement StmtElse = text "else"
prettyStatement StmtBreak = text "break"
prettyStatement (StmtGoto lbl) = text "goto" <+> text lbl
prettyStatement (StmtLabel lbl stmt) = text lbl <> char ':' $+$
                                       nest 2 (prettyStatement stmt)
prettyStatement (StmtCCode code) = text "c_code" <+> lbrace $$ (nest 2 $ vcat $ map text (lines code)) $$ rbrace
prettyStatement (StmtExpr expr) = prettyExpression 0 expr
prettyStatement (StmtAssign var expr) = prettyVarRef var <+> text "=" <+> prettyAnyExpression 0 expr
prettyStatement (StmtReceive to args) = text to <> char '?' <> hcat (punctuate comma (map prettyRecvArg args))
prettyStatement (StmtSequence steps) = lbrace $$ nest 2 (prettySequence steps) $$ rbrace

prettyRecvArg :: RecvArg -> Doc
prettyRecvArg (RecvVar ref) = prettyVarRef ref
prettyRecvArg (RecvEval ref) = text "eval" <> parens (prettyVarRef ref)
prettyRecvArg (RecvConst c) = prettyConst c

prettyBinOp :: BinOp -> Doc
prettyBinOp op = text $ case op of
  BinOr -> "or"
  BinAnd -> "and"
  BinBitOr -> "|"
  BinExp -> "^"
  BinBitAnd -> "&"
  BinEquals -> "=="
  BinNotEquals -> "!="
  BinGT -> ">"
  BinLT -> "<"
  BinGTE -> ">="
  BinLTE -> "<="
  BinShiftL -> "<<"
  BinShiftR -> ">>"
  BinPlus -> "+"
  BinMinus -> "-"
  BinMult -> "*"
  BinDiv -> "/"
  BinMod -> "%"
    
opPrec :: BinOp -> Int
opPrec BinOr = 3
opPrec BinAnd = 4
opPrec BinBitOr = 5
opPrec BinExp = 6
opPrec BinBitAnd = 7
opPrec BinEquals = 8
opPrec BinNotEquals = 8
opPrec BinGT = 9
opPrec BinLT = 9
opPrec BinGTE = 9
opPrec BinLTE = 9
opPrec BinShiftL = 10
opPrec BinShiftR = 10
opPrec BinPlus = 11
opPrec BinMinus = 11
opPrec BinMult = 12
opPrec BinDiv = 12
opPrec BinMod = 12

prettyTypename :: Typename -> Doc
prettyTypename TypeBit = text "bit"
prettyTypename TypeBool = text "bool"
prettyTypename TypeByte = text "byte"
prettyTypename TypeShort = text "short"
prettyTypename TypeInt = text "int"
prettyTypename TypeMType = text "mtype"
prettyTypename TypeChan = text "chan"
prettyTypename (TypeCustom name) = text name
