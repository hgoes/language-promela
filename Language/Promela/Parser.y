{
module Language.Promela.Parser where

import Language.Promela.Tokens
import Language.Promela.Syntax
import Language.Promela.Lexer (Lexer,promelaLexer,runLexer,addUname,unames)
import Data.Set
import Control.Monad.State
import Data.ByteString.Lazy.Char8 as LBS
import Debug.Trace
}

%name promela
%tokentype { Token }
%lexer { promelaLexer } { EOF }
%monad { Lexer }
%error { parseError }

%token
  active   { Key KeyActive }
  bit      { Key KeyBit }
  bool     { Key KeyBool }
  byte     { Key KeyByte }
  chan     { Key KeyChan }
  do       { Key KeyDo }
  fi       { Key KeyFi }
  hidden   { Key KeyHidden }
  if       { Key KeyIf }
  int      { Key KeyInt }
  mtype    { Key KeyMType }
  od       { Key KeyOd }
  priority { Key KeyPriority }
  proctype { Key KeyProctype }
  provided { Key KeyProvided }
  short    { Key KeyShort }
  show     { Key KeyShow }
  typedef  { Key KeyTypedef }
  unless   { Key KeyUnless }
  '['      { Bracket Square False }
  ']'      { Bracket Square True }
  '('      { Bracket Parentheses False }
  ')'      { Bracket Parentheses True }
  '{'      { Bracket Curly False }
  '}'      { Bracket Curly True }
  ','      { Comma }
  ';'      { Semicolon }
  '+'      { Plus }
  '.'      { Dot }
  '='      { Equals }
  '::'      { DoubleColon }
  Number   { Number $$ }
  Id       { Identifier $$ }
  Uname    { Uname $$ }
 
%left '+'

%%

Document : Module Document { $1:$2}
         | ';' Document    { $2 }
         |                 { [] }

Module : Proctype    { $1 }
       | Declaration { Decl $1 }
       | Utype       { $1 }
         
Proctype : Active proctype Id '(' DeclListOpt ')' PriorityOpt ProvidedOpt '{' Sequence '}' { ProcType $1 $3 $5 $7 $8 $10 }

Utype : typedef Id '{' DeclList '}' {% addUname $2 >> return (Typedef $2 $4) }

Active : active IndexOpt { Just $2 }
       |                 { Nothing }

IndexOpt : '[' Number ']' { Just $2 }
         |                { Nothing }

PriorityOpt : priority Number { Just $2 }
            |                 { Nothing }

ProvidedOpt : provided '(' Expr ')' { Just $3 }
            |                       { Nothing }

DeclListOpt : DeclList { $1 }
            |          { [] }

DeclList : Declaration DeclListN { $1:$2 }

DeclListN : ';' Declaration DeclListN { $2:$3 }
          |                           { [] }

Declaration : Visible Typename VarDecls { Declaration $1 $2 $3 }

Visible : show   { Just True }
        | hidden { Just False }
        |        { Nothing }

Typename : bit   { TypeBit }
         | bool  { TypeBool }
         | byte  { TypeByte }
         | short { TypeShort }
         | int   { TypeInt }
         | mtype { TypeMType }
         | chan  { TypeChan }
         | Uname { TypeCustom $1 }

VarDecls : VarDecl VarDeclsN { $1:$2 }

VarDeclsN : ',' VarDecl VarDecls { $2:$3 }
          |                      { [] }

VarDecl : Id IndexOpt DefaultExprOpt { ($1,$2,$3) }

DefaultExprOpt : '=' AnyExpr { Just $2 }
               |             { Nothing }

Expr : AnyExpr { ExprAny $1 }

AnyExpr : '(' AnyExpr ')'     { $2 }
        | AnyExpr '+' AnyExpr { BinExpr BinPlus $1 $3 }
        | VarRef              { RefExpr $1 }

VarRef : Id IndexOpt VarRefExt { VarRef $1 $2 $3 }

VarRefExt : '.' VarRef { Just $2 }
          |            { Nothing }

Sequence : Step StepsN { $1:$2 }

StepsN : ';' Step StepsN { $2:$3 }
       |                 { [] }

Step : Statement                  { StepStmt $1 Nothing }
     | Statement unless Statement { StepStmt $1 (Just $3) }
     | Declaration                { StepDecl $1 }

Statement : if Options fi { StmtIf $2 }
          | do Options od { StmtDo $2 }
          | Expr          { StmtExpr $1 }

Options : '::' Sequence Options { $2:$3 }
        |                      { [] }

{
parseError xs = error "Parse error"

parsePromela :: String -> [Module]
parsePromela str = runLexer promela (LBS.pack str)
}