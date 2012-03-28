{
module Language.Promela.Lexer where

import Language.Promela.Tokens
import Data.ByteString.Lazy.Char8 as LBS
import Control.Monad.State
import Data.Set as Set
import Debug.Trace
}

$letter = [a-zA-Z\_]
$digit10 = 0-9

tokens :-
  $white+                                  ;
  active                                   { key KeyActive }
  bit                                      { key KeyBit }
  bool                                     { key KeyBool }
  byte                                     { key KeyByte }
  chan                                     { key KeyChan }
  do                                       { key KeyDo }
  fi                                       { key KeyFi }
  hidden                                   { key KeyHidden }
  if                                       { key KeyIf }
  init                                     { key KeyInit }
  int                                      { key KeyInt }
  mtype                                    { key KeyMType }
  never                                    { key KeyNever }
  od                                       { key KeyOd }
  of                                       { key KeyOf }
  priority                                 { key KeyPriority }
  proctype                                 { key KeyProctype }
  short                                    { key KeyShort }
  show                                     { key KeyShow }
  trace                                    { key KeyTrace }
  typedef                                  { key KeyTypedef }
  unless                                   { key KeyUnless }
  "("                                      { token $ Bracket Parentheses False }
  ")"                                      { token $ Bracket Parentheses True }
  "["                                      { token $ Bracket Square False }
  "]"                                      { token $ Bracket Square True }
  "{"                                      { token $ Bracket Curly False }
  "}"                                      { token $ Bracket Curly True }
  ","                                      { token Comma }
  ";"                                      { token Semicolon }
  "+"                                      { token Plus }
  "."                                      { token Dot }
  "="                                      { token Equals }
  "::"                                     { token DoubleColon }
  $letter($letter|$digit10)*               { \len -> do
                                               tok <- getTok len
                                               st <- get
                                               if Set.member tok (unames st)
                                                 then return $ Uname tok
                                                 else return $ Identifier tok }
  $digit10+                                { \len -> do
                                               tok <- getTok len
                                               return $ Number $ read tok }
{

data LexerPos = LexerPos !Int !Int deriving Show

data LexerState = LexerState
                  { pos :: !LexerPos
                  , input :: LBS.ByteString
                  , curChar :: !Char
                  , curCode :: !Int
                  , unames :: Set String
                  } deriving Show

type Lexer a = State LexerState a

type AlexInput = (LexerPos,Char,LBS.ByteString)

lexerStartPos :: LexerPos
lexerStartPos = LexerPos 1 0

runLexer :: Lexer a -> LBS.ByteString -> a
runLexer lex bs = evalState lex (LexerState lexerStartPos bs '\n' 0 Set.empty)

lexerMove :: LexerPos -> Char -> LexerPos
lexerMove (LexerPos l c) '\t' = LexerPos l (((c+7) `div` 8)*8+1)
lexerMove (LexerPos l c) '\n' = LexerPos (l+1) 1
lexerMove (LexerPos l c) _    = LexerPos l (c+1)

alexGetByte :: AlexInput -> Maybe (Int,AlexInput)
alexGetByte inp = case alexGetChar inp of
  Nothing -> Nothing
  Just (ch,inp') -> Just (ord ch,inp')

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (pos,c,inp)
  | LBS.null inp = Nothing
  | otherwise = let c  = LBS.head inp
                    cs = LBS.tail inp
                    p  = lexerMove pos c
                in p `seq` cs `seq` Just (c, (p,c,cs))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_) = c

scanToken :: Lexer Token
scanToken = do
  st <- get
  case alexScan (pos st,curChar st,input st) (curCode st) of
    AlexEOF -> return EOF
    AlexError (LexerPos l c,curC,rest) -> error $ "Lexical error in line "++show l++", column "++show c
    AlexSkip (npos,curC,rest) len -> do
      put (st { pos = npos
              , input = rest
              , curChar = curC
              })
      scanToken
    AlexToken (npos,curC,rest) len act -> do
      tok <- act len
      modify (\cst -> cst { pos = npos
                          , input = rest
                          , curChar = curC
                          })
      return tok
      

getTok :: Int -> Lexer String
getTok len = do
  st <- get
  return $ LBS.unpack $ LBS.take (fromIntegral len) (input st)

promelaLexer :: (Token -> Lexer a) -> Lexer a
promelaLexer f = do
  tok <- scanToken
  f tok

getPos :: Lexer (Int,Int)
getPos = do
  LexerState { pos = LexerPos l c } <- get
  return (l,c)

addUname :: String -> Lexer ()
addUname name = modify (\st -> st { unames = Set.insert name (unames st) })

token :: Token -> Int -> Lexer Token
token t _ = return t

key :: KeyWord -> Int -> Lexer Token
key w _ = return $ Key w

}