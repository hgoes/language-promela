module Language.Promela.Tokens where

data Token = Key KeyWord
           | Identifier String
           | Uname String
           | Number Integer
           | Bracket BracketType Bool
           | Comma
           | Semicolon
           | Plus
           | Dot
           | Equals
           | DoubleColon
           | EOF
           deriving Show

data KeyWord = KeyActive
             | KeyBit
             | KeyBool
             | KeyByte
             | KeyChan
             | KeyDo
             | KeyFi
             | KeyHidden
             | KeyIf
             | KeyInit
             | KeyInt
             | KeyMType
             | KeyNever
             | KeyOd
             | KeyOf
             | KeyPriority
             | KeyProctype
             | KeyProvided
             | KeyShort
             | KeyShow
             | KeyTrace
             | KeyTypedef
             | KeyUnless
             deriving Show

data BracketType = Parentheses
                 | Square
                 | Curly
                 deriving Show
