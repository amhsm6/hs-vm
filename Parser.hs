module Parser where

import Control.Monad
import Control.Applicative
import Data.Char

import Core

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Monad Parser where
    return = pure
    (Parser f) >>= g = Parser $ \s -> f s >>= \(s', x) -> runParser (g x) s'

instance Applicative Parser where
    pure x = Parser $ \s -> Just (s, x)
    p1 <*> p2 = p1 >>= \f -> p2 >>= pure . f

instance Functor Parser where
    fmap f p = p >>= pure . f

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser f1) <|> (Parser f2) = Parser $ \s -> f1 s <|> f2 s

charF :: (Char -> Bool) -> Parser Char
charF f = Parser $ \s ->
    case s of
        "" -> Nothing
        (x:xs)
            | f x -> Just (xs, x)
            | otherwise -> Nothing

char :: Char -> Parser Char
char = charF . (==)

ws :: Parser Char
ws = charF isSpace

digit :: Parser Char
digit = charF isDigit

string :: String -> Parser String
string s = mapM char s

data InstParam = ParamInt Int
               | ParamLabel String

data InstParamDef = IntParamDef
                  | LabelParamDef

parseParam :: InstParamDef -> Parser InstParam
parseParam IntParamDef = some digit >>= pure . ParamInt . read
parseParam LabelParamDef = (some $ charF (not . isSpace)) >>= pure . ParamLabel

data InstDef = InstDef String [InstParamDef] ([InstParam] -> Inst)

parseInst :: InstDef -> Parser Inst
parseInst (InstDef name params constructor) = do
    many ws
    string name
    parsedParams <- mapM (\param -> some ws >> parseParam param) params
    pure $ constructor parsedParams

instruction :: Parser Inst
instruction = foldl (<|>) empty $ map parseInst $
    [ InstDef "push"  [IntParamDef] $ \[ParamInt x] -> InstPush x
    , InstDef "pop"   []            $ const InstPop
    , InstDef "print" []            $ const InstPrint
    , InstDef "add"   []            $ const InstAdd
    , InstDef "sub"   []            $ const InstSub
    , InstDef "mul"   []            $ const InstMul
    , InstDef "div"   []            $ const InstDiv
    , InstDef "mod"   []            $ const InstMod
    , InstDef "jmp"   [IntParamDef] $ \[ParamInt x] -> InstJmp x
    , InstDef "hlt"   []            $ const InstHlt
    ]

parseProgram :: Parser [Inst]
parseProgram = many $ instruction >>= \inst -> some ws >> pure inst
