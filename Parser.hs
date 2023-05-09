module Parser where

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
        [] -> Nothing
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
               | ParamFloat Float
               | ParamLabel String

data InstParamDef = IntParamDef
                  | FloatParamDef
                  | LabelParamDef

parseParam :: InstParamDef -> Parser InstParam
parseParam IntParamDef = some digit >>= pure . ParamInt . read
parseParam FloatParamDef = some (digit <|> char '.') >>= pure . ParamFloat . read
parseParam LabelParamDef = some (charF $ \c -> isAlphaNum c || c == '_') >>= pure . ParamLabel

data Token = TokenInst Inst
           | TokenLabel String

data InstDef = InstDef String [InstParamDef] ([InstParam] -> Inst)

parseInst :: InstDef -> Parser Token
parseInst (InstDef name params constructor) = do
    many ws
    string name
    parsedParams <- mapM (\param -> some ws >> parseParam param) params
    pure $ TokenInst $ constructor parsedParams

instruction :: Parser Token
instruction = foldl (<|>) empty $ map parseInst $
    [ InstDef "pushf"  [FloatParamDef]  $ \[ParamFloat x] -> InstPushF x
    , InstDef "push"   [IntParamDef]    $ \[ParamInt x]   -> InstPushI x
    , InstDef "pop"    []               $ const InstPop
    , InstDef "dup"    []               $ const InstDup
    , InstDef "hlt"    []               $ const InstHlt
    , InstDef "jmp"    [LabelParamDef]  $ \[ParamLabel x] -> InstJmp x
    , InstDef "jz"     [LabelParamDef]  $ \[ParamLabel x] -> InstJmpZero x
    , InstDef "jnz"    [LabelParamDef]  $ \[ParamLabel x] -> InstJmpNoZero x
    , InstDef "print"  []               $ const InstPrint
    , InstDef "addf"   []               $ const InstAddF
    , InstDef "subf"   []               $ const InstSubF
    , InstDef "mulf"   []               $ const InstMulF
    , InstDef "divf"   []               $ const InstDivF
    , InstDef "gtf"    []               $ const InstGtF
    , InstDef "gef"    []               $ const InstGeF
    , InstDef "eqf"    []               $ const InstEqF
    , InstDef "lef"    []               $ const InstLeF
    , InstDef "ltf"    []               $ const InstLtF
    , InstDef "add"    []               $ const InstAddI
    , InstDef "sub"    []               $ const InstSubI
    , InstDef "mul"    []               $ const InstMulI
    , InstDef "div"    []               $ const InstDivI
    , InstDef "mod"    []               $ const InstModI
    , InstDef "gt"     []               $ const InstGtI
    , InstDef "ge"     []               $ const InstGeI
    , InstDef "eq"     []               $ const InstEqI
    , InstDef "le"     []               $ const InstLeI
    , InstDef "lt"     []               $ const InstLtI
    ]

label :: Parser Token
label = do
    name <- some $ charF $ \c -> isAlphaNum c || c == '_'
    char ':'
    pure $ TokenLabel name

parseProgram :: Parser [Token]
parseProgram = many $ (instruction <|> label) >>= \token -> some ws >> pure token
