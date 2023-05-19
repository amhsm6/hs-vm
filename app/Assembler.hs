{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Monad
import Control.Applicative
import System.Exit
import System.Environment
import System.FilePath
import GHC.Generics
import Data.Binary
import Data.Char
import Data.List

import Engine

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

data InstParam = ParamInt Integer
               | ParamFloat Float
               | ParamAddress Int
               | ParamLabel String

data InstParamDef = IntParamDef
                  | FloatParamDef
                  | AddressParamDef
                  | LabelParamDef

data InstAdditionalInfo = InfoNothing
                        | InfoLabel String
                        deriving Show

type InstWrapper = (Inst, InstAdditionalInfo)

data InstDef = InstDef String [InstParamDef] ([InstParam] -> InstWrapper)

data Token = TokenNothing
           | TokenInst InstWrapper
           | TokenLabel String
           deriving Show

parseParam :: InstParamDef -> Parser InstParam
parseParam IntParamDef = some (digit <|> char '-') >>= pure . ParamInt . read
parseParam FloatParamDef = some (digit <|> char '-' <|> char '.') >>= pure . ParamFloat . read
parseParam AddressParamDef = some digit >>= pure . ParamAddress . read
parseParam LabelParamDef = some (charF $ \c -> isAlphaNum c || c == '_') >>= pure . ParamLabel

parseInst :: InstDef -> Parser Token
parseInst (InstDef name params constructor) = do
    many ws
    string name
    some ws
    parsedParams <- mapM (\param -> many ws >> parseParam param) params
    pure $ TokenInst $ constructor parsedParams

simpleInst :: Inst -> InstWrapper
simpleInst = (,InfoNothing)

labelInst :: String -> Inst -> InstWrapper
labelInst l = (,InfoLabel l)

instruction :: Parser Token
instruction = foldl (<|>) empty $ map parseInst $
    [ InstDef "pushf"  [FloatParamDef]   $ \[ParamFloat x] -> simpleInst $ InstPushF x
    , InstDef "push"   [IntParamDef]     $ \[ParamInt x]   -> simpleInst $ InstPushI x
    , InstDef "pop"    []                $ const $ simpleInst InstPop
    , InstDef "dup"    [AddressParamDef] $ \[ParamAddress x] -> simpleInst $ InstDup x
    , InstDef "swap"   [AddressParamDef] $ \[ParamAddress x] -> simpleInst $ InstSwap x
    , InstDef "hlt"    []                $ const $ simpleInst InstHlt
    , InstDef "jmp"    [LabelParamDef]   $ \[ParamLabel x] -> labelInst x $ InstJmp 0
    , InstDef "jz"     [LabelParamDef]   $ \[ParamLabel x] -> labelInst x $ InstJmpZero 0
    , InstDef "jnz"    [LabelParamDef]   $ \[ParamLabel x] -> labelInst x $ InstJmpNotZero 0
    , InstDef "print"  []                $ const $ simpleInst InstPrint
    , InstDef "addf"   []                $ const $ simpleInst InstAddF
    , InstDef "subf"   []                $ const $ simpleInst InstSubF
    , InstDef "mulf"   []                $ const $ simpleInst InstMulF
    , InstDef "divf"   []                $ const $ simpleInst InstDivF
    , InstDef "gtf"    []                $ const $ simpleInst InstGtF
    , InstDef "gef"    []                $ const $ simpleInst InstGeF
    , InstDef "eqf"    []                $ const $ simpleInst InstEqF
    , InstDef "lef"    []                $ const $ simpleInst InstLeF
    , InstDef "ltf"    []                $ const $ simpleInst InstLtF
    , InstDef "add"    []                $ const $ simpleInst InstAddI
    , InstDef "sub"    []                $ const $ simpleInst InstSubI
    , InstDef "mul"    []                $ const $ simpleInst InstMulI
    , InstDef "div"    []                $ const $ simpleInst InstDivI
    , InstDef "mod"    []                $ const $ simpleInst InstModI
    , InstDef "gt"     []                $ const $ simpleInst InstGtI
    , InstDef "ge"     []                $ const $ simpleInst InstGeI
    , InstDef "eq"     []                $ const $ simpleInst InstEqI
    , InstDef "le"     []                $ const $ simpleInst InstLeI
    , InstDef "lt"     []                $ const $ simpleInst InstLtI
    ]

label :: Parser Token
label = do
    many ws
    name <- some $ charF $ \c -> isAlphaNum c || c == '_'
    char ':'
    pure $ TokenLabel name

comment :: Parser Token
comment = do
    many ws
    char ';'
    many $ charF (/= '\n')
    pure TokenNothing

parseProgram :: Parser [Token]
parseProgram = do
    tokens <- many $ instruction <|> label <|> comment
    many ws

    let f TokenNothing = False
        f _ = True
    pure $ filter f tokens

processLabels :: [Token] -> ([InstWrapper], [(String, Address)])
processLabels prog = (map (\(_, TokenInst i) -> i) insts, map (\(addr, TokenLabel l) -> (l, addr)) labels)
    where (insts, labels) = partition isInst $ tail $ scanl address (0, TokenLabel "") prog
          isInst (_, (TokenInst _)) = True
          isInst (_, (TokenLabel _)) = False
          address (addr, _) t@(TokenInst _) = (addr + 1, t)
          address (addr, _) t@(TokenLabel _) = (addr, t)

replaceLabels :: [InstWrapper] -> [(String, Address)] -> [Inst]
replaceLabels insts labels = map processInst insts
    where processInst (i, InfoNothing) = i
          processInst (InstJmp _, InfoLabel l) = maybe (error "label not found") InstJmp $ lookup l labels 
          processInst (InstJmpZero _, InfoLabel l) = maybe (error "label not found") InstJmpZero $ lookup l labels 
          processInst (InstJmpNotZero _, InfoLabel l) = maybe (error "label not found") InstJmpNotZero $ lookup l labels

deriving instance Generic Inst
instance Binary Inst

deriving instance Show Inst

main :: IO ()
main = do
    args <- getArgs

    when (null args) $ putStrLn "fatal error: no input files" >> exitFailure

    input <- readFile $ head args 

    tokens <- case runParser parseProgram input of
                  Just ([], tokens) -> pure tokens
                  Just (rest, tokens) -> do
                      putStrLn "parse error:"
                      putStrLn $ "parsed: " ++ show tokens
                      putStrLn $ "not parsed: " ++ rest
                      exitFailure

    let (insts, labels) = processLabels tokens
    let prog = replaceLabels insts labels

    if length args == 1 then
        encodeFile (head args -<.> ".bin") prog
    else
        encodeFile (args !! 1) prog
