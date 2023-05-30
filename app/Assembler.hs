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

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p = (p >>= \x -> many (many ws >> sep >> many ws >> p) >>= pure . (x:)) <|> pure []

data InstParam = ParamInt Integer
               | ParamFloat Float
               | ParamAddress Int
               | ParamString String

data InstParamDef = IntParamDef
                  | FloatParamDef
                  | AddressParamDef
                  | StringParamDef

data InstAdditionalInfo = InfoNothing
                        | InfoLabel String

type InstWrapper = (Inst, InstAdditionalInfo)

data InstDef = InstDef String [InstParamDef] ([InstParam] -> InstWrapper)

data Token = TokenInst InstWrapper
           | TokenLabel String
           | TokenComment
           | TokenForeign String [Frame] Frame

parseParam :: InstParamDef -> Parser InstParam
parseParam IntParamDef = some (digit <|> char '-') >>= pure . ParamInt . read
parseParam FloatParamDef = some (digit <|> char '-' <|> char '.') >>= pure . ParamFloat . read
parseParam AddressParamDef = some digit >>= pure . ParamAddress . read
parseParam StringParamDef = some (charF $ \c -> isAlphaNum c || c == '_') >>= pure . ParamString

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
    , InstDef "jmp"    [StringParamDef]  $ \[ParamString x] -> labelInst x $ InstJmp 0
    , InstDef "jz"     [StringParamDef]  $ \[ParamString x] -> labelInst x $ InstJmpZero 0
    , InstDef "jnz"    [StringParamDef]  $ \[ParamString x] -> labelInst x $ InstJmpNotZero 0
    , InstDef "call"   [StringParamDef]  $ \[ParamString x] -> labelInst x $ InstCall 0
    , InstDef "ext"    [StringParamDef]  $ \[ParamString x] -> simpleInst $ InstForeign x
    , InstDef "ret"    []                $ const $ simpleInst InstRet
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
    pure TokenComment

parseInt :: Parser Frame
parseInt = string "Int" >> pure (FrameInt 0)

parseFloat :: Parser Frame
parseFloat = string "Float" >> pure (FrameFloat 0)

parsePtr :: Parser Frame
parsePtr = string "Ptr" >> pure (FramePtr 0)

parseType :: Parser Frame
parseType = parseInt <|>
            parseFloat <|>
            parsePtr

foreignFunction :: Parser Token
foreignFunction = do
    many ws
    name <- some $ charF $ \c -> isAlphaNum c || c == '_'
    many ws
    string "::"
    many ws
    types <- sepBy (string "->") parseType
    pure $ TokenForeign name (init types) (last types)

parseProgram :: Parser [Token]
parseProgram = do
    tokens <- many $ instruction <|> label <|> comment <|> foreignFunction
    many ws
    pure tokens

splitTokens :: [Token] -> ([InstWrapper], [(String, Address)], [(String, [Frame], Frame)])
splitTokens tokens = go tokens 0 [] [] []
    where go [] _ insts labels foreigns = (insts, labels, foreigns)
          go ((TokenInst i):ts) addr insts labels foreigns =                go ts (addr + 1) (insts ++ [i]) labels                  foreigns
          go ((TokenLabel l):ts) addr insts labels foreigns =               go ts addr       insts          (labels ++ [(l, addr)]) foreigns
          go ((TokenForeign name args ret):ts) addr insts labels foreigns = go ts addr       insts          labels                  (foreigns ++ [(name, args, ret)])
          go (_:ts) addr insts labels foreigns =                            go ts addr       insts          labels                  foreigns

replaceLabels :: [InstWrapper] -> [(String, Address)] -> [Inst]
replaceLabels insts labels = map processInst insts
    where processInst (i, InfoNothing) = i
          processInst (InstJmp _, InfoLabel l) = maybe (error "label not found") InstJmp $ lookup l labels 
          processInst (InstJmpZero _, InfoLabel l) = maybe (error "label not found") InstJmpZero $ lookup l labels 
          processInst (InstJmpNotZero _, InfoLabel l) = maybe (error "label not found") InstJmpNotZero $ lookup l labels
          processInst (InstCall _, InfoLabel l) = maybe (error "label not found") InstCall $ lookup l labels
          processInst _ = undefined

deriving instance Generic Inst
instance Binary Inst

deriving instance Generic Frame
instance Binary Frame

deriving instance Show InstAdditionalInfo
deriving instance Show Inst
deriving instance Show Token

main :: IO ()
main = do
    args <- getArgs

    when (null args) $ putStrLn "fatal error: no input files" >> exitFailure

    input <- readFile $ head args 

    tokens <- case runParser parseProgram input of
                  Nothing -> pure []
                  Just ([], tokens) -> pure tokens
                  Just (rest, tokens) -> do
                      putStrLn "parse error:"
                      putStrLn $ "parsed: " ++ show tokens
                      putStrLn $ "not parsed: " ++ rest
                      exitFailure

    let (insts, labels, foreigns) = splitTokens tokens
    let prog = replaceLabels insts labels

    bc <- case lookup "main" labels of
              Nothing -> putStrLn "error: no entry" >> exitFailure
              Just entry -> pure (entry, foreigns, prog)

    if length args == 1 then
        encodeFile (head args -<.> ".bin") bc
    else
        encodeFile (args !! 1) bc
