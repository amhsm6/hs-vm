import Control.Monad
import Control.Applicative
import System.Exit
import System.Environment
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

data InstParam = ParamInt Int
               | ParamFloat Float
               | ParamLabel String

data InstParamDef = IntParamDef
                  | FloatParamDef
                  | LabelParamDef

data InstAdditionalInfo = InfoNothing
                        | InfoLabel String

type InstWrapper = (Inst, InstAdditionalInfo)

data InstDef = InstDef String [InstParamDef] ([InstParam] -> InstWrapper)

data Token = TokenInst InstWrapper
           | TokenLabel String

parseParam :: InstParamDef -> Parser InstParam
parseParam IntParamDef = some digit >>= pure . ParamInt . read
parseParam FloatParamDef = some (digit <|> char '.') >>= pure . ParamFloat . read
parseParam LabelParamDef = some (charF $ \c -> isAlphaNum c || c == '_') >>= pure . ParamLabel

parseInst :: InstDef -> Parser Token
parseInst (InstDef name params constructor) = do
    many ws
    string name
    parsedParams <- mapM (\param -> some ws >> parseParam param) params
    pure $ TokenInst $ constructor parsedParams

simpleInst :: Inst -> InstWrapper
simpleInst = (,InfoNothing)

labelInst :: String -> Inst -> InstWrapper
labelInst l = (,InfoLabel l)

instruction :: Parser Token
instruction = foldl (<|>) empty $ map parseInst $
    [ InstDef "pushf"  [FloatParamDef]  $ \[ParamFloat x] -> simpleInst $ InstPushF x
    , InstDef "push"   [IntParamDef]    $ \[ParamInt x]   -> simpleInst $ InstPushI x
    , InstDef "pop"    []               $ const $ simpleInst InstPop
    , InstDef "dup"    []               $ const $ simpleInst InstDup
    , InstDef "hlt"    []               $ const $ simpleInst InstHlt
    , InstDef "jmp"    [LabelParamDef]  $ \[ParamLabel x] -> labelInst x $ InstJmp 0
    , InstDef "jz"     [LabelParamDef]  $ \[ParamLabel x] -> labelInst x $ InstJmpZero 0
    , InstDef "jnz"    [LabelParamDef]  $ \[ParamLabel x] -> labelInst x $ InstJmpNotZero 0
    , InstDef "print"  []               $ const $ simpleInst InstPrint
    , InstDef "addf"   []               $ const $ simpleInst InstAddF
    , InstDef "subf"   []               $ const $ simpleInst InstSubF
    , InstDef "mulf"   []               $ const $ simpleInst InstMulF
    , InstDef "divf"   []               $ const $ simpleInst InstDivF
    , InstDef "gtf"    []               $ const $ simpleInst InstGtF
    , InstDef "gef"    []               $ const $ simpleInst InstGeF
    , InstDef "eqf"    []               $ const $ simpleInst InstEqF
    , InstDef "lef"    []               $ const $ simpleInst InstLeF
    , InstDef "ltf"    []               $ const $ simpleInst InstLtF
    , InstDef "add"    []               $ const $ simpleInst InstAddI
    , InstDef "sub"    []               $ const $ simpleInst InstSubI
    , InstDef "mul"    []               $ const $ simpleInst InstMulI
    , InstDef "div"    []               $ const $ simpleInst InstDivI
    , InstDef "mod"    []               $ const $ simpleInst InstModI
    , InstDef "gt"     []               $ const $ simpleInst InstGtI
    , InstDef "ge"     []               $ const $ simpleInst InstGeI
    , InstDef "eq"     []               $ const $ simpleInst InstEqI
    , InstDef "le"     []               $ const $ simpleInst InstLeI
    , InstDef "lt"     []               $ const $ simpleInst InstLtI
    ]

label :: Parser Token
label = do
    name <- some $ charF $ \c -> isAlphaNum c || c == '_'
    char ':'
    pure $ TokenLabel name

parseProgram :: Parser [Token]
parseProgram = many $ (instruction <|> label) >>= \token -> some ws >> pure token

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
          processInst (InstJmp _, InfoLabel l) = maybe (error "Label not found") InstJmp $ lookup l labels 
          processInst (InstJmpZero _, InfoLabel l) = maybe (error "Label not found") InstJmpZero $ lookup l labels 
          processInst (InstJmpNotZero _, InfoLabel l) = maybe (error "Label not found") InstJmpNotZero $ lookup l labels

main :: IO ()
main = do
    args <- getArgs

    when (null args) $ putStrLn "fatal error: no input files" >> exitFailure

    input <- readFile $ head args 

    tokens <- case runParser parseProgram input of
                  Just ([], tokens) -> pure tokens
                  _ -> putStrLn "parse error" >> exitFailure

    let (insts, labels) = processLabels tokens
    let prog = replaceLabels insts labels

    res <- execProg prog
    case res of
        Right _ -> pure ()
        Left e -> putStrLn $ "ERROR: " ++ show e
