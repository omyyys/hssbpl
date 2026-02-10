module Parser
    ( Program(..)
    , Function(..)
    , TypeSig(..)
    , Type(..)
    , Block(..)
    , Statement(..)
    , Expr(..)
    , parseProgram
    , showProgram
    ) where

import Lexer
import Control.Monad (when)
import Data.List (intercalate)

data Program = Program
    { progDecls    :: [(String, Integer)]  
    , progFuncs    :: [Function]          
    , progMain     :: [Statement]     
    } deriving (Eq, Show)

data Function = Function
    { funcName :: String
    , funcSig  :: TypeSig
    , funcBody :: Block
    , funcPos  :: Position
    } deriving (Eq, Show)

data TypeSig = TypeSig
    { sigInputs  :: [Type]
    , sigOutputs :: [Type]
    } deriving (Eq, Show)

data Type
    = TInt64
    | TFloat64
    | TPtr
    | TChar
    | TAny
    deriving (Eq, Show)

data Block = Block
    { blockStmts :: [Statement]
    , blockPos   :: Position
    } deriving (Eq, Show)

data Statement
    = SLet [String] Position  
    | SIf Block (Maybe Block) [Expr] Position 
    | SWhile Block [Expr] Position  
    | SExpr Expr 
    deriving (Eq, Show)

data Expr
    = EIntLit Integer Position
    | EFloatLit Double Position
    | EStringLit String Position
    | ECharLit Char Position
    | EStdop String Position
    | EIdent String Position
    deriving (Eq, Show)

data ParserState = ParserState
    { psTokens :: [Token]
    , psPos    :: Position
    } deriving (Show)

newtype Parser a = Parser { runParser :: ParserState -> Either String (a, ParserState) }

instance Functor Parser where
    fmap f (Parser g) = Parser $ \s -> case g s of
        Left err -> Left err
        Right (a, s') -> Right (f a, s')

instance Applicative Parser where
    pure a = Parser $ \s -> Right (a, s)
    (Parser f) <*> (Parser g) = Parser $ \s -> case f s of
        Left err -> Left err
        Right (fab, s') -> case g s' of
            Left err -> Left err
            Right (a, s'') -> Right (fab a, s'')

instance Monad Parser where
    return = pure
    (Parser f) >>= k = Parser $ \s -> case f s of
        Left err -> Left err
        Right (a, s') -> runParser (k a) s'

parseProgram :: String -> [Token] -> Either String Program
parseProgram filename tokens = case runParser program (initialParserState tokens) of
    Left err -> Left err
    Right (prog, _) -> Right prog
  where
    initialParserState toks = ParserState
        { psTokens = toks
        , psPos = Position filename 1 1
        }

program :: Parser Program
program = do
    skipEOLs
    items <- parseTopLevelItems
    eof
    return $ groupItems items
  where
    groupItems items = Program
        { progDecls    = [(name, size) | ItemDeclare name size <- items]
        , progFuncs    = [func | ItemFunc func <- items]
        , progMain     = [stmt | ItemStmt stmt <- items]
        }

data TopLevelItem
    = ItemDeclare String Integer
    | ItemFunc Function
    | ItemStmt Statement
    deriving (Show)

parseTopLevelItems :: Parser [TopLevelItem]
parseTopLevelItems = do
    tok <- peek
    case tokenType tok of
        TokEOF -> return []
        _ -> do
            item <- parseTopLevelItem
            skipEOLs
            rest <- parseTopLevelItems
            return (item : rest)

parseTopLevelItem :: Parser TopLevelItem
parseTopLevelItem = do
    tok <- peek
    case tokenType tok of
        TokDeclare -> parseDeclare
        TokFunc    -> ItemFunc <$> parseFunction
        _          -> ItemStmt <$> parseStatement

parseDeclare :: Parser TopLevelItem
parseDeclare = do
    pos <- currentPos
    consume TokDeclare
    name <- expectIdent
    tok <- nextToken
    case tokenType tok of
        TokIntLit size | size > 0 -> do
            consumeEOL
            return $ ItemDeclare name size
        TokIntLit _ -> parserErrorAt pos "declare size must be positive"
        _ -> parserErrorAt pos "expected integer literal for declare size"

parseUntilEOL :: Parser [Expr]
parseUntilEOL = do
    tok <- peek
    case tokenType tok of
        TokEOL -> return []
        TokEOF -> return []
        _      -> do
            expr <- parseExpr
            rest <- parseUntilEOL
            return (expr : rest)

parseFunction :: Parser Function
parseFunction = do
    pos <- currentPos
    consume TokFunc
    name <- expectIdent
    sig <- parseTypeSig
    consumeEOL
    consume TokIn
    consumeEOL
    body <- parseBlock
    return $ Function name sig body pos

parseTypeSig :: Parser TypeSig
parseTypeSig = do
    inputs <- parseTypes
    consume TokColon
    outputs <- parseTypes
    return $ TypeSig inputs outputs

parseTypes :: Parser [Type]
parseTypes = many parseType

parseType :: Parser Type
parseType = do
    tok <- peek
    case tokenType tok of
        TokStdop "int64"   -> advance >> return TInt64
        TokStdop "float64" -> advance >> return TFloat64
        TokStdop "ptr"     -> advance >> return TPtr
        TokStdop "char"    -> advance >> return TChar
        _ -> parserError "expected type (int64, float64, ptr, or char)"

parseBlock :: Parser Block
parseBlock = do
    pos <- currentPos
    stmts <- parseStatements
    consume TokEnd
    return $ Block stmts pos

parseStatement :: Parser Statement
parseStatement = do
    tok <- peek
    case tokenType tok of
        TokLet   -> parseLetStatement
        TokIf    -> parseIfStatement
        TokWhile -> parseWhileStatement
        _        -> do
            expr <- parseExpr
            consumeEOL
            return $ SExpr expr

parseStatements :: Parser [Statement]
parseStatements = do
    skipEOLs
    tok <- peek
    case tokenType tok of
        TokEnd  -> return []
        TokElse -> return []
        _ -> do
            stmt <- parseStatement
            rest <- parseStatements
            return (stmt : rest)

parseIfStatement :: Parser Statement
parseIfStatement = do
    pos <- currentPos
    consume TokIf
    condition <- parseUntilThen
    consume TokThen
    consumeEOL
    thenStmts <- parseStatements  -- don't parse a full block, just statements
    skipEOLs
    tok <- peek
    elseStmts <- case tokenType tok of
        TokElse -> do
            advance
            consumeEOL
            parseStatements  -- parse statements until end
        _ -> return []

    consume TokEnd
    consumeEOL
    
    let thenBlock = Block thenStmts pos
    let elseBlock = if null elseStmts then Nothing else Just (Block elseStmts pos)
    
    return $ SIf thenBlock elseBlock condition pos

  where
    parseUntilThen = do
        tok <- peek
        case tokenType tok of
            TokThen -> return []
            _       -> do
                expr <- parseExpr
                rest <- parseUntilThen
                return (expr : rest)

parseWhileStatement :: Parser Statement
parseWhileStatement = do
    pos <- currentPos
    consume TokWhile
    condition <- parseUntilDo
    consume TokDo
    consumeEOL
    body <- parseBlock
    return $ SWhile body condition pos
  where
    parseUntilDo = do
        tok <- peek
        case tokenType tok of
            TokDo -> return []
            _     -> do
                expr <- parseExpr
                rest <- parseUntilDo
                return (expr : rest)

parseLetStatement :: Parser Statement
parseLetStatement = do
    pos <- currentPos
    consume TokLet
    vars <- parseVarList
    consumeEOL
    return $ SLet vars pos
  where
    parseVarList = do
        tok <- peek
        case tokenType tok of
            TokIdent name -> do
                advance
                rest <- parseVarList
                return (name : rest)
            _ -> return []

parseExpr :: Parser Expr
parseExpr = do
    tok <- nextToken
    case tokenType tok of
        TokIntLit n    -> return $ EIntLit n (tokenPos tok)
        TokFloatLit f  -> return $ EFloatLit f (tokenPos tok)
        TokStringLit s -> return $ EStringLit s (tokenPos tok)
        TokCharLit c   -> return $ ECharLit c (tokenPos tok)
        TokStdop op    -> return $ EStdop op (tokenPos tok)
        TokIdent name  -> return $ EIdent name (tokenPos tok)
        _ -> parserErrorAt (tokenPos tok) $ 
            "unexpected token in expression: " ++ show (tokenType tok)

skipEOLs :: Parser ()
skipEOLs = do
    tok <- peek
    case tokenType tok of
        TokEOL -> advance >> skipEOLs
        _ -> return ()

consumeEOL :: Parser ()
consumeEOL = do
    tok <- peek
    case tokenType tok of
        TokEOL -> advance
        TokEOF -> return ()  
        _ -> return ()  

many :: Parser a -> Parser [a]
many p = do
    result <- optional p
    case result of
        Nothing -> return []
        Just x  -> do
            xs <- many p
            return (x : xs)

optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> p) `catchParser` (\_ -> return Nothing)

catchParser :: Parser a -> (String -> Parser a) -> Parser a
catchParser (Parser p) handler = Parser $ \s -> case p s of
    Left err -> runParser (handler err) s
    Right result -> Right result

peek :: Parser Token
peek = Parser $ \s -> case psTokens s of
    [] -> Left "unexpected end of input"
    (t:_) -> Right (t, s)

nextToken :: Parser Token
nextToken = Parser $ \s -> case psTokens s of
    [] -> Left "unexpected end of input"
    (t:ts) -> Right (t, s { psTokens = ts, psPos = tokenPos t })

advance :: Parser ()
advance = nextToken >> return ()

consume :: TokenType -> Parser ()
consume expected = do
    tok <- nextToken
    when (tokenType tok /= expected) $
        parserErrorAt (tokenPos tok) $
            "expected " ++ show expected ++ " but got " ++ show (tokenType tok)

expectIdent :: Parser String
expectIdent = do
    tok <- nextToken
    case tokenType tok of
        TokIdent name -> return name
        _ -> parserErrorAt (tokenPos tok) "expected identifier"

eof :: Parser ()
eof = do
    tok <- peek
    case tokenType tok of
        TokEOF -> return ()
        _ -> parserErrorAt (tokenPos tok) "expected end of file"

currentPos :: Parser Position
currentPos = Parser $ \s -> Right (psPos s, s)

parserError :: String -> Parser a
parserError msg = do
    pos <- currentPos
    parserErrorAt pos msg

parserErrorAt :: Position -> String -> Parser a
parserErrorAt pos msg = Parser $ \_ -> Left $ show pos ++ ": error: " ++ msg

showProgram :: Program -> String
showProgram prog = unlines $
    map (\(name, size) -> "declare " ++ name ++ " " ++ show size) (progDecls prog) ++
    map showFunction (progFuncs prog) ++
    ["<main code>"]

showFunction :: Function -> String
showFunction (Function name (TypeSig ins outs) body pos) =
    "func " ++ name ++ " " ++
    unwords (map showType ins) ++ " : " ++
    unwords (map showType outs) ++ " in ... end"

showType :: Type -> String
showType TInt64   = "int64"
showType TFloat64 = "float64"
showType TPtr     = "ptr"
showType TChar    = "char"
