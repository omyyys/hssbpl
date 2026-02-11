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
    { progFuncs    :: [Function]
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
    | TChar
    | TVoid
    | TPtr Type
    | TAny
    deriving (Eq, Show)

data Block = Block
    { blockStmts :: [Statement]
    , blockPos   :: Position
    } deriving (Eq, Show)

data Statement
    = SLet [String] Position
    | SDeclare String Type [Expr] Position
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

data TopLevelItem
    = ItemFunc Function
    | ItemStmt Statement
    deriving (Show)

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
        { progFuncs    = [func | ItemFunc func <- items]
        , progMain     = [stmt | ItemStmt stmt <- items]
        }

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
        TokFunc    -> ItemFunc <$> parseFunction
        _          -> ItemStmt <$> parseStatement

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
    tok <- nextToken
    case tokenType tok of
        TokStdop s -> parse_type_string (tokenPos tok) s
        TokIdent s -> parse_type_string (tokenPos tok) s
        _ -> parserErrorAt (tokenPos tok) "expected type"
  where
    parse_type_string pos s =
        case s of
            "int64"   -> return TInt64
            "float64" -> return TFloat64
            "char"    -> return TChar
            "void"    -> return TVoid
            "any"     -> return TAny
            _ ->
                case strip_prefix "ptr@" s of
                    Just rest -> do
                        inner <- parse_type_string pos rest
                        return (TPtr inner)
                    Nothing ->
                        parserErrorAt pos ("unknown type: " ++ s)

strip_prefix :: String -> String -> Maybe String
strip_prefix pref str =
    if take (length pref) str == pref
        then Just (drop (length pref) str)
        else Nothing

parseBlock :: Parser Block
parseBlock = do
    pos <- currentPos
    stmts <- parseStatements
    consume TokEnd
    return $ Block stmts pos

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

parseStatement :: Parser Statement
parseStatement = do
    tok <- peek
    case tokenType tok of
        TokLet     -> parseLetStatement
        TokDeclare -> parseDeclareStatement
        TokIf      -> parseIfStatement
        TokWhile   -> parseWhileStatement
        _          -> do
            expr <- parseExpr
            consumeEOL
            return $ SExpr expr

parseIfStatement :: Parser Statement
parseIfStatement = do
    pos <- currentPos
    consume TokIf
    condition <- parseUntilThen
    consume TokThen
    consumeEOL
    thenStmts <- parseStatements
    skipEOLs
    tok <- peek
    elseStmts <- case tokenType tok of
        TokElse -> do
            advance
            consumeEOL
            parseStatements
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

parseDeclareStatement :: Parser Statement
parseDeclareStatement = do
    pos <- currentPos
    consume TokDeclare
    name <- expectIdent
    elemType <- parseType
    countExpr <- parseUntilEOL
    consumeEOL
    return (SDeclare name elemType countExpr pos)

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
        _      -> return ()

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
    map showFunction (progFuncs prog) ++
    ["<main code>"]

showFunction :: Function -> String
showFunction (Function name (TypeSig ins outs) _body _pos) =
    "func " ++ name ++ " " ++
    unwords (map showType ins) ++ " : " ++
    unwords (map showType outs) ++ " in ... end"

showType :: Type -> String
showType TInt64    = "int64"
showType TFloat64  = "float64"
showType TChar     = "char"
showType TVoid     = "void"
showType (TPtr t)  = "ptr@" ++ showType t
showType TAny      = "any"
