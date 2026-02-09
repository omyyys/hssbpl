module Lexer
    ( Token(..)
    , TokenType(..)
    , Position(..)
    , lexer
    , showToken
    , showTokens
    ) where

import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum)
import Control.Monad (when, filterM)
import System.FilePath (takeDirectory, (</>))
import System.Directory (doesFileExist, canonicalizePath)
import qualified Data.Set as Set

data Position = Position 
    { posFile :: String
    , posLine :: Int
    , posCol  :: Int
    } deriving (Eq)

instance Show Position where
    show (Position file line col) = file ++ ":" ++ show line ++ ":" ++ show col

data Token = Token 
    { tokenType :: TokenType
    , tokenPos  :: Position
    , tokenRaw  :: String    
    } deriving (Eq, Show)

data TokenType
    = TokIntLit Integer
    | TokFloatLit Double
    | TokStringLit String 
    | TokCharLit Char
    
    | TokDefine
    | TokFunc
    | TokIn
    | TokEnd
    | TokLet
    | TokIf
    | TokThen
    | TokElse
    | TokWhile
    | TokDo
    | TokInclude
    | TokDeclare
    
    | TokStdop String
    | TokIdent String
    
    | TokColon
    | TokEOL
    | TokEOF
    
    deriving (Eq, Show)

data LexerState = LexerState
    { lsInput    :: String
    , lsPos      :: Position
    , lsTokens   :: [Token]
    , lsIncluded :: Set.Set FilePath
    , lsPosStack :: [Position]  -- stack of positions to restore after includes
    , lsDefines  :: [(String, String)] 
    } deriving (Show)

includeEOF :: Char
includeEOF = '\x00'

newtype Lexer a = Lexer { runLexer :: LexerState -> IO (Either String (a, LexerState)) }

instance Functor Lexer where
    fmap f (Lexer g) = Lexer $ \s -> do
        result <- g s
        return $ case result of
            Left err -> Left err
            Right (a, s') -> Right (f a, s')

instance Applicative Lexer where
    pure a = Lexer $ \s -> return $ Right (a, s)
    (Lexer f) <*> (Lexer g) = Lexer $ \s -> do
        resultF <- f s
        case resultF of
            Left err -> return $ Left err
            Right (fab, s') -> do
                resultG <- g s'
                return $ case resultG of
                    Left err -> Left err
                    Right (a, s'') -> Right (fab a, s'')

instance Monad Lexer where
    return = pure
    (Lexer f) >>= k = Lexer $ \s -> do
        resultF <- f s
        case resultF of
            Left err -> return $ Left err
            Right (a, s') -> runLexer (k a) s'

initialState :: String -> String -> LexerState
initialState filename input = LexerState
    { lsInput    = input
    , lsPos      = Position filename 1 1
    , lsTokens   = []
    , lsIncluded = Set.singleton filename
    , lsPosStack = []
    , lsDefines  = []
    }

lexer :: String -> String -> IO (Either String [Token])
lexer filename input = 
    let baseDir = takeDirectory filename
    in do
        result <- runLexer (lexAll baseDir) (initialState filename input)
        return $ case result of
            Left err -> Left err
            Right (tokens, _) -> Right (reverse tokens)

lexAll :: String -> Lexer [Token]
lexAll baseDir = do
    skipWhitespaceAndComments
    state <- getLexerState
    case lsInput state of
        [] -> do
            tok <- makeToken TokEOF ""
            addToken tok
            getTokens
        (c:_) | c == includeEOF -> do
            case lsPosStack state of
                (savedPos:rest) -> do
                    putLexerState $ state 
                        { lsInput = tail (lsInput state)  -- skip the marker
                        , lsPos = savedPos
                        , lsPosStack = rest 
                        }
                    lexAll baseDir
                [] -> lexerError "internal error: includeEOF marker without saved position"
        _ -> do
            lexToken baseDir
            lexAll baseDir

lexToken :: String -> Lexer ()
lexToken baseDir = do
    state <- getLexerState
    case lsInput state of
        [] -> return ()
        ('"':_) -> lexStringLiteral
        ('\'':_) -> lexCharLiteral
        (':':_) -> do
            tok <- makeToken TokColon ":"
            addToken tok
            advance 1

        ('\n':_) -> do 
            tok <- makeToken TokEOL "\\n"
            addToken tok
            advance 1

        ('-':c:_) | isDigit c -> lexNumber
        (c:_) | isDigit c -> lexNumber
        (c:_) | isIdentStart c -> lexWordOrPreprocess baseDir
        (c:_) | isSymbolChar c -> lexSymbol
        (c:_) -> lexerError $ "unexpected character " ++ [c]

isSymbolChar :: Char -> Bool
isSymbolChar c = c `elem` "+-*/%=!<>&|"

lexSymbol :: Lexer ()
lexSymbol = do
    state <- getLexerState
    let input = lsInput state
    let op = case input of
            ('!':'=':_) -> "!="
            ('<':'=':_) -> "<="
            ('>':'=':_) -> ">="
            ('&':'&':_) -> "&&"
            ('|':'|':_) -> "||"
            (c:_) | isSymbolChar c -> [c]
            _ -> ""

    if null op
        then lexerError "internal error: lexSymbol called on non-symbol"
        else do
            tok <- makeToken (TokStdop op) op
            addToken tok
            advance (length op)

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_'

isIdentCont :: Char -> Bool
isIdentCont c = isAlphaNum c || c `elem` "_*&?!$()"

includeSearchPaths :: [FilePath]
includeSearchPaths = [ "/home/omar/hssbpl/std" ]

lexWordOrPreprocess :: String -> Lexer ()
lexWordOrPreprocess baseDir = do
    word <- takeWhile1 isIdentCont

    if word == "include" then processInclude baseDir
    else if word == "define" then processDefine
    else do
        state <- getLexerState
        case lookup word (lsDefines state) of
            Just replacement -> do
                putLexerState $ state { lsInput = replacement ++ lsInput state }
            Nothing -> do
                tok <- classifyWord word
                addToken tok

resolveInclude :: FilePath -> FilePath -> IO (Maybe FilePath)
resolveInclude baseDir includePath = do
    let primary = if null baseDir then includePath else baseDir </> includePath

    let candidates = primary : [ dir </> includePath | dir <- includeSearchPaths ]
    existing <- filterM doesFileExist candidates

    return $ case existing of
        (p:_) -> Just p
        []    -> Nothing

processInclude :: String -> Lexer ()
processInclude baseDir = do
    skipWhitespaceAndComments
    state <- getLexerState

    case lsInput state of
        ('"':_) -> do
            advance 1
            includePath <- takeUntilChar '"'
            advance 1
            skipToEOL

            mPath <- liftIO $ resolveInclude baseDir includePath

            fullPath <- case mPath of
                Nothing -> lexerError $ "include file not found: " ++ includePath
                Just p  -> liftIO $ canonicalizePath p

            st <- getLexerState

            if Set.member fullPath (lsIncluded st) then return ()
            else do
                content <- liftIO $ readFile fullPath

                -- save current position to stack before switching to included file
                let savedPos = lsPos st
                let newPosStack = savedPos : lsPosStack st

                putLexerState st
                    { lsIncluded = Set.insert fullPath (lsIncluded st)
                    , lsInput    = content ++ [includeEOF] ++ lsInput st
                    , lsPos      = Position fullPath 1 1
                    , lsPosStack = newPosStack
                    }

        _ -> lexerError "expected string literal after 'include'"

processDefine :: Lexer ()
processDefine = do
    skipWhitespaceAndComments
    state <- getLexerState
    
    case lsInput state of
        (c:_) | isIdentStart c -> do
            macroName <- takeWhile1 isIdentCont
            skipWhitespaceAndComments
            
            replacement <- takeUntilEOL
            st <- getLexerState
            putLexerState $ st { lsDefines = (macroName, replacement) : lsDefines st }
            
        _ -> lexerError "expected identifier after 'define'"

takeUntilEOL :: Lexer String
takeUntilEOL = do
    state <- getLexerState
    let (taken, rest) = span (/= '\n') (lsInput state)
    advance (length taken)
    case rest of
        ('\n':_) -> advance 1
        _ -> return ()
    return taken

liftIO :: IO a -> Lexer a
liftIO action = Lexer $ \s -> do
    result <- action
    return $ Right (result, s)

takeUntilChar :: Char -> Lexer String
takeUntilChar endChar = do
    state <- getLexerState
    let (taken, _) = span (/= endChar) (lsInput state)
    advance (length taken)
    return taken

skipToEOL :: Lexer ()
skipToEOL = do
    state <- getLexerState
    case lsInput state of
        [] -> return ()
        ('\n':_) -> advance 1
        _ -> advance 1 >> skipToEOL

classifyWord :: String -> Lexer Token
classifyWord word = makeToken tokType word
  where
    tokType = case word of
        "func"    -> TokFunc
        "in"      -> TokIn
        "end"     -> TokEnd
        "let"     -> TokLet
        "if"      -> TokIf
        "then"    -> TokThen
        "else"    -> TokElse
        "while"   -> TokWhile
        "do"      -> TokDo
        "declare" -> TokDeclare
        _ -> if isStdop word
             then TokStdop word
             else TokIdent word

isStdop :: String -> Bool
isStdop word = word `elem` stdops

stdops :: [String]
stdops =
    [ "+", "-", "*", "/", "%"
    , "=", "!=", "<", ">", "<=", ">="
    , "&", "|", "!", "&&", "||"
    , "dup", "drop", "swap", "over", "reach"
    , "int64", "float64", "ptr", "char"
    , "int64*", "int64&"
    , "float64*", "float64&"
    , "char*", "char&"
    , "ptr*", "ptr&"
    , "print"
    , "__syscall0__", "__syscall1__", "__syscall2__", "__syscall3__"
    , "__syscall4__", "__syscall5__", "__syscall6__"
    ]

lexNumber :: Lexer ()
lexNumber = do
    state <- getLexerState
    let input = lsInput state
    let (sign, rest1) = case input of
            ('-':r) -> ("-", r)
            _       -> ("", input)

    let (intPart, rest2) = span isDigit rest1
    case rest2 of
        ('.':c:_) | isDigit c -> do
            let (fracPart, _) = span isDigit (tail rest2)
            let numStr = sign ++ intPart ++ "." ++ fracPart
            let value = read numStr :: Double
            tok <- makeToken (TokFloatLit value) numStr
            addToken tok
            advance (length numStr)

        ('.':_) -> lexerError "invalid float literal: expected digit after decimal point"
        _ -> do
            let numStr = sign ++ intPart
            let value = read numStr :: Integer
            tok <- makeToken (TokIntLit value) numStr
            addToken tok
            advance (length numStr)

lexStringLiteral :: Lexer ()
lexStringLiteral = do
    startPos <- getCurrentPos
    advance 1 
    result <- lexStringContents []
    case result of
        Left err -> lexerError err
        Right str -> do
            state <- getLexerState
            case lsInput state of
                ('"':_) -> do
                    tok <- makeToken (TokStringLit str) ("\"" ++ escapeStringForDisplay str ++ "\"")
                    addToken tok
                    advance 1
                [] -> lexerErrorAt startPos "unterminated string literal (missing closing '\"')"
                _ -> lexerError "internal error in string lexing"

lexCharLiteral :: Lexer ()
lexCharLiteral = do
    startPos <- getCurrentPos
    advance 1
    state <- getLexerState
    case lsInput state of
        [] -> lexerErrorAt startPos "unterminated character literal (missing closing '\'')"
        ('\\':c:_) -> case c of
            'n'  -> lexCharHelper '\n' "\\n"
            't'  -> lexCharHelper '\t' "\\t"
            'r'  -> lexCharHelper '\r' "\\r"
            '\\' -> lexCharHelper '\\' "\\\\"
            '\'' -> lexCharHelper '\'' "\\'"
            '0'  -> lexCharHelper '\0' "\\0"
            _    -> lexerError $ "invalid escape sequence '\\" ++ [c] ++ "' in character literal"

        (c:'\'':_) -> do
            tok <- makeToken (TokCharLit c) ("\'" ++ [c] ++ "\'")
            addToken tok
            advance 2 

        (c:[]) -> lexerErrorAt startPos "unterminated character literal (missing closing '\'')"
        (c:rest) -> lexerError $ "character literal must contain exactly one character (found '" ++ [c] ++ "' followed by '" ++ take 1 rest ++ "')"
  where
    lexCharHelper ch rawChars = do
        state <- getLexerState
        case lsInput state of
            ('\'':_) -> do
                tok <- makeToken (TokCharLit ch) ("\'" ++ rawChars ++ "\'")
                addToken tok
                advance (length rawChars + 1)
            _ -> lexerError "expected closing '\'' after character escape sequence"

lexStringContents :: String -> Lexer (Either String String)
lexStringContents acc = do
    state <- getLexerState
    case lsInput state of
        [] -> return $ Right (reverse acc)
        ('"':_) -> return $ Right (reverse acc)
        ('\\':c:_) -> case c of
            'n'  -> escapeHelper '\n'
            't'  -> escapeHelper '\t'
            'r'  -> escapeHelper '\r'
            '\\' -> escapeHelper '\\'
            '"'  -> escapeHelper '"'
            '0'  -> escapeHelper '\0'
            _    -> return $ Left $ "invalid escape sequence '\\" ++ [c] ++ "' in string literal"

        ('\\':[]) -> return $ Left "incomplete escape sequence at end of string"
        ('\n':_) -> return $ Left "unterminated string literal (newline in string)"
        (c:_) -> do
            advance 1
            lexStringContents (c:acc)
  where
    escapeHelper ch = do
        advance 2
        lexStringContents (ch:acc)

escapeStringForDisplay :: String -> String
escapeStringForDisplay = concatMap escapeChar
  where
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar '\0' = "\\0"
    escapeChar c    = [c]

skipWhitespaceAndComments :: Lexer ()
skipWhitespaceAndComments = do
    state <- getLexerState
    case lsInput state of
        [] -> return ()
        (';':_) -> skipLineComment >> skipWhitespaceAndComments
        (c:_) | c `elem` " \t\r" -> advance 1 >> skipWhitespaceAndComments
        (c:_) | c == includeEOF -> return ()  -- Don't skip the marker here, let lexAll handle it
        _ -> return ()

skipLineComment :: Lexer ()
skipLineComment = do
    state <- getLexerState
    case lsInput state of
        [] -> return ()
        ('\n':_) -> return () 
        _ -> advance 1 >> skipLineComment

takeWhile1 :: (Char -> Bool) -> Lexer String
takeWhile1 pred = do
    state <- getLexerState
    let (taken, _) = span pred (lsInput state)
    if null taken
        then lexerError "expected at least one character (internal lexer error)"
        else do
            advance (length taken)
            return taken

advance :: Int -> Lexer ()
advance n = do
    state <- getLexerState
    let (consumed, rest) = splitAt n (lsInput state)
    let newPos = foldl updatePos (lsPos state) consumed
    putLexerState $ state { lsInput = rest, lsPos = newPos }
  where
    updatePos (Position file line col) '\n' = Position file (line + 1) 1
    updatePos (Position file line col) _    = Position file line (col + 1)

makeToken :: TokenType -> String -> Lexer Token
makeToken tokType raw = do
    state <- getLexerState
    return $ Token tokType (lsPos state) raw

addToken :: Token -> Lexer ()
addToken tok = do
    state <- getLexerState
    putLexerState $ state { lsTokens = tok : lsTokens state }

getTokens :: Lexer [Token]
getTokens = do
    state <- getLexerState
    return (lsTokens state)

getCurrentPos :: Lexer Position
getCurrentPos = do
    state <- getLexerState
    return (lsPos state)

getLexerState :: Lexer LexerState
getLexerState = Lexer $ \s -> return $ Right (s, s)

putLexerState :: LexerState -> Lexer ()
putLexerState s = Lexer $ \_ -> return $ Right ((), s)

lexerError :: String -> Lexer a
lexerError msg = do
    pos <- getCurrentPos
    lexerErrorAt pos msg

lexerErrorAt :: Position -> String -> Lexer a
lexerErrorAt pos msg = Lexer $ \_ -> return $ Left $ formatError pos msg

formatError :: Position -> String -> String
formatError pos msg = show pos ++ ": error: " ++ msg

showToken :: Token -> String
showToken (Token tokType pos raw) =
    show pos ++ ": " ++ show tokType ++ " (raw: \"" ++ raw ++ "\")"

showTokens :: [Token] -> String
showTokens = unlines . map showToken
