module Typecheck
    ( typecheckProgram
    , TypeError(..)
    , sizeofType
    , showType
    , evalDeclareCount
    ) where

import qualified Eval
import Parser
import Lexer (Token(..), TokenType(..), Position(..))
import qualified Data.Map as Map
import Data.List (intercalate)

data TypeError = TypeError Position String deriving (Eq)

instance Show TypeError where
    show (TypeError pos msg) = show pos ++ ": type error: " ++ msg

type TypeStack = [Type]
type SymbolTable = Map.Map String Type
type FunctionTable = Map.Map String TypeSig

data TypecheckState = TypecheckState
    { tcStack :: TypeStack
    , tcSymbols :: SymbolTable
    , tcFunctions :: FunctionTable
    , tcGlobalSymbols :: SymbolTable
    , tcInFunction :: Bool
    } deriving (Show)

initialState :: TypecheckState
initialState = TypecheckState
    { tcStack = []
    , tcSymbols = Map.empty
    , tcFunctions = Map.empty
    , tcGlobalSymbols = Map.empty
    , tcInFunction = False
    }

typecheckProgram :: Program -> Either TypeError Program
typecheckProgram prog =
    let state0 = initialState
        state1 = foldl collectFunctionSig state0 (progFuncs prog)
    in do
        checkedFuncs <- mapM (typecheckFunction state1) (progFuncs prog)
        (checkedMain, _) <- typecheckStatements (progMain prog) state1
        return prog { progFuncs = checkedFuncs, progMain = checkedMain }

collectFunctionSig :: TypecheckState -> Function -> TypecheckState
collectFunctionSig state func =
    state { tcFunctions = Map.insert (funcName func) (funcSig func) (tcFunctions state) }

typecheckFunction :: TypecheckState -> Function -> Either TypeError Function
typecheckFunction state func = do
    let sig = funcSig func
        inputTypes = sigInputs sig
        state1 = state
            { tcStack = reverse inputTypes
            , tcSymbols = Map.empty
            , tcInFunction = True
            }

    (checkedBody, finalState) <- typecheckBlock (funcBody func) state1

    let expectedOutputs = sigOutputs sig
        actualOutputs = tcStack finalState

    if actualOutputs /= expectedOutputs
        then Left $ TypeError (funcPos func) $
            "function " ++ funcName func ++ " has incorrect output types\n" ++
            "  expected: " ++ showTypeStack expectedOutputs ++ "\n" ++
            "  actual:   " ++ showTypeStack actualOutputs
        else return func { funcBody = checkedBody }

typecheckBlock :: Block -> TypecheckState -> Either TypeError (Block, TypecheckState)
typecheckBlock (Block stmts pos) state = do
    (checkedStmts, finalState) <- typecheckStatements stmts state
    return (Block checkedStmts pos, finalState)

typecheckStatements :: [Statement] -> TypecheckState -> Either TypeError ([Statement], TypecheckState)
typecheckStatements [] state = return ([], state)
typecheckStatements (stmt:stmts) state = do
    (checkedStmt, state1) <- typecheckStatement stmt state
    (checkedRest, state2) <- typecheckStatements stmts state1
    return (checkedStmt : checkedRest, state2)

typecheckStatement :: Statement -> TypecheckState -> Either TypeError (Statement, TypecheckState)
typecheckStatement (SLet vars pos) state = do
    let stack = tcStack state
    if length vars > length stack
        then Left $ TypeError pos $
            "let binding requires " ++ show (length vars) ++ " values but stack only has " ++
            show (length stack) ++ " elements"
        else do
            let (types, restStack) = splitAt (length vars) stack
                bindings = zip (reverse vars) types
                newSymbols = foldl (\m (v, t) -> Map.insert v t m) (tcSymbols state) bindings
                globalSyms = if tcInFunction state
                    then tcGlobalSymbols state
                    else foldl (\m (v, t) -> Map.insert v t m) (tcGlobalSymbols state) bindings

            return (SLet vars pos, state
                { tcStack = restStack
                , tcSymbols = newSymbols
                , tcGlobalSymbols = globalSyms
                })

typecheckStatement (SDeclare name elemType countExprs pos) state = do
    -- reject duplicates in the same scope
    if Map.member name (tcSymbols state) || (not (tcInFunction state) && Map.member name (tcGlobalSymbols state))
        then Left $ TypeError pos ("redeclaration of " ++ name)
        else do
            n <- evalDeclareCount pos countExprs

            if n <= 0
                then Left $ TypeError pos ("declare object_count must be positive, got " ++ show n)
                else do
                    let ptrType = TPtr elemType
                        newLocal = Map.insert name ptrType (tcSymbols state)
                        newGlobal =
                            if tcInFunction state
                                then tcGlobalSymbols state
                                else Map.insert name ptrType (tcGlobalSymbols state)

                    -- no stack effect
                    pure (SDeclare name elemType countExprs pos, state
                        { tcSymbols = newLocal
                        , tcGlobalSymbols = newGlobal
                        })

typecheckStatement (SIf thenBlock elseBlock cond pos) state = do
    (checkedCond, state1) <- typecheckExprs cond state
    case tcStack state1 of
        [] -> Left $ TypeError pos "if condition produces no value"
        (_condType:restStack) -> do
            let state2 = state1 { tcStack = restStack }
            (checkedThen, thenState) <- typecheckBlock thenBlock state2

            case elseBlock of
                Nothing ->
                    if tcStack thenState /= tcStack state2
                        then Left $ TypeError pos $
                            "if without else must not change stack\n" ++
                            "  before: " ++ showTypeStack (tcStack state2) ++ "\n" ++
                            "  after:  " ++ showTypeStack (tcStack thenState)
                        else return (SIf checkedThen Nothing checkedCond pos, state2)

                Just elseBlk -> do
                    (checkedElse, elseState) <- typecheckBlock elseBlk state2
                    if tcStack thenState /= tcStack elseState
                        then Left $ TypeError pos $
                            "if/else branches have different stack effects\n" ++
                            "  then: " ++ showTypeStack (tcStack thenState) ++ "\n" ++
                            "  else: " ++ showTypeStack (tcStack elseState)
                        else return (SIf checkedThen (Just checkedElse) checkedCond pos, thenState)

typecheckStatement (SWhile body cond pos) state = do
    (checkedCond, state1) <- typecheckExprs cond state
    case tcStack state1 of
        [] -> Left $ TypeError pos "while condition produces no value"
        (_:restStack) -> do
            let state2 = state1 { tcStack = restStack }
            (checkedBody, bodyState) <- typecheckBlock body state2
            if tcStack bodyState /= tcStack state2
                then Left $ TypeError pos $
                    "while loop body changes stack\n" ++
                    "  before: " ++ showTypeStack (tcStack state2) ++ "\n" ++
                    "  after:  " ++ showTypeStack (tcStack bodyState)
                else return (SWhile checkedBody checkedCond pos, state)

typecheckStatement (SExpr expr) state = do
    (checkedExpr, state1) <- typecheckExpr expr state
    return (SExpr checkedExpr, state1)

typecheckExprs :: [Expr] -> TypecheckState -> Either TypeError ([Expr], TypecheckState)
typecheckExprs [] state = return ([], state)
typecheckExprs (expr:exprs) state = do
    (checkedExpr, state1) <- typecheckExpr expr state
    (checkedRest, state2) <- typecheckExprs exprs state1
    return (checkedExpr : checkedRest, state2)

typecheckExpr :: Expr -> TypecheckState -> Either TypeError (Expr, TypecheckState)
typecheckExpr (EIntLit n pos) state =
    return (EIntLit n pos, state { tcStack = TInt64 : tcStack state })

typecheckExpr (EFloatLit f pos) state =
    return (EFloatLit f pos, state { tcStack = TFloat64 : tcStack state })

typecheckExpr (ECharLit c pos) state =
    return (ECharLit c pos, state { tcStack = TChar : tcStack state })

typecheckExpr (EStringLit s pos) state =
    return (EStringLit s pos, state { tcStack = TPtr TChar : tcStack state })

typecheckExpr (EStdop op pos) state = do
    (transformedOp, newStack) <- typecheckStdop op pos (tcStack state)
    return (EStdop transformedOp pos, state { tcStack = newStack })

typecheckExpr (EIdent name pos) state =
    case Map.lookup name (tcSymbols state) of
        Just varType ->
            return (EIdent name pos, state { tcStack = varType : tcStack state })
        Nothing ->
            case Map.lookup name (tcGlobalSymbols state) of
                Just varType ->
                    return (EIdent name pos, state { tcStack = varType : tcStack state })
                Nothing ->
                    case Map.lookup name (tcFunctions state) of
                        Just sig -> do
                            let inputs = sigInputs sig
                                outputs = sigOutputs sig
                                stack = tcStack state
                                n = length inputs

                            if length stack < n
                                then Left $ TypeError pos $
                                    "function " ++ name ++ " requires " ++ show n ++
                                    " arguments but stack only has " ++ show (length stack)
                                else do
                                    let (topTypes, restStack) = splitAt n stack
                                        arg_types = reverse topTypes
                                    if arg_types /= inputs
                                        then Left $ TypeError pos $
                                            "function " ++ name ++ " expects arguments " ++
                                            showTypeStack inputs ++ " but got " ++ showTypeStack arg_types
                                        else return (EIdent name pos, state { tcStack = outputs ++ restStack })
                                    
                        Nothing -> Left $ TypeError pos $
                            "undefined symbol or function: " ++ name

apply_stack_effect :: Int -> [Type] -> TypeStack -> TypeStack
apply_stack_effect n outs stack = outs ++ drop n stack

typecheckStdop :: String -> Position -> TypeStack -> Either TypeError (String, TypeStack)
typecheckStdop op pos stack = case op of
    "+"  -> binaryArithmetic op pos stack
    "-"  -> binaryArithmetic op pos stack
    "*"  -> binaryArithmetic op pos stack
    "/"  -> binaryArithmetic op pos stack
    "%"  -> requireTypes pos [TInt64, TInt64] stack >> return ("%", apply_stack_effect 2 [TInt64] stack)

    "="  -> binaryComparison op pos stack
    "!=" -> binaryComparison op pos stack
    "<"  -> binaryComparison op pos stack
    "<=" -> binaryComparison op pos stack
    ">"  -> binaryComparison op pos stack
    ">=" -> binaryComparison op pos stack

    "&"  -> requireTypes pos [TInt64, TInt64] stack >> return ("&",  apply_stack_effect 2 [TInt64] stack)
    "|"  -> requireTypes pos [TInt64, TInt64] stack >> return ("|",  apply_stack_effect 2 [TInt64] stack)
    "!"  -> requireTypes pos [TInt64]         stack >> return ("!",  apply_stack_effect 1 [TInt64] stack)
    "&&" -> requireTypes pos [TInt64, TInt64] stack >> return ("&&", apply_stack_effect 2 [TInt64] stack)
    "||" -> requireTypes pos [TInt64, TInt64] stack >> return ("||", apply_stack_effect 2 [TInt64] stack)

    "dup" -> case stack of
        []       -> Left $ TypeError pos "dup requires at least 1 element on stack"
        (t:rest) -> return ("dup", t : t : rest)

    "drop" -> case stack of
        []       -> Left $ TypeError pos "drop requires at least 1 element on stack"
        (_:rest) -> return ("drop", rest)

    "swap" -> case stack of
        (a:b:rest) -> return ("swap", b : a : rest)
        _          -> Left $ TypeError pos "swap requires at least 2 elements on stack"

    "over" -> case stack of
        (a:b:rest) -> return ("over", b : a : b : rest)
        _          -> Left $ TypeError pos "over requires at least 2 elements on stack"

    "reach" -> case stack of
        (a:b:c:rest) -> return ("reach", c : a : b : rest)
        _            -> Left $ TypeError pos "reach requires at least 3 elements on stack"


    "??" -> typecheck_deref pos stack
    "!!" -> typecheck_store pos stack

    "print" -> requireTypes pos [TInt64] stack >> return ("print", apply_stack_effect 1 [] stack)

    "int64" -> case stack of
        (TFloat64:rest) -> return ("ftoi", TInt64 : rest)
        (_:rest)        -> return ("int64", TInt64 : rest)
        []              -> Left $ TypeError pos "int64 cast requires value on stack"

    "float64" -> case stack of
        (TInt64:rest)   -> return ("float64", TFloat64 : rest)
        (TFloat64:rest) -> return ("float64", TFloat64 : rest)
        _               -> Left $ TypeError pos "float64 cast requires int64 on stack"

    "char" -> case stack of
        (TInt64:rest) -> return ("char", TChar : rest)
        (TChar:rest)  -> return ("char", TChar : rest)
        _             -> Left $ TypeError pos "char cast requires int64 on stack"
 
    "__syscall0__" -> requireTypes pos [TInt64] stack >>
        return ("__syscall0__", apply_stack_effect 1 [TInt64] stack)
    
    "__syscall1__" -> requireTypes pos [TInt64, TAny] stack >>
        return ("__syscall1__", apply_stack_effect 2 [TInt64] stack)

    "__syscall2__" -> requireTypes pos [TInt64, TAny, TAny] stack >>
        return ("__syscall1__", apply_stack_effect 3 [TInt64] stack)

    "__syscall3__" -> requireTypes pos [TInt64, TAny, TAny, TAny] stack >>
        return ("__syscall3__", apply_stack_effect 4 [TInt64] stack)

    "__syscall4__" -> requireTypes pos [TInt64, TAny, TAny, TAny, TAny] stack >>
        return ("__syscall4__", apply_stack_effect 5 [TInt64] stack)

    _ | Just target <- parsePtrCast op ->
            case stack of
                (_:rest) -> return (op, TPtr target : rest)
                []       -> Left $ TypeError pos (op ++ "pointer cast requires value on stack")

    _ -> Left $ TypeError pos $ "unknown stdop: " ++ op

typecheck_deref :: Position -> TypeStack -> Either TypeError (String, TypeStack)
typecheck_deref pos stack = case stack of
    (TPtr TVoid : _) -> Left $ TypeError pos "cannot dereference ptr@void (unknown pointee type)"
    (TPtr TAny  : _) -> Left $ TypeError pos "cannot dereference ptr@any (cast to a concrete ptr@T first)"
    (TPtr t : rest) ->
        case t of
            TInt64       -> return ("int64*",   TInt64   : rest)
            TFloat64     -> return ("float64*", TFloat64 : rest)
            TChar        -> return ("char*",    TChar    : rest)
            TVoid        -> Left $ TypeError pos "cannot dereference ptr@void"
            TAny         -> Left $ TypeError pos "cannot dereference ptr@any"
            TPtr inner   -> return ("ptr*",     TPtr inner : rest)

    _ -> Left $ TypeError pos "?? requires ptr@T on stack"

typecheck_store :: Position -> TypeStack -> Either TypeError (String, TypeStack)
typecheck_store pos stack = case stack of
    (_val : TPtr TVoid : _) -> Left $ TypeError pos "cannot store through ptr@void (unknown pointee type)"
    (_val : TPtr TAny  : _) -> Left $ TypeError pos "cannot store through ptr@any (cast ptr to ptr@T first)"
    (val : TPtr t : rest) ->
        if types_match t val
            then case t of
                TInt64     -> return ("int64&",   rest)
                TFloat64   -> return ("float64&", rest)
                TChar      -> return ("char&",    rest)
                TPtr inner -> return ("ptr&",     rest)
                TVoid      -> Left $ TypeError pos "cannot store in ptr@void"
                TAny       -> Left $ TypeError pos "cannot store in ptr@any"

            else Left $ TypeError pos $
                "store type mismatch: expected " ++ showType (TPtr t) ++
                " but value is " ++ showType val

    _ -> Left $ TypeError pos "!! requires (value, ptr@value_type) on stack"
  where
    types_match expected actual =
        case (expected, actual) of
            (TAny, _) -> True
            (_, TAny) -> True
            _         -> expected == actual

parsePtrCast :: String -> Maybe Type
parsePtrCast op =
    case stripPrefix "ptr@" op of
        Just rest -> parseTypeString rest
        Nothing   -> Nothing

parseTypeString :: String -> Maybe Type
parseTypeString s = case s of
    "int64"   -> Just TInt64
    "float64" -> Just TFloat64
    "char"    -> Just TChar
    "void"    -> Just TVoid
    "any"     -> Just TAny
    _ ->
        case stripPrefix "ptr@" s of
            Just rest -> TPtr <$> parseTypeString rest
            Nothing   -> Nothing

stripPrefix :: String -> String -> Maybe String
stripPrefix pref str =
    if take (length pref) str == pref
        then Just (drop (length pref) str)
        else Nothing

same_type :: Type -> Type -> Bool
same_type a b =
    case (a, b) of
        (TAny, _) -> True
        (_, TAny) -> True
        _         -> a == b

binaryArithmetic :: String -> Position -> TypeStack -> Either TypeError (String, TypeStack)
binaryArithmetic op pos stack = case stack of
    (TFloat64:TFloat64:rest) -> return ("f" ++ op, TFloat64 : rest)
    (TInt64:TInt64:rest)     -> return (op, TInt64 : rest)

    (TInt64:TChar:rest)      -> return (op, TChar : rest)
    (TChar:TInt64:rest)      -> return (op, TChar : rest)

    (TInt64 : TPtr t : rest) | op == "+" || op == "-" -> do
        n <- case sizeofType t of
            Just n  -> Right n
            Nothing -> Left $ TypeError pos ("cannot do pointer arithmetic on ptr@" ++ showType t)

        return ("p" ++ op ++ show n, TPtr t : rest)

    (TPtr t : TInt64 : rest) | op == "+" -> do
        n <- case sizeofType t of
            Just n  -> Right n
            Nothing -> Left $ TypeError pos ("cannot do pointer arithmetic on ptr@" ++ showType t)

        return ("p+" ++ show n, TPtr t : rest)

    (TPtr t : TInt64 : rest) | op == "-" -> do
        n <- case sizeofType t of
            Just n  -> Right n
            Nothing -> Left $ TypeError pos ("cannot do pointer arithmetic on ptr@" ++ showType t)

        return ("p-" ++ show n, TPtr t : rest)

    (TPtr t1 : TPtr t2 : rest) | op == "-" -> do
        n1 <- case sizeofType t1 of
            Just n1  -> Right n1
            Nothing -> Left $ TypeError pos ("cannot do pointer arithmetic on ptr@" ++ showType t1)

        n2 <- case sizeofType t2 of
            Just n2  -> Right n2
            Nothing -> Left $ TypeError pos ("cannot do pointer arithmetic on ptr@" ++ showType t2)

        return (op, TInt64 : rest)

    (t1:t2:_) -> Left $ TypeError pos $
        "arithmetic operator " ++ op ++ " type mismatch, got " ++ show t2 ++ " and " ++ show t1
    _ -> Left $ TypeError pos $
        "arithmetic operator " ++ op ++ " requires at least 2 elements on stack"

binaryComparison :: String -> Position -> TypeStack -> Either TypeError (String, TypeStack)
binaryComparison op pos stack = case stack of
    (TFloat64:TFloat64:rest) -> return ("f" ++ op, TInt64 : rest)
    (TInt64:TInt64:rest)     -> return (op, TInt64 : rest)
    (TChar:TChar:rest)       -> return (op, TInt64 : rest)
    (TChar:TInt64:rest)      -> return (op, TInt64 : rest)
    (TInt64:TChar:rest)      -> return (op, TInt64 : rest)

    (TPtr t1 : TPtr t2 : rest) ->
        if t1 == t2 || t1 == TAny || t2 == TAny
            then return (op, TInt64 : rest)
            else Left $ TypeError pos "pointer comparison requires same ptr@T types"

    (t1:t2:_) -> Left $ TypeError pos $
        "comparison operator " ++ op ++ " type mismatch, got " ++ show t2 ++ " and " ++ show t1
    _ -> Left $ TypeError pos $
        "comparison operator " ++ op ++ " requires at least 2 elements on stack"

requireTypes :: Position -> [Type] -> TypeStack -> Either TypeError ()
requireTypes pos inputs stack = do
    let stackLen = length stack
        inputLen = length inputs

    if stackLen < inputLen
        then Left $ TypeError pos $
            "operation requires " ++ show inputLen ++
            " arguments but stack only has " ++ show stackLen
        else do
            let (topTypes, _) = splitAt inputLen stack
            if not (typesMatch inputs topTypes)
                then Left $ TypeError pos $
                    "type mismatch: expected " ++ showTypeStack inputs ++
                    " but got " ++ showTypeStack topTypes
                else return ()
  where
    matches TAny _ = True
    matches _ TAny = True
    matches a b    = a == b

    typesMatch expected actual = and (zipWith matches expected actual)

showTypeStack :: [Type] -> String
showTypeStack [] = "[]"
showTypeStack types = "[" ++ intercalate ", " (map showType types) ++ "]"

showType :: Type -> String
showType TInt64      = "int64"
showType TFloat64    = "float64"
showType TChar       = "char"
showType TVoid       = "void"
showType (TPtr t)    = "ptr@" ++ showType t
showType TAny        = "any"

sizeofType :: Type -> Maybe Int
sizeofType t = case t of
    TInt64   -> Just 8
    TFloat64 -> Just 8
    TPtr _   -> Just 8
    TChar    -> Just 1
    TVoid    -> Nothing
    TAny     -> Nothing

evalDeclareCount :: Position -> [Expr] -> Either TypeError Integer
evalDeclareCount stmtPos exprs = do
    toks <- mapM (exprToCountToken stmtPos) exprs
    case Eval.evalTokensToInt64 toks of
        Left msg -> Left $ TypeError stmtPos ("invalid declare object_count expression: " ++ msg)
        Right (Token tt _ _) ->
            case tt of
                TokIntLit n -> Right n
                _ -> Left $ TypeError stmtPos "declare object_count did not evaluate to int64"

exprToCountToken :: Position -> Expr -> Either TypeError Token
exprToCountToken stmtPos e =
    case e of
        EIntLit n pos   -> Right (Token (TokIntLit n) pos "")
        EStdop op pos   -> Right (Token (TokStdop op) pos op)

        EFloatLit _ pos  -> Left $ TypeError pos "declare object_count cannot use float literals"
        ECharLit _ pos   -> Left $ TypeError pos "declare object_count cannot use char literals"
        EStringLit _ pos -> Left $ TypeError pos "declare object_count cannot use string literals"

        EIdent name pos  -> Left $ TypeError pos ("declare object_count referencing identifier is not implemented: " ++ name)
