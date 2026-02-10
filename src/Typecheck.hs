module Typecheck
    ( typecheckProgram
    , TypeError(..)
    ) where

import Parser
import Lexer (Position(..))
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
        -- First pass: collect all function signatures
        state1 = foldl collectFunctionSig state0 (progFuncs prog)
        -- Second pass: typecheck functions and main
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
        savedSymbols = tcSymbols state
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

typecheckStatement (SIf thenBlock elseBlock cond pos) state = do
    (checkedCond, state1) <- typecheckExprs cond state
    
    case tcStack state1 of
        [] -> Left $ TypeError pos "if condition produces no value"
        (condType:restStack) -> do
            let state2 = state1 { tcStack = restStack }
            
            (checkedThen, thenState) <- typecheckBlock thenBlock state2
            
            case elseBlock of
                Nothing -> 
                    return (SIf checkedThen Nothing checkedCond pos, thenState)
                
                Just elseBlk -> do
                    (checkedElse, elseState) <- typecheckBlock elseBlk state2
                    
                    -- both branches should leave stack in same state
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
            
            -- loop body should restore stack to original state
            if tcStack bodyState /= tcStack state
                then Left $ TypeError pos $
                    "while loop body changes stack\n" ++
                    "  before: " ++ showTypeStack (tcStack state) ++ "\n" ++
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
    return (EStringLit s pos, state { tcStack = TPtr : tcStack state })

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
                                        arg_types = reverse topTypes  -- top of stack slice must be reversed

                                    if arg_types /= inputs
                                        then Left $ TypeError pos $
                                            "function " ++ name ++ " expects arguments " ++
                                            showTypeStack inputs ++ " but got " ++ showTypeStack arg_types
                                        else return (EIdent name pos, state { tcStack = outputs ++ restStack })
                        
                        Nothing -> Left $ TypeError pos $
                            "undefined symbol or function: " ++ name

typecheckStdop :: String -> Position -> TypeStack -> Either TypeError (String, TypeStack)
typecheckStdop op pos stack = case op of
    "+" -> binaryArithmetic op pos stack
    "-" -> binaryArithmetic op pos stack
    "*" -> binaryArithmetic op pos stack
    "/" -> binaryArithmetic op pos stack
    "%" -> requireTypes pos [TInt64, TInt64] [TInt64] stack >> return ("%", TInt64 : popN 2 stack)
    
    "=" -> binaryComparison op pos stack
    "!=" -> binaryComparison op pos stack
    "<" -> binaryComparison op pos stack
    "<=" -> binaryComparison op pos stack
    ">" -> binaryComparison op pos stack
    ">=" -> binaryComparison op pos stack
    
    "&" -> requireTypes pos [TInt64, TInt64] [TInt64] stack >> return ("&", TInt64 : popN 2 stack)
    "|" -> requireTypes pos [TInt64, TInt64] [TInt64] stack >> return ("|", TInt64 : popN 2 stack)
    "!" -> requireTypes pos [TInt64] [TInt64] stack >> return ("!", TInt64 : popN 1 stack)
    "&&" -> requireTypes pos [TInt64, TInt64] [TInt64] stack >> return ("&&", TInt64 : popN 2 stack)
    "||" -> requireTypes pos [TInt64, TInt64] [TInt64] stack >> return ("||", TInt64 : popN 2 stack)
    
    "dup" -> case stack of
        [] -> Left $ TypeError pos "dup requires at least 1 element on stack"
        (t:rest) -> return ("dup", t : t : rest)
    
    "drop" -> case stack of
        [] -> Left $ TypeError pos "drop requires at least 1 element on stack"
        (_:rest) -> return ("drop", rest)
    
    "swap" -> case stack of
        (a:b:rest) -> return ("swap", b : a : rest)
        _ -> Left $ TypeError pos "swap requires at least 2 elements on stack"
    
    "over" -> case stack of
        (a:b:rest) -> return ("over", b : a : b : rest)
        _ -> Left $ TypeError pos "over requires at least 2 elements on stack"
    
    "reach" -> case stack of
        (a:b:c:rest) -> return ("reach", c : a : b : rest)
        _ -> Left $ TypeError pos "reach requires at least 3 elements on stack"
    
    "char*" -> requireTypes pos [TPtr] [TChar] stack >> return ("char*", popN 1 stack ++ [TChar])
    "int64*" -> requireTypes pos [TPtr] [TInt64] stack >> return ("int64*", popN 1 stack ++ [TInt64])
    "float64*" -> requireTypes pos [TPtr] [TFloat64] stack >> return ("float64*", popN 1 stack ++ [TFloat64])
    "ptr*" -> requireTypes pos [TPtr] [TPtr] stack >> return ("ptr*", popN 1 stack ++ [TPtr])
    
    "char&" -> requireTypes pos [TChar, TPtr] [] stack >> return ("char&", popN 2 stack)
    "int64&" -> requireTypes pos [TInt64, TPtr] [] stack >> return ("int64&", popN 2 stack)
    "float64&" -> requireTypes pos [TFloat64, TPtr] [] stack >> return ("float64&", popN 2 stack)
    "ptr&" -> requireTypes pos [TPtr, TPtr] [] stack >> return ("ptr&", popN 2 stack)
    
    "print" -> requireTypes pos [TInt64] [] stack >> return ("print", popN 1 stack)

    "int64" -> case stack of
        (TFloat64:rest) -> return ("ftoi", TInt64 : rest)
        (t:rest) -> return ("int64", TInt64 : rest)
        [] -> Left $ TypeError pos "int64 conversion requires value on stack"
    
    "float64" -> case stack of
        (TInt64:rest) -> return ("float64", TFloat64 : rest)
        (TFloat64:rest) -> return ("float64", TFloat64 : rest)
        _ -> Left $ TypeError pos "float64 conversion requires int64 on stack"
    
    "char" -> case stack of
        (TInt64:rest) -> return ("char", TChar : rest)
        (TChar:rest) -> return ("char", TChar : rest)
        _ -> Left $ TypeError pos "char conversion requires int64 on stack"
    
    "ptr" -> case stack of
        (t:rest) -> return ("ptr", TPtr : rest)
        [] -> Left $ TypeError pos "ptr conversion requires value on stack"
    
    "__syscall1__" -> requireTypes pos [TInt64, TAny] [TInt64] stack >> 
        return ("__syscall1__", popN 2 stack ++ [TInt64])
    
    "__syscall3__" -> requireTypes pos [TInt64, TAny, TAny, TAny] [TInt64] stack >> 
        return ("__syscall3__", popN 4 stack ++ [TInt64])

    "__syscall4__" -> requireTypes pos [TInt64, TAny, TAny, TAny, TAny] [TInt64] stack >> 
        return ("__syscall4__", popN 5 stack ++ [TInt64])
    
    "f+" -> requireTypes pos [TFloat64, TFloat64] [TFloat64] stack >> return ("f+", popN 2 stack ++ [TFloat64])
    "f-" -> requireTypes pos [TFloat64, TFloat64] [TFloat64] stack >> return ("f-", popN 2 stack ++ [TFloat64])
    "f*" -> requireTypes pos [TFloat64, TFloat64] [TFloat64] stack >> return ("f*", popN 2 stack ++ [TFloat64])
    "f/" -> requireTypes pos [TFloat64, TFloat64] [TFloat64] stack >> return ("f/", popN 2 stack ++ [TFloat64])
    "f=" -> requireTypes pos [TFloat64, TFloat64] [TInt64] stack >> return ("f=", popN 2 stack ++ [TInt64])
    "f!=" -> requireTypes pos [TFloat64, TFloat64] [TInt64] stack >> return ("f!=", popN 2 stack ++ [TInt64])
    "f<" -> requireTypes pos [TFloat64, TFloat64] [TInt64] stack >> return ("f<", popN 2 stack ++ [TInt64])
    "f<=" -> requireTypes pos [TFloat64, TFloat64] [TInt64] stack >> return ("f<=", popN 2 stack ++ [TInt64])
    "f>" -> requireTypes pos [TFloat64, TFloat64] [TInt64] stack >> return ("f>", popN 2 stack ++ [TInt64])
    "f>=" -> requireTypes pos [TFloat64, TFloat64] [TInt64] stack >> return ("f>=", popN 2 stack ++ [TInt64])
    
    _ -> Left $ TypeError pos $ "unknown stdop: " ++ op

binaryArithmetic :: String -> Position -> TypeStack -> Either TypeError (String, TypeStack)
binaryArithmetic op pos stack = case stack of
    (TFloat64:TFloat64:rest) -> return ("f" ++ op, TFloat64 : rest)
    (TInt64:TInt64:rest) -> return (op, TInt64 : rest)
    (TInt64:TPtr:rest) -> return (op, TPtr : rest)
    (TInt64:TChar:rest) -> return (op, TChar : rest)
    (TChar:TChar:rest) -> return (op, TChar : rest) 
    (TChar:TInt64:rest) -> return (op, TChar : rest)
    (TPtr:TPtr:rest) -> return (op, TPtr : rest) 
    (TPtr:TInt64:rest) -> return (op, TPtr : rest) 

    (t1:t2:_) -> Left $ TypeError pos $
        "arithmetic operator " ++ op ++ " requires two values of same numeric type, got " ++
        show t2 ++ " and " ++ show t1
    _ -> Left $ TypeError pos $ "arithmetic operator " ++ op ++ " requires at least 2 elements on stack"

binaryComparison :: String -> Position -> TypeStack -> Either TypeError (String, TypeStack)
binaryComparison op pos stack = case stack of
    (TFloat64:TFloat64:rest) -> return ("f" ++ op, TInt64 : rest)
    (TInt64:TInt64:rest) -> return (op, TInt64 : rest)
    (TInt64:TChar:rest) -> return (op, TInt64 : rest)
    (TChar:TChar:rest) -> return (op, TInt64 : rest)
    (TChar:TInt64:rest) -> return (op, TInt64 : rest)
    (TPtr:TPtr:rest) -> return (op, TInt64 : rest)

    (t1:t2:_) -> Left $ TypeError pos $
        "comparison operator " ++ op ++ " requires two values of same type, got " ++
        show t2 ++ " and " ++ show t1

    _ -> Left $ TypeError pos $ "comparison operator " ++ op ++ " requires at least 2 elements on stack"

requireTypes :: Position -> [Type] -> [Type] -> TypeStack -> Either TypeError ()
requireTypes pos inputs outputs stack = do
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

popN :: Int -> TypeStack -> TypeStack
popN n stack = drop n stack

showTypeStack :: [Type] -> String
showTypeStack [] = "[]"
showTypeStack types = "[" ++ intercalate ", " (map showType types) ++ "]"

showType :: Type -> String
showType TInt64 = "int64"
showType TFloat64 = "float64"
showType TPtr = "ptr"
showType TChar = "char"
