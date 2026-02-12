module Codegen
    ( generateNASMx86_64 ) where

import Typecheck
import Parser
import Lexer (Position(..))
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map as Map
import Data.Char (ord, isDigit)

data VarLocation
    = GlobalVal  Int
    | LocalVal   Int
    | GlobalAddr Int
    | LocalAddr  Int
    deriving (Show, Eq)

data CodeGenState = CodeGenState
    { cgsLabelCounter :: Int
    , cgsStringLiterals :: Map.Map String String
    , cgsStringCounter :: Int
    , cgsSymbols :: Map.Map String VarLocation
    , cgsLocalOffset :: Int
    , cgsGlobalOffset :: Int
    , cgsInFunction :: Bool
    , cgsReturnLabel :: Maybe String
    } deriving (Show)

initialState :: CodeGenState
initialState = CodeGenState
    { cgsLabelCounter = 0
    , cgsStringLiterals = Map.empty
    , cgsStringCounter = 0
    , cgsSymbols = Map.empty
    , cgsLocalOffset = 0
    , cgsGlobalOffset = 0
    , cgsInFunction = False
    , cgsReturnLabel = Nothing
    }

tab :: String 
tab = "    "

blankLine :: String
blankLine = ""

hardcodeRuntime :: [String]
hardcodeRuntime =
    [ "__print_int__:"
    , tab ++ "push rbp"
    , tab ++ "mov rbp, rsp"
    , tab ++ "sub rsp, 48"
    , tab ++ "mov DWORD [rbp-36], edi"
    , tab ++ "mov DWORD [rbp-4], 1"
    , tab ++ "mov eax, DWORD [rbp-4]"
    , tab ++ "cdqe"
    , tab ++ "mov edx, 16"
    , tab ++ "sub rdx, rax"
    , tab ++ "mov BYTE [rbp-32+rdx], 10"
    , ".L2:"
    , tab ++ "mov edx, DWORD [rbp-36]"
    , tab ++ "movsx rax, edx"
    , tab ++ "imul rax, rax, 1717986919"
    , tab ++ "shr rax, 32"
    , tab ++ "mov ecx, eax"
    , tab ++ "sar ecx, 2"
    , tab ++ "mov eax, edx"
    , tab ++ "sar eax, 31"
    , tab ++ "sub ecx, eax"
    , tab ++ "mov eax, ecx"
    , tab ++ "sal eax, 2"
    , tab ++ "add eax, ecx"
    , tab ++ "add eax, eax"
    , tab ++ "mov ecx, edx"
    , tab ++ "sub ecx, eax"
    , tab ++ "mov eax, ecx"
    , tab ++ "lea ecx, [rax+48]"
    , tab ++ "mov eax, DWORD [rbp-4]"
    , tab ++ "lea edx, [rax+1]"
    , tab ++ "mov DWORD [rbp-4], edx"
    , tab ++ "cdqe"
    , tab ++ "mov edx, 15"
    , tab ++ "sub rdx, rax"
    , tab ++ "mov eax, ecx"
    , tab ++ "mov BYTE [rbp-32+rdx], al"
    , tab ++ "mov eax, DWORD [rbp-36]"
    , tab ++ "movsx rdx, eax"
    , tab ++ "imul rdx, rdx, 1717986919"
    , tab ++ "shr rdx, 32"
    , tab ++ "mov ecx, edx"
    , tab ++ "sar ecx, 2"
    , tab ++ "cdq"
    , tab ++ "mov eax, ecx"
    , tab ++ "sub eax, edx"
    , tab ++ "mov DWORD [rbp-36], eax"
    , tab ++ "cmp DWORD [rbp-36], 0"
    , tab ++ "jne .L2"
    , tab ++ "mov eax, DWORD [rbp-4]"
    , tab ++ "cdqe"
    , tab ++ "mov edx, 16"
    , tab ++ "sub rdx, rax"
    , tab ++ "lea rax, [rbp-32]"
    , tab ++ "lea rcx, [rax+rdx]"
    , tab ++ "mov eax, DWORD [rbp-4]"
    , tab ++ "mov edx, eax"
    , tab ++ "mov rsi, rcx"
    , tab ++ "mov edi, 1"
    , tab ++ "mov eax, 0"
    , tab ++ "mov rax, 1"
    , tab ++ "syscall"
    , tab ++ "nop"
    , tab ++ "leave"
    , tab ++ "ret"
    , blankLine
    ]
   
dataStackSizeWords :: Integer
dataStackSizeWords = 65536 

generateNASMx86_64 :: Program -> String
generateNASMx86_64 prog =
    let ((funcsCode, mainCode), finalState) =
            runCodeGen (genProgram prog) initialState

        totalGlobalBytes =
            roundUp8 (cgsGlobalOffset finalState) + 8

    in unlines $
        [ "section .data"
        ]
        ++ genStringLiterals finalState ++
        [ blankLine
        , "section .bss"
        , "__dstack__      resq " ++ show dataStackSizeWords
        , "__global_vars__ resb " ++ show totalGlobalBytes
        , blankLine
        , "section .text"
        , "global _start"
        , blankLine
        ]
        ++ hardcodeRuntime ++
        funcsCode ++
        [ "_start:"
        , tab ++ "mov r12, __dstack__"
        , tab ++ "mov r13, __global_vars__"
        , blankLine
        ]
        ++ mainCode ++
        [ blankLine
        , tab ++ "mov rax, 60"
        , tab ++ "xor rdi, rdi"
        , tab ++ "syscall"
        ] 

type CodeGen a = CodeGenState -> (a, CodeGenState)

runCodeGen :: CodeGen a -> CodeGenState -> (a, CodeGenState)
runCodeGen = id

genProgram :: Program -> CodeGen ([String], [String])
genProgram prog state = 
    let (funcsCode, state1) = genFunctions (progFuncs prog) state
        (mainCode, state2) = genStatements (progMain prog) state1
    in ((funcsCode, mainCode), state2)

commentExpr :: Position -> String -> String 
commentExpr pos comment = tab ++ "; " ++ show pos ++ " " ++ comment 

escapeString :: String -> String
escapeString s = intercalate ", " (toOrdIfSpecial s)
  where
    toOrdIfSpecial [] = []
    toOrdIfSpecial xs =
      let (chunk, rest) = span isNormal xs
      in  (if null chunk then [] else ["\"" ++ chunk ++ "\""])
          ++ case rest of
               []     -> []
               (c:cs) -> show (ord c) : toOrdIfSpecial cs

    isNormal c = c /= '\n' && c /= '\t' && c /= '\r' && c /= '\0'

genStringLiterals :: CodeGenState -> [String]
genStringLiterals state =
    map (\(str, label) -> label ++ ": db " ++ escapeString str ++ ", 0")
        (Map.toList (cgsStringLiterals state))

genDeclares :: Program -> [String]
genDeclares prog = []

genFunctions :: [Function] -> CodeGen [String]
genFunctions [] state = ([], state)
genFunctions (func:funcs) state =
    let (code1, state1) = genFunction func state
        (code2, state2) = genFunctions funcs state1
    in (code1 ++ code2, state2)

genFunction :: Function -> CodeGen [String]
genFunction (Function name _ body pos) state =
    let savedSymbols = cgsSymbols state
        savedRet     = cgsReturnLabel state

        (retLabel, state0) = freshLabel "ret" state

        state1 =
            state0 { cgsInFunction   = True
                   , cgsLocalOffset  = 0
                   , cgsReturnLabel  = Just retLabel
                   }

        (bodyCode, state2) =
            genBlock body state1

        localsize = cgsLocalOffset state2

        prologue =
            [ name ++ ":"
            , tab ++ "push rbp"
            , tab ++ "mov rbp, rsp"
            , blankLine
            ]
            ++ if localsize == 0 then []
               else [ tab ++ "sub rsp, " ++ show localsize ]

        epilogue =
            [ retLabel ++ ":"
            , tab ++ "leave"
            , tab ++ "ret"
            , blankLine
            ]

        state3 =
            state2 { cgsSymbols      = savedSymbols
                   , cgsInFunction   = False
                   , cgsReturnLabel  = savedRet
                   }

    in (prologue ++ bodyCode ++ epilogue, state3)
genStatements :: [Statement] -> CodeGen [String]
genStatements [] state = ([], state)
genStatements (stmt:stmts) state =
    let (code1, state1) = genStatement stmt state
        (code2, state2) = genStatements stmts state1
    in (code1 ++ code2, state2)

genStatement :: Statement -> CodeGen [String]
genStatement (SExpr expr) state = genExpr expr state

genStatement (SReturn pos) state =
    case cgsReturnLabel state of
        Nothing ->
            error (show pos ++ ": return used outside of a function")
        Just lbl ->
            ([ commentExpr pos "RETURN"
             , tab ++ "jmp " ++ lbl
             ], state)

genStatement (SLet vars pos) state = 
    let (code, state') = genLetBinding (reverse vars) pos state
    in (code, state')

genStatement (SDeclare name elemType countExprs pos) state =
    let n = 
            case evalDeclareCount pos countExprs of
                Left err -> error $ show err
                Right s  -> s

        elemSize = sizeofTypeForDeclare elemType
        bytes0 = (fromInteger n) * (fromIntegral elemSize)

    in if n <= 0
        then error (show pos ++ ": declare object_count must be positive")
        else
            let bytes = roundUp8 (fromIntegral bytes0)
                inFunc = cgsInFunction state

                (loc, state1) =
                    case Map.lookup name (cgsSymbols state) of
                        Just _ ->
                            error (show pos ++ ": redeclaration of " ++ name)
                        Nothing ->
                            if inFunc
                            then
                                let off = cgsLocalOffset state + bytes
                                in ( LocalAddr off
                                   , state { cgsLocalOffset = off }
                                   )
                            else
                                let off = cgsGlobalOffset state
                                in ( GlobalAddr off
                                   , state { cgsGlobalOffset = off + bytes }
                                   )

                state2 = state1 { cgsSymbols = Map.insert name loc (cgsSymbols state1) }
                code = [ commentExpr pos ("DECLARE " ++ name ++ " " ++ showType elemType ++ " count=" ++ show n ++ " bytes=" ++ show bytes) ]

            in (code, state2)

genStatement (SIf thenBlock elseBlock cond pos) state =
    let (condCode, state1) = genExprs cond state
        (thenCode, state2) = genBlock thenBlock state1
        (elseCode, state3) = case elseBlock of
            Nothing -> ([], state2)
            Just blk -> genBlock blk state2
        (labelElse, state4) = freshLabel "else" state3
        (labelEnd, state5) = freshLabel "endif" state4
    in (condCode ++
        [ commentExpr pos "IF-THEN-ELSE"
        , popStack "rax"
        , tab ++ "test rax, rax"
        , tab ++ "jz " ++ labelElse
        ] ++
        thenCode ++
        [ tab ++ "jmp " ++ labelEnd
        , blankLine
        , labelElse ++ ":"
        ] ++
        elseCode ++
        [ labelEnd ++ ":"
        ], state5)

genStatement (SWhile body cond pos) state =
    let (condCode, state1) = genExprs cond state
        (bodyCode, state2) = genBlock body state1
        (labelStart, state3) = freshLabel "while" state2
        (labelEnd, state4) = freshLabel "endwhile" state3
    in ([ labelStart ++ ":"
        ] ++
        condCode ++
        [ commentExpr pos "WHILE"
        , popStack "rax"
        , tab ++ "test rax, rax"
        , tab ++ "jz " ++ labelEnd
        ] ++
        bodyCode ++
        [ tab ++ "jmp " ++ labelStart
        , labelEnd ++ ":"
        ], state4)

genLetBinding :: [String] -> Position -> CodeGen [String]
genLetBinding [] _ s = ([], s)
genLetBinding (v:vs) pos s =
    let inFunc = cgsInFunction s

        -- reuse existing location if symbol already exists
        (loc, s1) =
            case Map.lookup v (cgsSymbols s) of
                Just existing ->
                    (existing, s)

                Nothing ->
                    if inFunc
                    then
                        let off = cgsLocalOffset s + 8
                        in ( LocalVal off
                           , s { cgsLocalOffset = off }
                           )
                    else
                        let off = cgsGlobalOffset s
                        in ( GlobalVal off
                           , s { cgsGlobalOffset = off + 8 }
                           )

        -- update symbol table
        s2 = s1 { cgsSymbols = Map.insert v loc (cgsSymbols s1) }

        store =
            case loc of
                LocalVal off ->
                    [ commentExpr pos "LOCAL LET"
                    , popStack "rax"
                    , tab ++ "mov [rbp - " ++ show off ++ "], rax"
                    ]

                GlobalVal off ->
                    [ commentExpr pos "GLOBAL LET"
                    , popStack "rax"
                    , tab ++ "mov [r13 + " ++ show off ++ "], rax"
                    ]

                _ -> error "internal: let bound to non-let location"

        (rest, s3) =
            genLetBinding vs pos s2

    in (store ++ rest, s3)



genBlock :: Block -> CodeGen [String]
genBlock (Block stmts _) state = genStatements stmts state

genExprs :: [Expr] -> CodeGen [String]
genExprs [] state = ([], state)
genExprs (expr:exprs) state =
    let (code1, state1) = genExpr expr state
        (code2, state2) = genExprs exprs state1
    in (code1 ++ code2, state2)

genExpr :: Expr -> CodeGen [String]
genExpr (EIntLit n pos) state = 
    ([ commentExpr pos "PUSH INT64: " ++ show n
     , tab ++ "mov rax, " ++ show n
     , pushStack "rax"
     ], state)

genExpr (EFloatLit f pos) state = 
    ([ commentExpr pos "PUSH FLOAT64: " ++ show f
     , tab ++ "mov rax, __float64__(" ++ show f ++ ")"
     , pushStack "rax"
     ], state)

genExpr (ECharLit c pos) state = 
    ([ commentExpr pos "PUSH CHAR: " ++ show c
     , tab ++ "mov rax, " ++ show (fromEnum c)
     , pushStack "rax"
     ], state)

genExpr (EStringLit s pos) state =
    let (label, state1) = getOrCreateStringLiteral s state
    in ([ commentExpr pos "PUSH STRING: " ++ s
        , tab ++ "mov rax, " ++ label
        , pushStack "rax"
        ], state1)

genExpr (EStdop op pos) state = (genStdop op pos, state)

genExpr (EIdent name pos) state =
    case Map.lookup name (cgsSymbols state) of

        Just (LocalVal off) ->
            ([ commentExpr pos ("LOCAL " ++ name)
             , tab ++ "mov rax, [rbp - " ++ show off ++ "]"
             , pushStack "rax"
             ], state)

        Just (GlobalVal off) ->
            ([ commentExpr pos ("GLOBAL " ++ name)
             , tab ++ "mov rax, [r13 + " ++ show off ++ "]"
             , pushStack "rax"
             ], state)

        Just (LocalAddr off) ->
            ([ commentExpr pos ("LOCAL_ADDR " ++ name)
             , tab ++ "lea rax, [rbp - " ++ show off ++ "]"
             , pushStack "rax"
             ], state)

        Just (GlobalAddr off) ->
            ([ commentExpr pos ("GLOBAL_ADDR " ++ name)
             , tab ++ "lea rax, [r13 + " ++ show off ++ "]"
             , pushStack "rax"
             ], state)

        Nothing ->
            ([ commentExpr pos ("CALL " ++ name)
             , tab ++ "call " ++ name
             , blankLine
             ], state)

pushStack :: String -> String 
pushStack pushFrom = unlines [tab ++ "mov [r12], " ++ pushFrom, tab ++ "add r12, 8"]

popStack :: String -> String 
popStack popTo = unlines [tab ++ "sub r12, 8", tab ++ "mov " ++ popTo ++ ", [r12]"]

-- (isAdd, size)
parsePtrOp :: String -> Maybe (Bool, Int)
parsePtrOp s = case s of
    ('p':'+':rest) -> do
        n <- parse_num rest
        Just (True, n)
    ('p':'-':rest) -> do
        n <- parse_num rest
        Just (False, n)
    _ -> Nothing
  where
    parse_num xs
        | null xs = Nothing
        | all isDigit xs =
            let n = read xs :: Int
            in if n > 0 then Just n else Nothing
        | otherwise = Nothing

genStdop :: String -> Position -> [String]
genStdop op pos = commentExpr pos ("STDOP: " ++ op) : genStdopRaw op

genStdopRaw :: String -> [String]
genStdopRaw "+" = 
    [ popStack "rbx"
    , popStack "rax"
    , tab ++ "add rax, rbx"
    , pushStack "rax"
    ]

genStdopRaw "-" = 
    [ popStack "rbx"
    , popStack "rax"
    , tab ++ "sub rax, rbx"
    , pushStack "rax"
    ]

genStdopRaw "*" = 
    [ popStack "rbx"
    , popStack "rax"
    , tab ++ "imul rax, rbx"
    , pushStack "rax"
    ]

genStdopRaw "/" = 
    [ popStack "rbx"
    , popStack "rax"
    , tab ++ "cqo"
    , tab ++ "idiv rbx"
    , pushStack "rax"
    ]

genStdopRaw "%" = 
    [ popStack "rbx"
    , popStack "rax"
    , tab ++ "cqo"
    , tab ++ "idiv rbx"
    , pushStack "rdx"
    ]

genStdopRaw "=" = 
    [ popStack "rcx"
    , popStack "rbx"
    , tab ++ "cmp rbx, rcx"
    , tab ++ "sete al"
    , tab ++ "movzx rax, al"
    , pushStack "rax"
    ]

genStdopRaw "!=" = 
    [ popStack "rcx"
    , popStack "rbx"
    , tab ++ "cmp rbx, rcx"
    , tab ++ "setne al"
    , tab ++ "movzx rax, al"
    , pushStack "rax"
    ]

genStdopRaw "<" = 
    [ popStack "rcx"
    , popStack "rbx"
    , tab ++ "cmp rbx, rcx"
    , tab ++ "setl al"
    , tab ++ "movzx rax, al"
    , pushStack "rax"
    ]

genStdopRaw "<=" = 
    [ popStack "rcx"
    , popStack "rbx"
    , tab ++ "cmp rbx, rcx"
    , tab ++ "setle al"
    , tab ++ "movzx rax, al"
    , pushStack "rax"
    ]

genStdopRaw ">" = 
    [ popStack "rcx"
    , popStack "rbx"
    , tab ++ "cmp rbx, rcx"
    , tab ++ "setg al"
    , tab ++ "movzx rax, al"
    , pushStack "rax"
    ]

genStdopRaw ">=" = 
    [ popStack "rcx"
    , popStack "rbx"
    , tab ++ "cmp rbx, rcx"
    , tab ++ "setge al"
    , tab ++ "movzx rax, al"
    , pushStack "rax"
    ]

genStdopRaw "||" = 
    [ popStack "rax"
    , popStack "rbx"
    , tab ++ "or rax, rbx"
    , pushStack "rax"
    ]

genStdopRaw "&&" = 
    [ popStack "rax"
    , popStack "rbx"
    , tab ++ "and rax, rbx"
    , pushStack "rax"
    ]

genStdopRaw "|" = genStdopRaw "||"
genStdopRaw "&" = genStdopRaw "&&"

genStdopRaw "!" = 
    [ popStack "rax"
    , tab ++ "cmp al, 0"
    , tab ++ "setz al"
    , tab ++ "movzx rax, al"
    , pushStack "rax"
    ]

genStdopRaw "f+" = 
    [ popStack "rbx"
    , popStack "rax"
    , tab ++ "movq xmm0, rax"
    , tab ++ "movq xmm1, rbx"
    , tab ++ "addsd xmm0, xmm1"
    , tab ++ "movq rax, xmm0"
    , pushStack "rax"
    ]

genStdopRaw "f-" = 
    [ popStack "rbx"
    , popStack "rax"
    , tab ++ "movq xmm0, rax"
    , tab ++ "movq xmm1, rbx"
    , tab ++ "subsd xmm0, xmm1"
    , tab ++ "movq rax, xmm0"
    , pushStack "rax"
    ]

genStdopRaw "f*" = 
    [ popStack "rbx"
    , popStack "rax"
    , tab ++ "movq xmm0, rax"
    , tab ++ "movq xmm1, rbx"
    , tab ++ "mulsd xmm0, xmm1"
    , tab ++ "movq rax, xmm0"
    , pushStack "rax"
    ]

genStdopRaw "f/" = 
    [ popStack "rbx"
    , popStack "rax"
    , tab ++ "movq xmm0, rax"
    , tab ++ "movq xmm1, rbx"
    , tab ++ "divsd xmm0, xmm1"
    , tab ++ "movq rax, xmm0"
    , pushStack "rax"
    ]

genStdopRaw "f=" = genStdopRaw "="

genStdopRaw "f<" = 
    [ popStack "rax"
    , popStack "rbx"
    , tab ++ "movq xmm0, rax"
    , tab ++ "movq xmm1, rbx"
    , tab ++ "ucomisd xmm0, xmm1"
    , tab ++ "seta al"
    , tab ++ "movzx rax, al"
    , pushStack "rax"
    ]

genStdopRaw "f<=" = 
    [ popStack "rax"
    , popStack "rbx"
    , tab ++ "movq xmm0, rax"
    , tab ++ "movq xmm1, rbx"
    , tab ++ "ucomisd xmm0, xmm1"
    , tab ++ "setae al"
    , tab ++ "movzx rax, al"
    , pushStack "rax"
    ]

genStdopRaw "f>" = 
    [ popStack "rax"
    , popStack "rbx"
    , tab ++ "movq xmm0, rax"
    , tab ++ "movq xmm1, rbx"
    , tab ++ "ucomisd xmm1, xmm0"
    , tab ++ "seta al"
    , tab ++ "movzx rax, al"
    , pushStack "rax"
    ]

genStdopRaw "f>=" = 
    [ popStack "rax"
    , popStack "rbx"
    , tab ++ "movq xmm0, rax"
    , tab ++ "movq xmm1, rbx"
    , tab ++ "ucomisd xmm1, xmm0"
    , tab ++ "setae al"
    , tab ++ "movzx rax, al"
    , pushStack "rax"
    ]

genStdopRaw "dup" = 
    [ popStack "rax"
    , pushStack "rax"
    , pushStack "rax"
    ]

genStdopRaw "drop" = 
    [ popStack "rax" ]

genStdopRaw "swap" = 
    [ popStack "rax"
    , popStack "rbx"
    , pushStack "rax"
    , pushStack "rbx"
    ]

genStdopRaw "over" = 
    [ popStack "rax"
    , popStack "rbx"
    , pushStack "rbx"
    , pushStack "rax"
    , pushStack "rbx"
    ]

genStdopRaw "reach" = 
    [ popStack "rax"
    , popStack "rbx"
    , popStack "rcx"
    , pushStack "rbx"
    , pushStack "rax"
    , pushStack "rcx"
    ]

genStdopRaw "char*" = 
    [ popStack "rax"
    , tab ++ "mov rbx, 0" 
    , tab ++ "mov bl, [rax]"
    , pushStack "rbx"
    ]

genStdopRaw "int64*" = 
    [ popStack "rax"
    , tab ++ "mov rbx, QWORD [rax]"
    , pushStack "rbx"
    ]

genStdopRaw "ptr*" = genStdopRaw "int64*"
genStdopRaw "float64*" = genStdopRaw "int64*"

genStdopRaw "char&" = 
    [ popStack "rbx"
    , popStack "rax"
    , tab ++ "mov [rax], bl"
    ]

genStdopRaw "int64&" = 
    [ popStack "rbx"
    , popStack "rax"
    , tab ++ "mov QWORD [rax], rbx"
    ]

genStdopRaw "ptr&" = genStdopRaw "int64&"
genStdopRaw "float64&" = genStdopRaw "int64&"

genStdopRaw "ftoi" = 
    [ popStack "rax"
    , tab ++ "movq xmm0, rax"
    , tab ++ "cvttsd2si rax, xmm0"
    , pushStack "rax"
    ]

genStdopRaw "float64" = 
    [
      popStack "rax"
    , tab ++ "cvtsi2sd xmm0, rax"
    , tab ++ "movq rax, xmm0"
    , pushStack "rax"
    ]

genStdopRaw op
  | Just (is_add, n) <- parsePtrOp op =
      [ popStack "rbx"
      , tab ++ "imul rbx, " ++ show n
      , popStack "rax"
      , tab ++ (if is_add then "add" else "sub") ++ " rax, rbx"
      , pushStack "rax"
      ]

genStdopRaw op 
    | "ptr@" `isPrefixOf` op = []
    | op `elem` ["int64", "char"] = []

genStdopRaw "print" = 
    [ popStack "rdi"
    , tab ++ "call __print_int__"
    ]

genStdopRaw "__syscall0__" = 
    [ popStack "rax"
    , tab ++ "syscall"
    , pushStack "rax"
    ]

genStdopRaw "__syscall1__" = 
    [ popStack "rdi"
    , popStack "rax"
    , tab ++ "syscall"
    , pushStack "rax"
    ]

genStdopRaw "__syscall2__" = 
    [ popStack "rsi"
    , popStack "rdi"
    , popStack "rax"
    , tab ++ "syscall"
    , pushStack "rax"
    ]

genStdopRaw "__syscall3__" = 
    [ popStack "rdx"
    , popStack "rsi"
    , popStack "rdi"
    , popStack "rax"
    , tab ++ "syscall"
    , pushStack "rax"
    ]

genStdopRaw "__syscall4__" = 
    [ popStack "r10"
    , popStack "rdx"
    , popStack "rsi"
    , popStack "rdi"
    , popStack "rax"
    , tab ++ "syscall"
    , pushStack "rax"
    ]

genStdopRaw op = error $ "codegen: unimplemented stdop: " ++ op

freshLabel :: String -> CodeGen String
freshLabel prefix state =
    let counter = cgsLabelCounter state
        label = "." ++ prefix ++ "_" ++ show counter
        newState = state { cgsLabelCounter = counter + 1 }
    in (label, newState)

getOrCreateStringLiteral :: String -> CodeGen String
getOrCreateStringLiteral s state =
    case Map.lookup s (cgsStringLiterals state) of
        Just label -> (label, state)
        Nothing ->
            let counter = cgsStringCounter state
                label = "str_" ++ show counter
                newLiterals = Map.insert s label (cgsStringLiterals state)
                newState = state 
                    { cgsStringLiterals = newLiterals
                    , cgsStringCounter = counter + 1
                    }
            in (label, newState)


roundUp8 :: Int -> Int
roundUp8 n =
    let r = n `mod` 8
    in if r == 0 then n else n + (8 - r)

sizeofTypeForDeclare :: Type -> Int
sizeofTypeForDeclare t = case t of
    TInt64     -> 8
    TFloat64   -> 8
    TChar      -> 1
    TPtr _     -> 8
    TAny       -> 1
