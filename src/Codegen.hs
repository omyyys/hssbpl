module Codegen
    ( generateNASMx86_64
    ) where

import Parser
import Lexer (Position(..))
import Data.List (intercalate)
import qualified Data.Map as Map

data CodeGenState = CodeGenState
    { cgsLabelCounter :: Int
    , cgsStringLiterals :: Map.Map String String
    , cgsStringCounter :: Int
    } deriving (Show)

tab :: String 
tab = "    "

initialState :: CodeGenState
initialState = CodeGenState
    { cgsLabelCounter = 0
    , cgsStringLiterals = Map.empty
    , cgsStringCounter = 0
    }

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
    ]
    
generateNASMx86_64 :: Program -> String
generateNASMx86_64 prog = 
    let (code, finalState) = runCodeGen (genProgram prog) initialState
    in unlines $
        [ "section .data"
        ] ++
        genStringLiterals finalState ++
        genDeclares prog ++
        [ ""
        , "section .bss"
        ] ++
        genBSS prog ++
        [ ""
        , "section .text"
        , "global _start"
        ] ++
        hardcodeRuntime ++ 
        [ "_start:" ] ++
        code ++
        [ tab ++ "mov rax, 60"
        , tab ++ "mov rdi, 0"
        , tab ++ "syscall"
        ]

type CodeGen a = CodeGenState -> (a, CodeGenState)

runCodeGen :: CodeGen a -> CodeGenState -> (a, CodeGenState)
runCodeGen = id

genProgram :: Program -> CodeGen [String]
genProgram prog state = 
    let (mainCode, state') = genStatements (progMain prog) state
    in (mainCode, state')

genStringLiterals :: CodeGenState -> [String]
genStringLiterals state = 
    map (\(label, str) -> label ++ ": db " ++ escapeString str ++ ", 0")
        (Map.toList (cgsStringLiterals state))
  where
    escapeString s = "\"" ++ concatMap escapeChar s ++ "\""
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar '\0' = "\\0"
    escapeChar c    = [c]

genDeclares :: Program -> [String]
genDeclares prog = []

genBSS :: Program -> [String]
genBSS prog = 
    map (\(name, size) -> name ++ ": resb " ++ show size) (progDecls prog)

genStatements :: [Statement] -> CodeGen [String]
genStatements [] state = ([], state)
genStatements (stmt:stmts) state =
    let (code1, state1) = genStatement stmt state
        (code2, state2) = genStatements stmts state1
    in (code1 ++ code2, state2)

genStatement :: Statement -> CodeGen [String]
genStatement (SExpr expr) state = genExpr expr state
genStatement (SLet vars pos) state = ([], state)

genStatement (SIf thenBlock elseBlock cond pos) state =
    let (condCode, state1) = genExprs cond state
        (thenCode, state2) = genBlock thenBlock state1
        (elseCode, state3) = case elseBlock of
            Nothing -> ([], state2)
            Just blk -> genBlock blk state2
        (labelElse, state4) = freshLabel "else" state3
        (labelEnd, state5) = freshLabel "endif" state4
    in (condCode ++
        [ tab ++ "pop rax"
        , tab ++ "test rax, rax"
        , tab ++ "jz " ++ labelElse
        ] ++
        thenCode ++
        [ tab ++ "jmp " ++ labelEnd
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
        [ tab ++ "pop rax"
        , tab ++ "test rax, rax"
        , tab ++ "jz " ++ labelEnd
        ] ++
        bodyCode ++
        [ tab ++ "jmp " ++ labelStart
        , labelEnd ++ ":"
        ], state4)

genBlock :: Block -> CodeGen [String]
genBlock (Block stmts _) state = genStatements stmts state

genExprs :: [Expr] -> CodeGen [String]
genExprs [] state = ([], state)
genExprs (expr:exprs) state =
    let (code1, state1) = genExpr expr state
        (code2, state2) = genExprs exprs state1
    in (code1 ++ code2, state2)

genExpr :: Expr -> CodeGen [String]
genExpr (EIntLit n _) state = 
    ([ tab ++ "mov rax, " ++ show n
     , tab ++ "push rax"
     ], state)

genExpr (EFloatLit f _) state = 
    ([ tab ++ "mov rax, __float64_bits__(" ++ show f ++ ")"
     , tab ++ "push rax"
     ], state)

genExpr (ECharLit c _) state = 
    ([ tab ++ "mov rax, " ++ show (fromEnum c)
     , tab ++ "push rax"
     ], state)

genExpr (EStringLit s pos) state =
    let (label, state1) = getOrCreateStringLiteral s state
    in ([ tab ++ "lea rax, [rel " ++ label ++ "]"
        , tab ++ "push rax"
        ], state1)

genExpr (EStdop op _) state = (genStdop op, state)

genExpr (EIdent name _) state = 
    ([ tab ++ "call " ++ name
     ], state)

genStdop :: String -> [String]
genStdop "+" = 
    [ tab ++ "pop rbx"
    , tab ++ "pop rax"
    , tab ++ "add rax, rbx"
    , tab ++ "push rax"
    ]

genStdop "-" = 
    [ tab ++ "pop rbx"
    , tab ++ "pop rax"
    , tab ++ "sub rax, rbx"
    , tab ++ "push rax"
    ]

genStdop "*" = 
    [ tab ++ "pop rbx"
    , tab ++ "pop rax"
    , tab ++ "imul rax, rbx"
    , tab ++ "push rax"
    ]

genStdop "/" = 
    [ tab ++ "pop rbx"
    , tab ++ "pop rax"
    , tab ++ "cqo"
    , tab ++ "idiv rbx"
    , tab ++ "push rax"
    ]

genStdop "%" = 
    [ tab ++ "pop rbx"
    , tab ++ "pop rax"
    , tab ++ "cqo"
    , tab ++ "idiv rbx"
    , tab ++ "push rdx"
    ]

genStdop "=" = 
    [ tab ++ "pop rcx"
    , tab ++ "pop rbx"
    , tab ++ "cmp rbx, rcx"
    , tab ++ "sete al"
    , tab ++ "movzx rax, al"
    , tab ++ "push rax"
    ]

genStdop "!=" = 
    [ tab ++ "pop rcx"
    , tab ++ "pop rbx"
    , tab ++ "cmp rbx, rcx"
    , tab ++ "setne al"
    , tab ++ "movzx rax, al"
    , tab ++ "push rax"
    ]

genStdop "<" = 
    [ tab ++ "pop rcx"
    , tab ++ "pop rbx"
    , tab ++ "cmp rbx, rcx"
    , tab ++ "setl al"
    , tab ++ "movzx rax, al"
    , tab ++ "push rax"
    ]

genStdop "<=" = 
    [ tab ++ "pop rcx"
    , tab ++ "pop rbx"
    , tab ++ "cmp rbx, rcx"
    , tab ++ "setle al"
    , tab ++ "movzx rax, al"
    , tab ++ "push rax"
    ]

genStdop ">" = 
    [ tab ++ "pop rcx"
    , tab ++ "pop rbx"
    , tab ++ "cmp rbx, rcx"
    , tab ++ "setg al"
    , tab ++ "movzx rax, al"
    , tab ++ "push rax"
    ]

genStdop ">=" = 
    [ tab ++ "pop rcx"
    , tab ++ "pop rbx"
    , tab ++ "cmp rbx, rcx"
    , tab ++ "setge al"
    , tab ++ "movzx rax, al"
    , tab ++ "push rax"
    ]

genStdop "f+" = 
    [ tab ++ "pop rax"
    , tab ++ "pop rbx"
    , tab ++ "movq xmm0, rbx"
    , tab ++ "movq xmm1, rax"
    , tab ++ "addsd xmm0, xmm1"
    , tab ++ "movq rax, xmm0"
    , tab ++ "push rax"
    ]

genStdop "f-" = 
    [ tab ++ "pop rax"
    , tab ++ "pop rbx"
    , tab ++ "movq xmm0, rbx"
    , tab ++ "movq xmm1, rax"
    , tab ++ "subsd xmm0, xmm1"
    , tab ++ "movq rax, xmm0"
    , tab ++ "push rax"
    ]

genStdop "f*" = 
    [ tab ++ "pop rax"
    , tab ++ "pop rbx"
    , tab ++ "movq xmm0, rbx"
    , tab ++ "movq xmm1, rax"
    , tab ++ "mulsd xmm0, xmm1"
    , tab ++ "movq rax, xmm0"
    , tab ++ "push rax"
    ]

genStdop "f/" = 
    [ tab ++ "pop rax"
    , tab ++ "pop rbx"
    , tab ++ "movq xmm0, rbx"
    , tab ++ "movq xmm1, rax"
    , tab ++ "divsd xmm0, xmm1"
    , tab ++ "movq rax, xmm0"
    , tab ++ "push rax"
    ]

genStdop "dup" = 
    [ tab ++ "pop rax"
    , tab ++ "push rax"
    , tab ++ "push rax"
    ]

genStdop "drop" = 
    [ tab ++ "pop rax" ]

genStdop "swap" = 
    [ tab ++ "pop rax"
    , tab ++ "pop rbx"
    , tab ++ "push rax"
    , tab ++ "push rbx"
    ]

genStdop "over" = 
    [ tab ++ "pop rax"
    , tab ++ "pop rbx"
    , tab ++ "push rbx"
    , tab ++ "push rax"
    , tab ++ "push rbx"
    ]

genStdop "rot" = 
    [ tab ++ "pop rax"
    , tab ++ "pop rbx"
    , tab ++ "pop rcx"
    , tab ++ "push rbx"
    , tab ++ "push rax"
    , tab ++ "push rcx"
    ]

genStdop "reach" = 
    [ tab ++ "pop rax"
    , tab ++ "mov rbx, rsp"
    , tab ++ "mov rcx, [rbx + rax*8]"
    , tab ++ "push rcx"
    ]

genStdop "print" = 
    [ tab ++ "pop rdi"
    , tab ++ "call __print_int__"
    ]

genStdop "fprint" = 
    [ tab ++ "pop rax"
    , tab ++ "movq xmm0, rax"
    , tab ++ "call __print_float__"
    ]

genStdop op = error $ "codegen: unimplemented stdop: " ++ op

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
