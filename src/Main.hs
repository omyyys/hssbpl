import Lexer
import Parser
import Codegen
import Typecheck

import System.Environment (getArgs)
import Data.List.Split (splitOn)

import System.Process (callProcess)
import System.Directory (removeFile)

main :: IO ()
main = do
    args <- getArgs
    let filename = last args
        filename_no_ext = head $ splitOn "." filename

    source <- readFile filename
    tokens <- lexer filename source

    case tokens of 
        Left lexer_err -> error lexer_err
        Right prog -> do
            case parseProgram filename prog of 
                Left parser_err -> error parser_err
                Right parsed_prog -> do
                    putStrLn $ "hssbpl: " ++ filename ++ ": checking types" 
                    case typecheckProgram parsed_prog of 
                        Left type_err -> error $ show type_err
                        Right checked_prog -> do
                            putStrLn $ "hssbpl: " ++ filename ++ ": generating NASM"
                            let nasm_code = generateNASMx86_64 checked_prog 

                            writeFile (filename_no_ext ++ ".nasm") nasm_code
                            callProcess "nasm" ["-felf64", "-g", filename_no_ext ++ ".nasm"]
                            callProcess "ld"   [filename_no_ext ++ ".o", "-o", filename_no_ext]
            
                            putStrLn $ "hssbpl: " ++ filename ++ ": compilation successful"

                            removeFile $ filename_no_ext ++ ".o"
