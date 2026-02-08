import Lexer
import Parser
import Codegen
import Typecheck

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (partition)

import System.Process (callProcess)
import System.Directory (removeFile)

data Options = Options
    { opt_unsafe :: Bool
    , opt_run    :: Bool
    , opt_silent :: Bool
    , opt_no_asm :: Bool
    }

default_options :: Options
default_options = Options False False False False

parse_options :: [String] -> (Options, String)
parse_options args =
    let (flags, files) = partition (\a -> head a == '-') args
        opts = foldl apply default_options flags
        filename =
            case files of
                [f] -> f
                _   -> error "expected exactly one input file"
    in (opts, filename)

  where
    apply o "--unsafe" = o { opt_unsafe = True }
    apply o "-r"       = o { opt_run = True }
    apply o "--run"    = o { opt_run = True }
    apply o "-s"       = o { opt_silent = True }
    apply o "--silent" = o { opt_silent = True }
    apply o "--no-asm" = o { opt_no_asm = True }
    apply _ f          = error ("unknown flag: " ++ f)

log_msg :: Options -> String -> IO ()
log_msg opts msg =
    if opt_silent opts then pure () else putStrLn msg

main :: IO ()
main = do
    args <- getArgs
    let (opts, filename) = parse_options args
        filename_no_ext = head $ splitOn "." filename

    source <- readFile filename
    tokens <- lexer filename source

    prog <- case tokens of
        Left e  -> error e
        Right p -> pure p

    parsed_prog <- case parseProgram filename prog of
        Left e  -> error e
        Right p -> pure p

    checked_prog <-
        if opt_unsafe opts
        then pure parsed_prog
        else do
            log_msg opts $ "hssbpl: " ++ filename ++ ": checking types"
            case typecheckProgram parsed_prog of
                Left e  -> error (show e)
                Right p -> pure p

    log_msg opts $ "hssbpl: " ++ filename ++ ": generating NASM"

    let nasm_code = generateNASMx86_64 checked_prog
        asm_file  = filename_no_ext ++ ".nasm"
        obj_file  = filename_no_ext ++ ".o"
        exe_file  = filename_no_ext

    writeFile asm_file nasm_code

    callProcess "nasm" ["-felf64", "-g", asm_file]
    callProcess "ld"   [obj_file, "-o", exe_file]

    log_msg opts $ "hssbpl: " ++ filename ++ ": compilation successful"

    removeFile obj_file
    if opt_no_asm opts then removeFile asm_file else pure ()
    if opt_run opts then callProcess ("./" ++ exe_file) [] else pure ()
