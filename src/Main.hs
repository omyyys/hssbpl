import Lexer
import Parser
import Codegen

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let filename = last args

    source <- readFile filename
    let result = lexer filename source
    
    print result
