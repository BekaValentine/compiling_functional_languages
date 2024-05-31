module Main where

-- import RealLanguage.Surface
import qualified RealLanguage.Elaboration as RE
import qualified RealLanguage.Example as RE
import qualified RealLanguage.Parser as P

main :: IO ()
main = do
    putStrLn (take 20 (repeat '-'))
    print RE.program

    putStrLn (take 20 (repeat '-'))
    print $ RE.runElab0 (RE.goal gl)

    putStrLn (take 20 (repeat '-'))
    let path = "./app/RealLanguage/example.src"
    src <- readFile path
    case P.parse path P.program src of
        Left err -> putStrLn err
        Right prog ->
            print $ RE.runElab0 (RE.goal (RE.ProgramValid [] prog))
     
    where
        d = []
        gl = RE.ProgramValid d RE.program