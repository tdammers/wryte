{-#LANGUAGE OverloadedStrings #-}
import Text.Wryte
import Control.Monad (forM_)

main :: IO ()
main = putStrLn $ runWryte_ defWryteOptions code

code :: Wryte String ()
code = do
    wryteLn "Hi!"
    indented $ do
        wryteLn "How is life"
        wryteLn "and everything?"
    wryte "These are aligned: "
    aligned $ do
        wryteLn "adam"
        wryteLn "eve"
        wryteLn "alice"
        wryteLn "bob"
    wryteLn "These are indented: "
    indented $ do
        wryteLn "adam"
        wryteLn "eve"
        wryteLn "alice"
        wryteLn "bob"
    indented $ do
        wryte "These are both: "
        aligned $ do
            wryteLn "adam"
            wryteLn "eve"
            wryteLn "alice"
            wryteLn "bob"
    wryte "Let's align: "
    aligned $ do
        wryteLn "one level deep"
        wryte "a second: "
        aligned $ do
            wryteLn "level"
            wryteLn "deep"
        wryteLn "and back to"
        wryteLn "level 1"
    forM_ [minBound..maxBound] $ \style -> indented $ do
        wryteLn (show style)
        wryteList
            (ListOptions style Parentheses ",")
            [ wryte "things"
            , wryteLn "to" >> wryte "do"
            , wryte "today"
            ]
    wryteLn ""
    indented $ importLines "Hello,\nworld\n  indented"
    wryteLn "Bye!"
