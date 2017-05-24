{-#LANGUAGE OverloadedStrings #-}
import Text.Wryte

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
    wryteLn "Bye!"
