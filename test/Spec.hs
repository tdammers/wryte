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
    wryteLn "Bye!"
