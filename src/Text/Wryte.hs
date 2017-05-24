{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE FlexibleInstances #-}
module Text.Wryte
where

import Control.Monad.RWS
    ( RWS
    , ask, asks, get, gets, put, modify, tell
    , evalRWS
    , MonadReader
    , MonadWriter
    , MonadState
    )
import Data.String (IsString (..))
import Data.Monoid (Monoid, mconcat)
import Control.Monad (when)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText

class StrLen a where
    strlen :: a -> Int

instance StrLen [a] where
    strlen = length

instance StrLen Text.Text where
    strlen = Text.length

instance StrLen LText.Text where
    strlen = fromIntegral . LText.length

data WryteState =
    WryteState
        { wryteIndentLevel :: Int
        , wryteAlignWidth :: Int
        , wryteAtLineStart :: Bool
        , wryteCurrentColumn :: Int
        }
        deriving (Show)

defWryteState = WryteState 0 0 True 0

data WryteOptions t =
    WryteOptions
        { wryteIndentToken :: t
        , wryteAlignToken :: t
        }

defWryteOptions :: IsString t => WryteOptions t
defWryteOptions =
    WryteOptions "    " " "

newtype Wryte t a = Wryte { unWryte :: RWS (WryteOptions t) t WryteState a }
    deriving (MonadReader (WryteOptions t), MonadWriter t, MonadState WryteState, Functor, Applicative, Monad)

runWryte :: Monoid t => IsString t => WryteOptions t -> Wryte t a -> (a, t)
runWryte opts a = evalRWS (unWryte a) opts defWryteState

runWryte_ :: Monoid t => IsString t => WryteOptions t -> Wryte t () -> t
runWryte_ opts = snd . runWryte opts

wryte :: StrLen t
      => Monoid t
      => t
      -> Wryte t ()
wryte x = do
    atLineStart <- gets wryteAtLineStart
    when atLineStart $ wryteIndent >> wryteAlign
    modify (\s -> s
        { wryteAtLineStart = False
        , wryteCurrentColumn = wryteCurrentColumn s + strlen x
        })
    tell x

instance (StrLen t, Monoid t, IsString t) => IsString (Wryte t ()) where
    fromString = wryte . fromString

wryteIndent :: Monoid t => Wryte t ()
wryteIndent = do
    indentLevel <- gets wryteIndentLevel
    indentToken <- asks wryteIndentToken
    let indentStr = mconcat . replicate indentLevel $ indentToken
    tell indentStr

wryteAlign :: Monoid t => Wryte t ()
wryteAlign = do
    alignLevel <- gets wryteAlignWidth
    alignToken <- asks wryteAlignToken
    let alignStr = mconcat . replicate alignLevel $ alignToken
    tell alignStr

indented :: Monoid t => Wryte t a -> Wryte t a
indented x =
    incIndent *> x <* decIndent

incIndent :: Monoid t => Wryte t ()
incIndent = modify (\s -> s { wryteIndentLevel = succ (wryteIndentLevel s) })

decIndent :: Monoid t => Wryte t ()
decIndent = modify (\s -> s { wryteIndentLevel = pred (wryteIndentLevel s) })

aligned :: Monoid t => Wryte t a -> Wryte t a
aligned x = do
    oldAlign <- gets wryteAlignWidth
    newAlign <- gets wryteCurrentColumn
    setAlign newAlign *> x <* setAlign oldAlign
    
setAlign :: Monoid t => Int -> Wryte t ()
setAlign n = modify $ \s -> s { wryteAlignWidth = n }


eol :: Monoid t
    => IsString t
    => Wryte t ()
eol = do
    tell "\n"
    modify (\s -> s { wryteAtLineStart = True, wryteCurrentColumn = 0 })

wryteLn :: Monoid t
        => IsString t
        => StrLen t
        => t
        -> Wryte t ()
wryteLn s = wryte s >> eol
