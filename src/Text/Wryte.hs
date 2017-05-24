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

data WryteState =
    WryteState
        { wryteIndentLevel :: Int
        , wryteAtLineStart :: Bool
        }
        deriving (Show)

defWryteState = WryteState 0 True

data WryteOptions t =
    WryteOptions
        { wryteIndentToken :: t
        }

defWryteOptions :: IsString t => WryteOptions t
defWryteOptions =
    WryteOptions "    "

newtype Wryte t a = Wryte { unWryte :: RWS (WryteOptions t) t WryteState a }
    deriving (MonadReader (WryteOptions t), MonadWriter t, MonadState WryteState, Functor, Applicative, Monad)

runWryte :: Monoid t => IsString t => WryteOptions t -> Wryte t a -> (a, t)
runWryte opts a = evalRWS (unWryte a) opts defWryteState

runWryte_ :: Monoid t => IsString t => WryteOptions t -> Wryte t () -> t
runWryte_ opts = snd . runWryte opts

wryte :: Monoid t => t -> Wryte t ()
wryte x = do
    atLineStart <- gets wryteAtLineStart
    when atLineStart $ wryteIndent
    modify (\s -> s { wryteAtLineStart = False })
    tell x

instance (Monoid t, IsString t) => IsString (Wryte t ()) where
    fromString = wryte . fromString

wryteIndent :: Monoid t => Wryte t ()
wryteIndent = do
    indentLevel <- gets wryteIndentLevel
    indentToken <- asks wryteIndentToken
    let indentStr = mconcat . replicate indentLevel $ indentToken
    tell indentStr

indented :: Monoid t => Wryte t a -> Wryte t a
indented x =
    incIndent *> x <* decIndent

incIndent :: Monoid t => Wryte t ()
incIndent = modify (\s -> s { wryteIndentLevel = succ (wryteIndentLevel s) })

decIndent :: Monoid t => Wryte t ()
decIndent = modify (\s -> s { wryteIndentLevel = pred (wryteIndentLevel s) })

eol :: Monoid t
    => IsString t
    => Wryte t ()
eol = do
    tell "\n"
    modify (\s -> s { wryteAtLineStart = True })

wryteLn :: Monoid t
        => IsString t
        => t
        -> Wryte t ()
wryteLn s = wryte s >> eol
