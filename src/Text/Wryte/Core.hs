{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE FlexibleInstances #-}
module Text.Wryte.Core
( Wryte
, runWryte
, runWryte_
, WryteOptions (..)
, defWryteOptions
, StrLen (..)
, wryte
, wryteLn
, eol
, aligned
, indented
)
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
import Control.Monad (when)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Data.Monoid (Monoid, mconcat)
import Control.Applicative ( Applicative, (*>), (<*), (<$>) )

class StrLen a where
    strlen :: a -> Int

instance StrLen [a] where
    strlen = length

instance StrLen Text.Text where
    strlen = Text.length

instance StrLen LText.Text where
    strlen = fromIntegral . LText.length

data WryteState t =
    WryteState
        { wryteIndentStack :: [t]
        , wryteAtLineStart :: Bool
        , wryteCurrentColumn :: Int
        }
        deriving (Show)

defWryteState = WryteState [] True 0

data WryteOptions t =
    WryteOptions
        { wryteIndentToken :: t
        , wryteAlignToken :: t
        }

defWryteOptions :: IsString t => WryteOptions t
defWryteOptions =
    WryteOptions "    " " "

newtype Wryte t a = Wryte { unWryte :: RWS (WryteOptions t) t (WryteState t) a }
    deriving (MonadReader (WryteOptions t), MonadWriter t, MonadState (WryteState t), Functor, Applicative, Monad)

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
    when atLineStart $ do
        indent <- mconcat . reverse <$> gets wryteIndentStack
        tell indent
        modify (\s -> s { wryteCurrentColumn = strlen indent })
    modify (\s -> s
        { wryteAtLineStart = False
        , wryteCurrentColumn = wryteCurrentColumn s + strlen x
        })
    tell x

instance (StrLen t, Monoid t, IsString t) => IsString (Wryte t ()) where
    fromString = wryte . fromString

indented :: Monoid t => Wryte t a -> Wryte t a
indented x = do
    ic <- asks wryteIndentToken
    prefixed ic x

pushIndent :: Monoid t => t -> Wryte t ()
pushIndent ic =
    modify (\s -> s { wryteIndentStack = ic : wryteIndentStack s })

popIndent :: Monoid t => Wryte t ()
popIndent =
    modify (\s -> s { wryteIndentStack = drop 1 (wryteIndentStack s) })

prefixed :: Monoid t => t -> Wryte t a -> Wryte t a
prefixed prefix x =
    pushIndent prefix *> x <* popIndent

aligned :: StrLen t => Monoid t => Wryte t a -> Wryte t a
aligned x = do
    oldIndentW <- strlen . mconcat <$> gets wryteIndentStack
    newIndentW <- gets wryteCurrentColumn
    alignToken <- asks wryteAlignToken
    let ic = mconcat $ replicate (newIndentW - oldIndentW) alignToken
    prefixed ic x

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

