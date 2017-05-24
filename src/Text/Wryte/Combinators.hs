{-#LANGUAGE OverloadedStrings #-}
module Text.Wryte.Combinators
where

import Text.Wryte.Core
import Data.String (IsString (..))
import Control.Monad (when, forM_)

data ListOptions t =
    ListOptions
        { listStyle :: ListStyle
        , listBracketType :: BracketType t
        , listSeparator :: t
        }

data ListStyle
    -- | @
    -- { Foo
    -- , Bar
    -- , Baz
    -- }
    -- @
    = LeadingSeparator
    -- | @
    -- { Foo,
    --   Bar,
    --   Baz
    -- }
    | TrailingSeparatorAligned
    -- | @
    -- {
    --     Foo,
    --     Bar,
    --     Baz
    -- }
    | TrailingSeparatorIndented
    -- | @
    -- { Foo, Bar, Baz }
    | InlineList
    deriving (Show, Read, Eq, Enum, Bounded, Ord)

data BracketType t
    = Parentheses
    | CurlyBrackets
    | SquareBrackets
    | AngleBrackets
    | CustomBrackets t t

bracketStrOpen :: IsString t => BracketType t -> t
bracketStrOpen Parentheses = "("
bracketStrOpen CurlyBrackets = "{"
bracketStrOpen SquareBrackets = "["
bracketStrOpen AngleBrackets = "<"
bracketStrOpen (CustomBrackets a _) = a

bracketStrClose :: IsString t => BracketType t -> t
bracketStrClose Parentheses = ")"
bracketStrClose CurlyBrackets = "}"
bracketStrClose SquareBrackets = "]"
bracketStrClose AngleBrackets = ">"
bracketStrClose (CustomBrackets a _) = a

wryteList :: StrLen t
          => IsString t
          => Monoid t
          => ListOptions t
          -> [Wryte t ()]
          -> Wryte t ()
wryteList ls items = do
    wryte (bracketStrOpen . listBracketType $ ls)
    wryte " "
    when (listStyle ls == TrailingSeparatorIndented) eol

    let wrapper =
            case listStyle ls of
                TrailingSeparatorIndented -> indented
                InlineList -> aligned
                _ -> id
        innerWrapper =
            if listStyle ls == InlineList
                then id
                else aligned

    wrapper $ case items of
        [] -> pure ()
        (x:xs) -> do
            innerWrapper x
            when (listStyle ls `elem` [LeadingSeparator]) eol
            forM_ xs $ \x -> do
                wryte (listSeparator ls)
                if (listStyle ls `elem` [TrailingSeparatorAligned, TrailingSeparatorIndented])
                    then eol
                    else wryte " "
                when (listStyle ls `elem` [TrailingSeparatorAligned]) $
                    wryte "  "
                innerWrapper x
                when (listStyle ls == LeadingSeparator) eol

    when (listStyle ls `elem` [TrailingSeparatorAligned, TrailingSeparatorIndented]) eol
    when (listStyle ls == InlineList) $ wryte " "
    wryte (bracketStrClose . listBracketType $ ls)
    when (listStyle ls /= InlineList) eol
