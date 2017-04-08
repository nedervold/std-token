-- | A datatype for tokens.
{-# LANGUAGE TemplateHaskell #-}
module Text.StdToken(
    StdToken(..),
    tokenType,
    tokenText,
    tokenDeco,
    mkAlexToken) where

import Control.Lens.TH
import Data.Bifunctor

-- | A datatype for tokens.  Type synonyms will probably be used to
-- refer to its specializations.
data StdToken ty txt deco = Token {
    _tokenType :: ty,
    _tokenText :: txt,
    _tokenDeco :: deco
    }
    deriving (Eq, Ord, Show)

makeLenses ''StdToken

instance Functor (StdToken ty txt) where
    fmap f (Token ty txt deco) = Token ty txt (f deco)

instance Bifunctor (StdToken ty) where
    bimap f g (Token ty txt deco) = Token ty (f txt) (g deco)

-- | Can be used to create tokens in an Alex scanner that uses the
-- \"posn\" wrapper.
mkAlexToken :: ty -> posn -> String -> StdToken ty String posn
mkAlexToken ty posn txt = Token ty txt posn
