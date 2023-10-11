-- | A datatype for tokens.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.StdToken
  ( StdToken(..)
  -- * optics
  , tokenType
  , tokenText
  , tokenDeco
  -- * constructor for Alex scanners
  , mkAlexToken
  ) where

import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLenses)

-- | A datatype for tokens.  Type synonyms will probably be used to
-- refer to its specializations.
data StdToken ty txt deco =
  Token
    { _tokenType :: ty
    , _tokenText :: txt
    , _tokenDeco :: deco
    }
  deriving (Data, Generic, Show, Typeable)

makeLenses ''StdToken

instance Functor (StdToken ty txt) where
  fmap f (Token ty txt deco) = Token ty txt (f deco)

instance Bifunctor (StdToken ty) where
  bimap f g (Token ty txt deco) = Token ty (f txt) (g deco)

instance Foldable (StdToken ty txt) where
  foldMap f (Token _ty _txt deco) = f deco

instance Bifoldable (StdToken ty) where
  bifoldMap f g (Token _ty txt deco) = f txt <> g deco

instance Traversable (StdToken t txt) where
  traverse f (Token ty txt deco) = Token ty txt <$> f deco

instance Bitraversable (StdToken t) where
  bitraverse f g (Token ty txt deco) = Token ty <$> f txt <*> g deco

-- | Can be used to create tokens in an Alex scanner that uses the
-- \"posn\" wrapper.
mkAlexToken :: ty -> posn -> String -> StdToken ty String posn
mkAlexToken ty posn txt = Token ty txt posn
