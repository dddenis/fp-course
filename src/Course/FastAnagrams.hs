{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams word fp =
  (\c -> fromSet $ S.intersection (toSet $ permutations word) (toSet $ lines c)) <$> readFile fp

toSet :: List Chars -> S.Set NoCaseString
toSet = S.fromList . hlist . (NoCaseString <$>)

fromSet :: S.Set NoCaseString -> List Chars
fromSet = (ncString <$>) . listh . S.toList

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  } deriving Ord

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
