{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceIter
--
--  Author : Peter Gavin
--  derived from sourceview bindings by Axel Simon and Duncan Coutts
--
--  Created: 18 December 2008
--
--  Copyright (C) 2004-2008 Peter Gavin, Duncan Coutts, Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Adds extra useful methods for "TextIter" for searching forwards and
-- backwards within a region in the buffer and matching brackets.
--
-- * There is no SourceIter object, just extra methods for "TextIter"
--
module Graphics.UI.Gtk.SourceView.SourceIter (
-- * Enums
  SourceSearchFlags(..),

-- * Methods
  sourceIterForwardSearch,
  sourceIterBackwardSearch,
) where

import Control.Monad	(liftM)
import Data.Maybe    (fromMaybe)

import System.Glib.FFI
import System.Glib.Flags		(Flags, fromFlags)
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import Graphics.UI.Gtk.SourceView.Enums
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import Graphics.UI.Gtk.Multiline.TextIter#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Searches forward for str. Any match is returned by setting @matchStart@ to the first character of the
-- match and @matchEnd@ to the first character after the match. The search will not continue past
-- limit. Note that a search is a linear or O(n) operation, so you may wish to use limit to avoid
-- locking up your UI on large buffers.
-- 
-- If the 'SourceSearchVisibleOnly' flag is present, the match may have invisible text
-- interspersed in str. i.e. str will be a possibly-noncontiguous subsequence of the matched
-- range. similarly, if you specify 'SourceSearchTextOnly', the match may have pixbufs or child
-- widgets mixed inside the matched range. If these flags are not given, the match must be exact; the
-- special 0xFFFC character in str will match embedded pixbufs or child widgets. If you specify the
-- 'SourceSearchCaseInsensitive' flag, the text will be matched regardless of what case it is in.
-- 
-- Same as 'textIterForwardSearch', but supports case insensitive searching.
sourceIterForwardSearch :: TextIter -> String -> [SourceSearchFlags] -> 
                           Maybe TextIter -> IO (Maybe (TextIter, TextIter))
sourceIterForwardSearch ti str flags limit = do
   start  <- makeEmptyTextIter
   end <- makeEmptyTextIter
   found <- liftM toBool $ withUTFString str $ \cStr ->
     {#call unsafe source_iter_forward_search#} ti cStr
       ((fromIntegral.fromFlags) flags) start end
       (fromMaybe (TextIter nullForeignPtr) limit)
   return $ if found then Just (start,end) else Nothing

-- | same as 'textIterForwardSearch' but allows
-- case insensitive search and possibly in the future regular expressions.
--
sourceIterBackwardSearch :: TextIter -> String -> [SourceSearchFlags] -> 
                           Maybe TextIter -> IO (Maybe (TextIter, TextIter))
sourceIterBackwardSearch ti str flags limit = do
   start  <- makeEmptyTextIter
   end <- makeEmptyTextIter
   found <- liftM toBool $ withUTFString str $ \cStr ->
     {#call unsafe source_iter_backward_search#} ti cStr
       ((fromIntegral.fromFlags) flags) start end
       (fromMaybe (TextIter nullForeignPtr) limit)
   return $ if found then Just (start,end) else Nothing
