{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceMark
--
--  Author : Duncan Coutts, Andy Stewart
--  derived from GtkTextView bindings by Axel Simon
--
--  Created: 26 October 2003
--
--  Copyright (C) 2003-2005 Duncan Coutts, Axel Simon
--  Copyright (C) 2010 Andy Stewart
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
module Graphics.UI.Gtk.SourceView.SourceMark (
-- * Description
-- | A 'SourceMark' marks a position in the text where you want to display additional info. It is based
-- on 'TextMark' and thus is still valid after the text has changed though its position may change.
-- 
-- 'SourceMarks' are organised in categories which you have to set when you create the mark. Each
-- category can have a pixbuf and a priority associated using @gtkSourceViewSetMarkCategoryPixbuf@
-- and @gtkSourceViewSetMarkCategoryPriority@. The pixbuf will be displayed in the margin at the
-- line where the mark residents if the 'showLineMarks' property is set to 'True'. If there are
-- multiple marks in the same line, the pixbufs will be drawn on top of each other. The mark with the
-- highest priority will be drawn on top.

-- * Types
  SourceMark,

-- * Methods
  castToSourceMark,
  sourceMarkGetCategory,
  sourceMarkNext,
  sourceMarkPrev
) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject	(makeNewGObject)
{#import Graphics.UI.Gtk.SourceView.Types#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Returns the mark category
-- 
sourceMarkGetCategory :: SourceMark
                      -> IO String -- ^ returns the category of the 'SourceMark' 
sourceMarkGetCategory mark = do
  strPtr <- {#call unsafe source_mark_get_category#} mark
  markType <- peekUTFString strPtr
  {#call unsafe g_free#} (castPtr strPtr)
  return markType

-- | Returns the next 'SourceMark' in the buffer or 'Nothing' if the mark was not added to a buffer. If there
-- is no next mark, 'Nothing' will be returned.
-- 
-- If category is 'Nothing', looks for marks of any category
-- 
sourceMarkNext :: SourceMark 
               -> String  -- ^ @category@ a string specifying the mark category or 'Nothing' 
               -> IO SourceMark -- ^ returns  the next 'SourceMark' or 'Nothing'                
sourceMarkNext mark category = makeNewGObject mkSourceMark $
  withUTFString category $ {#call unsafe source_mark_next#} mark

-- | Returns the previous 'SourceMark' in the buffer or 'Nothing' if the mark was not added to a buffer. If
-- there is no previous mark, 'Nothing' is returned.
-- 
-- If category is 'Nothing', looks for marks of any category
-- 
sourceMarkPrev :: SourceMark 
               -> String  -- ^ @category@ a string specifying the mark category or 'Nothing' 
               -> IO SourceMark -- ^ returns  the previous 'SourceMark' or 'Nothing'            
sourceMarkPrev mark category = makeNewGObject mkSourceMark $
  withUTFString category $ {#call unsafe source_mark_prev#} mark

