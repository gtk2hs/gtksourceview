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
  SourceMarkClass,

-- * Methods
  castToSourceMark,
  sourceMarkNew,
  sourceMarkGetCategory,
  sourceMarkNext,
  sourceMarkPrev,

-- * Attributes
  sourceMarkCategory
) where

import Control.Monad	(liftM)

import System.Glib.Attributes
import System.Glib.FFI
import System.Glib.GObject	(makeNewGObject)
import System.Glib.Properties
import System.Glib.UTFString

{#import Graphics.UI.Gtk.SourceView.Types#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Creates a text mark. Add it to a buffer using 'textBufferAddMark'. If name is 'Nothing', the mark
-- is anonymous; otherwise, the mark can be retrieved by name using
-- 'textBufferGetMark'. Normally marks are created using the utility function
-- 'sourceBufferCreateMark'.
sourceMarkNew :: Maybe String -- ^ @name@     Name of the 'SourceMark', can be 'Nothing' when not using a name
              -> String  -- ^ @category@ is used to classify marks according to common characteristics (e.g. all the marks representing a bookmark could  
              -> IO SourceMark
sourceMarkNew name category = 
  makeNewGObject mkSourceMark $
  maybeWith withUTFString name $ \namePtr ->
  withUTFString category $ \categoryPtr ->
  {#call gtk_source_mark_new#}
    namePtr
    categoryPtr

-- | Returns the mark category
-- 
sourceMarkGetCategory :: SourceMarkClass mark => mark
                      -> IO String -- ^ returns the category of the 'SourceMark' 
sourceMarkGetCategory mark = do
  strPtr <- {#call unsafe source_mark_get_category#} (toSourceMark mark)
  markType <- peekUTFString strPtr
  {#call unsafe g_free#} (castPtr strPtr)
  return markType

-- | Returns the next 'SourceMark' in the buffer or 'Nothing' if the mark was not added to a buffer. If there
-- is no next mark, 'Nothing' will be returned.
-- 
-- If category is 'Nothing', looks for marks of any category
-- 
sourceMarkNext :: SourceMarkClass mark => mark 
               -> Maybe String  -- ^ @category@ a string specifying the mark category or 'Nothing' 
               -> IO (Maybe SourceMark) -- ^ returns  the next 'SourceMark' or 'Nothing'                
sourceMarkNext mark category = 
    maybeNull (makeNewGObject mkSourceMark) $
    maybeWith withUTFString category $ {#call unsafe source_mark_next#} (toSourceMark mark)

-- | Returns the previous 'SourceMark' in the buffer or 'Nothing' if the mark was not added to a buffer. If
-- there is no previous mark, 'Nothing' is returned.
-- 
-- If category is 'Nothing', looks for marks of any category
-- 
sourceMarkPrev :: SourceMarkClass mark => mark 
               -> Maybe String  -- ^ @category@ a string specifying the mark category or 'Nothing' 
               -> IO (Maybe SourceMark) -- ^ returns  the previous 'SourceMark' or 'Nothing'            
sourceMarkPrev mark category = 
    maybeNull (makeNewGObject mkSourceMark) $
    maybeWith withUTFString category $ {#call unsafe source_mark_prev#} (toSourceMark mark)

-- | The category of the 'SourceMark', classifies the mark and controls which pixbuf is used and with
-- which priority it is drawn.
-- Default value: \"\"
--
sourceMarkCategory :: SourceMarkClass mark => Attr mark String
sourceMarkCategory = newAttrFromStringProperty "category"
