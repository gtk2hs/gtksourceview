{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SourceCompletionItem
--
--  Author : Andy Stewart
--
--  Created: 08 Aug 2010
--
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
module Graphics.UI.Gtk.SourceView.SourceCompletionItem (
-- * Types
   SourceCompletionItem,
   SourceCompletionItemClass,

-- * Methods
   sourceCompletionItemNew,
   sourceCompletionItemNewWithMarkup,
   sourceCompletionItemNewFromStock,

-- * Attributes
   sourceCompletionItemIcon,
   sourceCompletionItemInfo,
   sourceCompletionItemLabel,
   sourceCompletionItemMarkup,
   sourceCompletionItemText,
) where

import Control.Monad	(liftM)
import Data.Maybe (fromMaybe)

import System.Glib.Attributes
import System.Glib.FFI
import System.Glib.GObject	(wrapNewGObject)
import System.Glib.Properties
import System.Glib.UTFString

{#import Graphics.UI.Gtk.SourceView.Signals#}
{#import Graphics.UI.Gtk.SourceView.Types#}

{# context lib="gtk" prefix="gtk" #}

-- | Create a new 'SourceCompletionItem' with label label, icon icon and extra information info. Both
-- icon and info can be 'Nothing' in which case there will be no icon shown and no extra information
-- available.
sourceCompletionItemNew :: String -- ^ @label@   The item label                   
                        -> String -- ^ @text@    The item text                    
                        -> Maybe Pixbuf -- ^ @icon@    The item icon or 'Nothing'
                        -> String -- ^ @info@    The item extra information       
                        -> IO SourceCompletionItem
sourceCompletionItemNew label text icon info =
  wrapNewGObject mkSourceCompletionItem $
  withUTFString label $ \ labelPtr -> 
  withUTFString text $ \ textPtr -> 
  withUTFString info $ \ infoPtr -> 
  {#call gtk_source_completion_item_new #}
    labelPtr
    textPtr
    (fromMaybe (Pixbuf nullForeignPtr) icon)
    infoPtr
  
  
-- | Create a new 'SourceCompletionItem' with markup label markup, icon icon and extra information
-- info. Both icon and info can be 'Nothing' in which case there will be no icon shown and no extra
-- information available.
sourceCompletionItemNewWithMarkup :: String
                                  -> String
                                  -> Maybe Pixbuf
                                  -> String
                                  -> IO SourceCompletionItem
sourceCompletionItemNewWithMarkup markup text icon info = 
  wrapNewGObject mkSourceCompletionItem $ 
  withUTFString markup $ \ markupPtr -> 
  withUTFString text $ \ textPtr -> 
  withUTFString info $ \ infoPtr -> 
  {#call gtk_source_completion_item_new_with_markup #}
    markupPtr
    textPtr
    (fromMaybe (Pixbuf nullForeignPtr) icon)
    infoPtr
  
-- | Creates a new 'SourceCompletionItem' from a stock item. If label is 'Nothing', the stock label will be
-- used.
sourceCompletionItemNewFromStock :: Maybe String -- ^ @label@   The item label or 'Nothing'
                                 -> String -- ^ @text@    The item text                                  
                                 -> String -- ^ @stock@   The stock icon                                 
                                 -> String -- ^ @info@    The item extra information                     
                                 -> IO SourceCompletionItem
sourceCompletionItemNewFromStock label text stock info =
  wrapNewGObject mkSourceCompletionItem $
  maybeWith withUTFString label $ \ labelPtr -> 
  withUTFString text $ \ textPtr -> 
  withUTFString stock $ \ stockPtr -> 
  withUTFString info $ \ infoPtr -> 
  {#call gtk_source_completion_item_new_from_stock #}
    labelPtr
    textPtr
    stockPtr
    infoPtr
   
-- | Icon to be shown for this proposal.
sourceCompletionItemIcon :: SourceCompletionItemClass item => Attr item Pixbuf
sourceCompletionItemIcon = newAttrFromObjectProperty "icon"
                           {# call pure unsafe gdk_pixbuf_get_type #}

-- | Optional extra information to be shown for this proposal.
-- 
-- Default value: \"\"
sourceCompletionItemInfo :: SourceCompletionItemClass item => Attr item String
sourceCompletionItemInfo = newAttrFromStringProperty "info"

-- | Optional extra labelrmation to be shown for this proposal.
-- 
-- Default value: \"\"
sourceCompletionItemLabel :: SourceCompletionItemClass item => Attr item String
sourceCompletionItemLabel = newAttrFromStringProperty "label"

-- | Optional extra markuprmation to be shown for this proposal.
-- 
-- Default value: \"\"
sourceCompletionItemMarkup :: SourceCompletionItemClass item => Attr item String
sourceCompletionItemMarkup = newAttrFromStringProperty "markup"

-- | Optional extra textrmation to be shown for this proposal.
-- 
-- Default value: \"\"
sourceCompletionItemText :: SourceCompletionItemClass item => Attr item String
sourceCompletionItemText = newAttrFromStringProperty "text"
