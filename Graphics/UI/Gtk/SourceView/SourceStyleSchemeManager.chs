{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SourceView
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
module Graphics.UI.Gtk.SourceView.SourceStyleSchemeManager (
-- * Types
  SourceStyleSchemeManager,
  SourceStyleSchemeManagerClass,

-- * Methods  
  sourceStyleSchemeManagerNew,
  sourceStyleSchemeManagerGetDefault,
  sourceStyleSchemeManagerSetSearchPath,
  sourceStyleSchemeManagerAppendSearchPath,
  sourceStyleSchemeManagerPrependSearchPath,
  sourceStyleSchemeManagerGetSearchPath,
  sourceStyleSchemeManagerGetSchemeIds,
  sourceStyleSchemeManagerGetScheme,
  sourceStyleSchemeManagerForceRescan,

-- * Attributes
  sourceStyleSchemeManagerStyleIds,
  sourceStyleSchemeManagerSearchPath
  ) where

{# context lib="gtk" prefix="gtk" #}

import Control.Monad	(liftM)
import Data.Maybe (fromMaybe)

import System.Glib.FFI
import System.Glib.GObject (constructNewGObject, makeNewGObject)
import System.Glib.UTFString
import System.Glib.Attributes
{#import System.Glib.Properties#}
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import Graphics.UI.Gtk.SourceView.SourceStyleScheme#}

-- | Creates a new style manager. If you do not need more than one style manager then use
-- 'sourceStyleSchemeManagerGetDefault' instead.
--
sourceStyleSchemeManagerNew :: IO SourceStyleSchemeManager
sourceStyleSchemeManagerNew = constructNewGObject mkSourceStyleSchemeManager $ liftM castPtr
  {#call unsafe source_style_scheme_manager_new#}

-- | Returns the default 'SourceStyleSchemeManager' instance.
--
sourceStyleSchemeManagerGetDefault :: IO SourceStyleSchemeManager -- ^ returns a 'SourceStyleSchemeManager'. Return value is owned by 'SourceView' library and must not be unref'ed.
sourceStyleSchemeManagerGetDefault = makeNewGObject mkSourceStyleSchemeManager $ liftM castPtr
  {#call unsafe source_style_scheme_manager_get_default#}

-- | Sets the list of directories where the manager looks for style scheme files. If dirs is 'Nothing', the
-- search path is reset to default.
--
sourceStyleSchemeManagerSetSearchPath :: SourceStyleSchemeManager -> Maybe [String] -> IO ()
sourceStyleSchemeManagerSetSearchPath ssm dirs =
  maybeWith withUTFStringArray0 dirs $ \dirsPtr -> do
    {#call unsafe source_style_scheme_manager_set_search_path#} ssm dirsPtr

-- | Appends path to the list of directories where the manager looks for style scheme files. See
-- 'sourceStyleSchemeManagerSetSearchPath' for details.
--
sourceStyleSchemeManagerAppendSearchPath :: SourceStyleSchemeManager 
                                         -> String  -- ^ @path@    a directory or a filename.     
                                         -> IO ()
sourceStyleSchemeManagerAppendSearchPath ssm dir =
  withUTFString dir $ {#call unsafe source_style_scheme_manager_append_search_path#} ssm

-- | Prepends path to the list of directories where the manager looks for style scheme files. See
-- 'sourceStyleSchemeManagerSetSearchPath' for details.
--
sourceStyleSchemeManagerPrependSearchPath :: SourceStyleSchemeManager 
                                          -> String  -- ^ @path@    a directory or a filename.     
                                          -> IO ()
sourceStyleSchemeManagerPrependSearchPath ssm dir =
  withUTFString dir $ {#call unsafe source_style_scheme_manager_prepend_search_path#} ssm

-- | Returns the current search path for the manager. See
-- 'sourceStyleSchemeManagerSetSearchPath' for details.
--
sourceStyleSchemeManagerGetSearchPath :: SourceStyleSchemeManager 
                                      -> IO [String]
sourceStyleSchemeManagerGetSearchPath ssm = do
  dirsPtr <- {#call unsafe source_style_scheme_manager_get_search_path#} ssm
  peekUTFStringArray0 dirsPtr

-- | Returns the ids of the available style schemes.
--
sourceStyleSchemeManagerGetSchemeIds :: SourceStyleSchemeManager -> IO [String]
sourceStyleSchemeManagerGetSchemeIds ssm = do
  idsPtr <- {#call unsafe source_style_scheme_manager_get_scheme_ids#} ssm
  liftM (fromMaybe []) $ maybePeek peekUTFStringArray0 idsPtr

-- | Looks up style scheme by id.
--
sourceStyleSchemeManagerGetScheme :: SourceStyleSchemeManager 
                                  -> String  -- ^ @schemeId@ style scheme id to find
                                  -> IO SourceStyleScheme
sourceStyleSchemeManagerGetScheme ssm id = makeNewGObject mkSourceStyleScheme $
  withUTFString id $ {#call unsafe source_style_scheme_manager_get_scheme#} ssm

-- | Mark any currently cached information about the available style scehems as invalid. All the
-- available style schemes will be reloaded next time the manager is accessed.
--
sourceStyleSchemeManagerForceRescan :: SourceStyleSchemeManager -> IO ()
sourceStyleSchemeManagerForceRescan =
  {#call unsafe source_style_scheme_manager_force_rescan#}

-- | List of the ids of the available style schemes.
--
sourceStyleSchemeManagerStyleIds :: ReadAttr SourceStyleSchemeManager [String]
sourceStyleSchemeManagerStyleIds =
  readAttrFromBoxedOpaqueProperty (liftM (fromMaybe []) . maybePeek peekUTFStringArray0 . castPtr)
                                  "style-ids" {#call pure g_strv_get_type#}

-- | List of directories and files where the style schemes are located.
--
sourceStyleSchemeManagerSearchPath :: ReadWriteAttr SourceStyleSchemeManager [String] (Maybe [String])
sourceStyleSchemeManagerSearchPath =
  newAttr (objectGetPropertyBoxedOpaque (peekUTFStringArray0 . castPtr) gtype "search-path")
          (objectSetPropertyBoxedOpaque (\dirs f -> maybeWith withUTFStringArray0 dirs (f . castPtr)) gtype "search-path")
  where gtype = {#call pure g_strv_get_type#}
