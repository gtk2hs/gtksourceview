{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SourceView
--
--  Author : Peter Gavin, Andy Stewart
--  derived from sourceview bindings by Axel Simon and Duncan Coutts
--
--  Created: 18 December 2008
--
--  Copyright (C) 2004-2008 Peter Gavin, Duncan Coutts, Axel Simon
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
module Graphics.UI.Gtk.SourceView.SourceLanguageManager (
-- * Description
-- | 'SourceLanguageManager' is an object which processes language description files and creates and
-- stores 'SourceLanguage' objects, and provides API to access them. Use
-- 'sourceLanguageManagerGetDefault' to retrieve the default instance of
-- 'SourceLanguageManager', and 'sourceLanguageManagerGuessLanguage' to get a
-- 'SourceLanguage' for given file name and content type.

-- * Types
  SourceLanguageManager,
  SourceLanguageManagerClass,

-- * Methods
  castToSourceLanguageManager,
  sourceLanguageManagerNew,
  sourceLanguageManagerGetDefault,
  sourceLanguageManagerSetSearchPath,
  sourceLanguageManagerGetSearchPath,
  sourceLanguageManagerGetLanguageIds,
  sourceLanguageManagerGetLanguage,
  sourceLanguageManagerGuessLanguage,

-- * Attributes
  sourceLanguageManagerLanguageIds,
  sourceLanguageManagerSearchPath,
  ) where

import Control.Monad	(liftM)
import Data.Maybe (fromMaybe)

import System.Glib.FFI
import System.Glib.GObject (constructNewGObject, makeNewGObject)
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
{#import Graphics.UI.Gtk.SourceView.Types#}

{# context lib="gtk" prefix="gtk" #}

-- | Creates a new language manager. If you do not need more than one language manager or a private
-- language manager instance then use 'sourceLanguageManagerGetDefault' instead.
--
sourceLanguageManagerNew :: IO SourceLanguageManager
sourceLanguageManagerNew = constructNewGObject mkSourceLanguageManager $ liftM castPtr
  {#call unsafe source_language_manager_new#}

-- | Returns the default 'SourceLanguageManager' instance.
--
sourceLanguageManagerGetDefault :: IO SourceLanguageManager
sourceLanguageManagerGetDefault = makeNewGObject mkSourceLanguageManager $ liftM castPtr
  {#call unsafe source_language_manager_new#}

-- | Sets the list of directories where the lm looks for language files. If dirs is 'Nothing', the search path
-- is reset to default.
-- 
-- Note
-- 
-- At the moment this function can be called only before the language files are loaded for the first
-- time. In practice to set a custom search path for a 'SourceLanguageManager', you have to call this
-- function right after creating it.
--
sourceLanguageManagerSetSearchPath :: SourceLanguageManagerClass slm => slm -> Maybe [String] -> IO ()
sourceLanguageManagerSetSearchPath slm dirs =
  maybeWith withUTFStringArray0 dirs $ \dirsPtr -> do
    {#call unsafe source_language_manager_set_search_path#} (toSourceLanguageManager slm) dirsPtr

-- | Gets the list directories where lm looks for language files.
--
sourceLanguageManagerGetSearchPath :: SourceLanguageManagerClass slm => slm -> IO [String]
sourceLanguageManagerGetSearchPath slm = do
  dirsPtr <- {#call unsafe source_language_manager_get_search_path#} (toSourceLanguageManager slm)
  liftM (fromMaybe []) $ maybePeek peekUTFStringArray0 dirsPtr

-- | Returns the ids of the available languages.
--
sourceLanguageManagerGetLanguageIds :: SourceLanguageManagerClass slm => slm -> IO [String]
sourceLanguageManagerGetLanguageIds slm = do
  idsPtr <- {#call unsafe source_language_manager_get_language_ids#} (toSourceLanguageManager slm)
  liftM (fromMaybe []) $ maybePeek peekUTFStringArray0 idsPtr

-- | Gets the 'SourceLanguage' identified by the given id in the language manager.
--
sourceLanguageManagerGetLanguage :: SourceLanguageManagerClass slm => slm 
                                 -> String  -- ^ @id@      a language id.
                                 -> IO (Maybe SourceLanguage) -- ^ returns a 'SourceLanguage', or 'Nothing' if there is no language identified by the given id. 
sourceLanguageManagerGetLanguage slm id = do
  slPtr <- liftM castPtr $
    withUTFString id ({#call unsafe source_language_manager_get_language#} (toSourceLanguageManager slm))
  if slPtr /= nullPtr
    then liftM Just $ makeNewGObject mkSourceLanguage $ return slPtr
    else return Nothing

-- | Picks a 'SourceLanguage' for given file name and content type, according to the information in lang
-- files. Either filename or @contentType@ may be 'Nothing'.
--
sourceLanguageManagerGuessLanguage :: SourceLanguageManagerClass slm => slm 
                                   -> Maybe String  -- ^ @filename@     a filename in Glib filename encoding, or 'Nothing'.
                                   -> Maybe String  -- ^ @contentType@ a content type (as in GIO API), or 'Nothing'.
                                   -> IO (Maybe SourceLanguage) -- ^ returns      a 'SourceLanguage', or 'Nothing' if there is no suitable language for given filename and/or @contentType@. 
sourceLanguageManagerGuessLanguage slm filename contentType =
  maybeWith withUTFString filename $ \cFilename ->
  maybeWith withUTFString contentType $ \cContentType -> do
    slPtr <- liftM castPtr $
      {#call unsafe source_language_manager_guess_language#} (toSourceLanguageManager slm) cFilename cContentType
    if slPtr /= nullPtr
      then liftM Just $ makeNewGObject mkSourceLanguage $ return slPtr
      else return Nothing

-- | List of the ids of the available languages.
--
sourceLanguageManagerLanguageIds :: SourceLanguageManagerClass slm => ReadAttr slm [String]
sourceLanguageManagerLanguageIds =
  readAttrFromBoxedOpaqueProperty (liftM (fromMaybe []) . maybePeek peekUTFStringArray0 . castPtr)
                                  "language-ids" {#call pure g_strv_get_type#}

-- | List of directories where the language specification files (.lang) are located.
--
sourceLanguageManagerSearchPath :: SourceLanguageManagerClass slm => ReadWriteAttr slm [String] (Maybe [String])
sourceLanguageManagerSearchPath =
  newAttr (objectGetPropertyBoxedOpaque (peekUTFStringArray0 . castPtr) gtype "search-path")
          (objectSetPropertyBoxedOpaque (\dirs f -> maybeWith withUTFStringArray0 dirs (f . castPtr)) gtype "search-path")
  where gtype = {#call pure g_strv_get_type#}
