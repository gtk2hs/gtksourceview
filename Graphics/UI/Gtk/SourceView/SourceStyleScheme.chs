{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceStyleScheme
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
module Graphics.UI.Gtk.SourceView.SourceStyleScheme (
-- * Description
-- | 'SourceStyleScheme' contains all the text styles to be used in 'SourceView' and
-- 'SourceBuffer'. For instance, it contains text styles for syntax highlighting, it may contain
-- foreground and background color for non-highlighted text, color for the line numbers, etc.
--
-- Style schemes are stored in XML files. The format of a scheme file is the documented in the style
-- scheme reference.

-- * Types
  SourceStyleScheme,
  SourceStyleSchemeClass,

-- * Methods
  castToSourceStyleScheme,
  sourceStyleSchemeGetId,
  sourceStyleSchemeGetName,
  sourceStyleSchemeGetDescription,
  sourceStyleSchemeGetAuthors,
  sourceStyleSchemeGetFilename,
  sourceStyleSchemeGetStyle,

-- * Attributes
  sourceStyleSchemeDescription,
  sourceStyleSchemeFilename,
  sourceStyleSchemeId,
  sourceStyleSchemeName,
  ) where

import Control.Monad    (liftM)

import Graphics.UI.Gtk.SourceView.SourceStyle
import System.Glib.Attributes
import System.Glib.FFI
import System.Glib.GObject      (makeNewGObject)
import System.Glib.UTFString

{#import Graphics.UI.Gtk.SourceView.SourceStyle.Internal#}
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import System.Glib.Properties#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- |
--
sourceStyleSchemeGetId :: (SourceStyleSchemeClass sss, GlibString string) => sss
                       -> IO string -- ^ returns scheme id.
sourceStyleSchemeGetId ss =
  {#call source_style_scheme_get_id#} (toSourceStyleScheme ss) >>= peekUTFString

-- |
--
sourceStyleSchemeGetName :: (SourceStyleSchemeClass sss, GlibString string) => sss
                         -> IO string -- ^ returns scheme name.
sourceStyleSchemeGetName ss =
  {#call source_style_scheme_get_name#} (toSourceStyleScheme ss) >>= peekUTFString

-- |
--
sourceStyleSchemeGetDescription :: (SourceStyleSchemeClass sss, GlibString string) => sss
                                -> IO string -- ^ returns scheme description (if defined) or empty.
sourceStyleSchemeGetDescription ss =
  {#call source_style_scheme_get_description#} (toSourceStyleScheme ss) >>= peekUTFString

-- |
--
sourceStyleSchemeGetAuthors :: (SourceStyleSchemeClass sss, GlibString string) => sss
                            -> IO [string] -- ^ returns an array containing the scheme authors or empty if no author is specified by the style scheme.
sourceStyleSchemeGetAuthors ss =
  {#call source_style_scheme_get_authors#} (toSourceStyleScheme ss) >>= peekUTFStringArray0

-- |
--
sourceStyleSchemeGetFilename :: (SourceStyleSchemeClass sss, GlibString string) => sss
                             -> IO string -- ^ returns scheme file name if the scheme was created parsing a style scheme file or empty in the other cases.
sourceStyleSchemeGetFilename ss =
  {#call source_style_scheme_get_filename#} (toSourceStyleScheme ss) >>= peekUTFString

-- |
--
sourceStyleSchemeGetStyle :: (SourceStyleSchemeClass sss, GlibString string) => sss
                          -> string  -- ^ @styleId@ id of the style to retrieve.
                          -> IO SourceStyle -- ^ returns  style which corresponds to @styleId@ in the scheme
sourceStyleSchemeGetStyle ss id = do
  styleObj <- makeNewGObject mkSourceStyleObject $
              withUTFString id ({#call source_style_scheme_get_style#} (toSourceStyleScheme ss))
  sourceStyleFromObject styleObj

-- | Style scheme description.
--
-- Default value: \"\"
--
sourceStyleSchemeDescription :: (SourceStyleSchemeClass sss, GlibString string) => ReadAttr sss string
sourceStyleSchemeDescription = readAttrFromStringProperty "description"

-- | Style scheme filename or 'Nothing'.
--
-- Default value: \"\"
--
sourceStyleSchemeFilename :: (SourceStyleSchemeClass sss, GlibFilePath fp) => ReadAttr sss fp
sourceStyleSchemeFilename = readAttrFromFilePathProperty "filename"

-- | Style scheme id, a unique string used to identify the style scheme in 'SourceStyleSchemeManager'.
--
-- Default value: \"\"
--
sourceStyleSchemeId :: (SourceStyleSchemeClass sss, GlibString string) => ReadAttr sss string
sourceStyleSchemeId = readAttrFromStringProperty "id"

-- | Style scheme name, a translatable string to present to user.
--
-- Default value: \"\"
--
sourceStyleSchemeName :: (SourceStyleSchemeClass sss, GlibString string) => ReadAttr sss string
sourceStyleSchemeName = readAttrFromStringProperty "name"
