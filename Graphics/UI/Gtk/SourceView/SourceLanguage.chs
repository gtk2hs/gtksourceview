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
--  Copyright (C) Andy Stewart
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
module Graphics.UI.Gtk.SourceView.SourceLanguage (
-- * Description
-- | 'SourceLanguage' encapsulates syntax and highlighting styles for a particular language. Use
-- 'SourceLanguageManager' to obtain a 'SourceLanguage' instance, and
-- 'sourceBufferSetLanguage' to apply it to a 'SourceBuffer'.

-- * Types
  SourceLanguage,
  SourceLanguageClass,

-- * Methods
  castToSourceLanguage,
  sourceLanguageGetId,
  sourceLanguageGetName,
  sourceLanguageGetSection,
  sourceLanguageGetHidden,
  sourceLanguageGetMetadata,
  sourceLanguageGetMimeTypes,
  sourceLanguageGetGlobs,
  sourceLanguageGetStyleName,
  sourceLanguageGetStyleIds,

-- * Attributes
  sourceLanguageHidden,
  sourceLanguageId,
  sourceLanguageName,
  sourceLanguageSection
  ) where

import Control.Monad	(liftM)
import Data.Maybe (fromMaybe)

import System.Glib.FFI
import System.Glib.UTFString
{#import System.Glib.Properties#}
import System.Glib.Attributes
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import Graphics.UI.Gtk.SourceView.SourceStyleScheme#}

{# context lib="gtk" prefix="gtk" #}


-- methods

-- | Returns the ID of the language. The ID is not locale-dependent.
--
sourceLanguageGetId :: SourceLanguageClass sl => sl
                    -> IO String -- ^ returns  the ID of language. The returned string is owned by language and should not be freed or modified.
sourceLanguageGetId sl =
  {#call unsafe source_language_get_id#} (toSourceLanguage sl) >>= peekUTFString

-- | Returns the localized name of the language.
--
sourceLanguageGetName :: SourceLanguageClass sl => sl 
                      -> IO String -- ^ returns  the name of language. The returned string is owned by language and should not be freed or modified.
sourceLanguageGetName sl =
  {#call unsafe source_language_get_name#} (toSourceLanguage sl) >>= peekUTFString

-- | Returns the localized section of the language. Each language belong to a section (ex. HTML belogs to
-- the Markup section).
--
sourceLanguageGetSection :: SourceLanguageClass sl => sl 
                         -> IO String -- ^ returns  the section of language. The returned string is owned by language and should not be freed or modified.
sourceLanguageGetSection sl =
  {#call unsafe source_language_get_section#} (toSourceLanguage sl) >>= peekUTFString

-- | Returns whether the language should be hidden from the user.
--
sourceLanguageGetHidden :: SourceLanguageClass sl => sl 
                        -> IO Bool -- ^ returns  'True' if the language should be hidden, 'False' otherwise. 
sourceLanguageGetHidden sl = liftM toBool $
  {#call unsafe source_language_get_hidden#} (toSourceLanguage sl)

-- |
--
sourceLanguageGetMetadata :: SourceLanguageClass sl => sl 
                          -> String  -- ^ @name@     metadata property name.
                          -> IO String -- ^ returns  value of property name stored in the metadata of language or empty if language doesn't contain that metadata
sourceLanguageGetMetadata sl name = do
  withUTFString name ({#call unsafe source_language_get_metadata#} (toSourceLanguage sl)) >>= peekUTFString

-- | Returns the mime types associated to this language. This is just an utility wrapper around
-- 'sourceLanguageGetMetadata ' to retrieve the "mimetypes" metadata property and split it into
-- an array.
--
sourceLanguageGetMimeTypes :: SourceLanguageClass sl => sl 
                           -> IO [String] -- ^ returns  an array containing the mime types or empty if no mime types are found. The        
sourceLanguageGetMimeTypes sl = do
  mimeTypesArray <- {#call unsafe source_language_get_mime_types#} (toSourceLanguage sl)
  mimeTypes <- liftM (fromMaybe []) $ maybePeek peekUTFStringArray0 mimeTypesArray
  {# call g_strfreev #} mimeTypesArray
  return mimeTypes

-- | Returns the globs associated to this language. This is just an utility wrapper around
-- 'sourceLanguageGetMetadata' to retrieve the "globs" metadata property and split it into an
-- array.
--
sourceLanguageGetGlobs :: SourceLanguageClass sl => sl 
                       -> IO [String] -- ^ returns  an array containing the globs or empty if no globs are found. 
sourceLanguageGetGlobs sl = do
  globsArray <- {#call unsafe source_language_get_globs#} (toSourceLanguage sl)
  globs <- liftM (fromMaybe []) $ maybePeek peekUTFStringArray0 globsArray
  {# call g_strfreev #} globsArray
  return globs

-- | Returns the name of the style with ID @styleId@ defined by this language.
sourceLanguageGetStyleName :: SourceLanguageClass sl => sl 
                           -> String -- ^ @styleId@ a style ID
                           -> IO String  -- ^ returns the name of the style with ID @styleId@ defined by this language or empty if the style has no name or there is no style with ID @styleId@ defined by this language. The returned string is owned by the language and must not be modified.
sourceLanguageGetStyleName sl styleId =
    withUTFString styleId $ \styleIdPtr ->
    {#call gtk_source_language_get_style_name#}
       (toSourceLanguage sl)
       styleIdPtr
    >>= peekUTFString

-- | Returns the ids of the styles defined by this language.
sourceLanguageGetStyleIds :: SourceLanguageClass sl => sl 
                          -> IO [String] -- ^ returns  an array containing ids of the styles defined by this language or empty if no style is defined. 
sourceLanguageGetStyleIds sl = do
  globsArray <- {#call gtk_source_language_get_style_ids#} (toSourceLanguage sl)
  globs <- liftM (fromMaybe []) $ maybePeek peekUTFStringArray0 globsArray
  {# call g_strfreev #} globsArray
  return globs

-- | Whether the language should be hidden from the user.
-- 
-- Default value: 'False'
--
sourceLanguageHidden :: SourceLanguageClass sl => ReadAttr sl Bool
sourceLanguageHidden = readAttrFromBoolProperty "hidden"

-- | Language id.
-- 
-- Default value: \"\"
--
sourceLanguageId :: SourceLanguageClass sl => ReadAttr sl String
sourceLanguageId = readAttrFromStringProperty "id"

-- | Language name.
-- 
-- Default value: \"\"
--
sourceLanguageName :: SourceLanguageClass sl => ReadAttr sl String
sourceLanguageName = readAttrFromStringProperty "name"

-- | Language section.
-- 
-- Default value: \"\"
--
sourceLanguageSection :: SourceLanguageClass sl => ReadAttr sl String
sourceLanguageSection = readAttrFromStringProperty "section"
