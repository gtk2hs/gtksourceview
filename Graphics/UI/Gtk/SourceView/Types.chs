{-# OPTIONS_HADDOCK hide #-}
-- -*-haskell-*-
-- -------------------- automatically generated file - do not edit ----------
--  Object hierarchy for the GIMP Toolkit (GTK) Binding for Haskell
--
--  Author : Axel Simon
--
--  Copyright (C) 2001-2005 Axel Simon
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
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- This file reflects the Gtk+ object hierarchy in terms of Haskell classes.
--
-- Note: the mk... functions were originally meant to simply be an alias
-- for the constructor. However, in order to communicate the destructor
-- of an object to objectNew, the mk... functions are now a tuple containing
-- Haskell constructor and the destructor function pointer. This hack avoids
-- changing all modules that simply pass mk... to objectNew.
--
module Graphics.UI.Gtk.SourceView.Types (

  module Graphics.UI.GtkInternals,
  SourceBuffer(SourceBuffer), SourceBufferClass,
  toSourceBuffer, 
  mkSourceBuffer, unSourceBuffer,
  castToSourceBuffer, gTypeSourceBuffer,
  SourceUndoManager(SourceUndoManager), SourceUndoManagerClass,
  toSourceUndoManager, 
  mkSourceUndoManager, unSourceUndoManager,
  castToSourceUndoManager, gTypeSourceUndoManager,
  SourceMark(SourceMark), SourceMarkClass,
  toSourceMark, 
  mkSourceMark, unSourceMark,
  castToSourceMark, gTypeSourceMark,
  SourceView(SourceView), SourceViewClass,
  toSourceView, 
  mkSourceView, unSourceView,
  castToSourceView, gTypeSourceView,
  SourceLanguage(SourceLanguage), SourceLanguageClass,
  toSourceLanguage, 
  mkSourceLanguage, unSourceLanguage,
  castToSourceLanguage, gTypeSourceLanguage,
  SourceLanguageManager(SourceLanguageManager), SourceLanguageManagerClass,
  toSourceLanguageManager, 
  mkSourceLanguageManager, unSourceLanguageManager,
  castToSourceLanguageManager, gTypeSourceLanguageManager,
  SourceGutter(SourceGutter), SourceGutterClass,
  toSourceGutter, 
  mkSourceGutter, unSourceGutter,
  castToSourceGutter, gTypeSourceGutter,
  SourceStyleObject(SourceStyleObject), SourceStyleObjectClass,
  toSourceStyleObject, 
  mkSourceStyleObject, unSourceStyleObject,
  castToSourceStyleObject, gTypeSourceStyleObject,
  SourceStyleScheme(SourceStyleScheme), SourceStyleSchemeClass,
  toSourceStyleScheme, 
  mkSourceStyleScheme, unSourceStyleScheme,
  castToSourceStyleScheme, gTypeSourceStyleScheme,
  SourceStyleSchemeManager(SourceStyleSchemeManager), SourceStyleSchemeManagerClass,
  toSourceStyleSchemeManager, 
  mkSourceStyleSchemeManager, unSourceStyleSchemeManager,
  castToSourceStyleSchemeManager, gTypeSourceStyleSchemeManager
  ) where

import Foreign.ForeignPtr (ForeignPtr, castForeignPtr, unsafeForeignPtrToPtr)
import Foreign.C.Types    (CULong, CUInt)
import System.Glib.GType	(GType, typeInstanceIsA)
{#import Graphics.UI.GtkInternals#}

{# context lib="gtk" prefix="gtk" #}

-- The usage of foreignPtrToPtr should be safe as the evaluation will only be
-- forced if the object is used afterwards
--
castTo :: (GObjectClass obj, GObjectClass obj') => GType -> String
                                                -> (obj -> obj')
castTo gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objFPtr)
      | typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr) objFPtr) gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName


-- *************************************************************** SourceBuffer

{#pointer *SourceBuffer foreign newtype #} deriving (Eq,Ord)

mkSourceBuffer = (SourceBuffer, objectUnrefFromMainloop)
unSourceBuffer (SourceBuffer o) = o

class TextBufferClass o => SourceBufferClass o
toSourceBuffer :: SourceBufferClass o => o -> SourceBuffer
toSourceBuffer = unsafeCastGObject . toGObject

instance SourceBufferClass SourceBuffer
instance TextBufferClass SourceBuffer
instance GObjectClass SourceBuffer where
  toGObject = GObject . castForeignPtr . unSourceBuffer
  unsafeCastGObject = SourceBuffer . castForeignPtr . unGObject

castToSourceBuffer :: GObjectClass obj => obj -> SourceBuffer
castToSourceBuffer = castTo gTypeSourceBuffer "SourceBuffer"

gTypeSourceBuffer :: GType
gTypeSourceBuffer =
  {# call fun unsafe gtk_source_buffer_get_type #}

-- ********************************************************** SourceUndoManager

{#pointer *SourceUndoManager foreign newtype #} deriving (Eq,Ord)

mkSourceUndoManager = (SourceUndoManager, objectUnrefFromMainloop)
unSourceUndoManager (SourceUndoManager o) = o

class TextMarkClass o => SourceUndoManagerClass o
toSourceUndoManager :: SourceUndoManagerClass o => o -> SourceUndoManager
toSourceUndoManager = unsafeCastGObject . toGObject

instance SourceUndoManagerClass SourceUndoManager
instance TextMarkClass SourceUndoManager
instance GObjectClass SourceUndoManager where
  toGObject = GObject . castForeignPtr . unSourceUndoManager
  unsafeCastGObject = SourceUndoManager . castForeignPtr . unGObject

castToSourceUndoManager :: GObjectClass obj => obj -> SourceUndoManager
castToSourceUndoManager = castTo gTypeSourceUndoManager "SourceUndoManager"

gTypeSourceUndoManager :: GType
gTypeSourceUndoManager =
  {# call fun unsafe gtk_source_undo_manager_get_type #}

-- ***************************************************************** SourceMark

{#pointer *SourceMark foreign newtype #} deriving (Eq,Ord)

mkSourceMark = (SourceMark, objectUnrefFromMainloop)
unSourceMark (SourceMark o) = o

class TextMarkClass o => SourceMarkClass o
toSourceMark :: SourceMarkClass o => o -> SourceMark
toSourceMark = unsafeCastGObject . toGObject

instance SourceMarkClass SourceMark
instance TextMarkClass SourceMark
instance GObjectClass SourceMark where
  toGObject = GObject . castForeignPtr . unSourceMark
  unsafeCastGObject = SourceMark . castForeignPtr . unGObject

castToSourceMark :: GObjectClass obj => obj -> SourceMark
castToSourceMark = castTo gTypeSourceMark "SourceMark"

gTypeSourceMark :: GType
gTypeSourceMark =
  {# call fun unsafe gtk_source_mark_get_type #}

-- ***************************************************************** SourceView

{#pointer *SourceView foreign newtype #} deriving (Eq,Ord)

mkSourceView = (SourceView, objectUnrefFromMainloop)
unSourceView (SourceView o) = o

class TextViewClass o => SourceViewClass o
toSourceView :: SourceViewClass o => o -> SourceView
toSourceView = unsafeCastGObject . toGObject

instance SourceViewClass SourceView
instance TextViewClass SourceView
instance ContainerClass SourceView
instance WidgetClass SourceView
instance ObjectClass SourceView
instance GObjectClass SourceView where
  toGObject = GObject . castForeignPtr . unSourceView
  unsafeCastGObject = SourceView . castForeignPtr . unGObject

castToSourceView :: GObjectClass obj => obj -> SourceView
castToSourceView = castTo gTypeSourceView "SourceView"

gTypeSourceView :: GType
gTypeSourceView =
  {# call fun unsafe gtk_source_view_get_type #}

-- ************************************************************* SourceLanguage

{#pointer *SourceLanguage foreign newtype #} deriving (Eq,Ord)

mkSourceLanguage = (SourceLanguage, objectUnrefFromMainloop)
unSourceLanguage (SourceLanguage o) = o

class GObjectClass o => SourceLanguageClass o
toSourceLanguage :: SourceLanguageClass o => o -> SourceLanguage
toSourceLanguage = unsafeCastGObject . toGObject

instance SourceLanguageClass SourceLanguage
instance GObjectClass SourceLanguage where
  toGObject = GObject . castForeignPtr . unSourceLanguage
  unsafeCastGObject = SourceLanguage . castForeignPtr . unGObject

castToSourceLanguage :: GObjectClass obj => obj -> SourceLanguage
castToSourceLanguage = castTo gTypeSourceLanguage "SourceLanguage"

gTypeSourceLanguage :: GType
gTypeSourceLanguage =
  {# call fun unsafe gtk_source_language_get_type #}

-- ****************************************************** SourceLanguageManager

{#pointer *SourceLanguageManager foreign newtype #} deriving (Eq,Ord)

mkSourceLanguageManager = (SourceLanguageManager, objectUnrefFromMainloop)
unSourceLanguageManager (SourceLanguageManager o) = o

class GObjectClass o => SourceLanguageManagerClass o
toSourceLanguageManager :: SourceLanguageManagerClass o => o -> SourceLanguageManager
toSourceLanguageManager = unsafeCastGObject . toGObject

instance SourceLanguageManagerClass SourceLanguageManager
instance GObjectClass SourceLanguageManager where
  toGObject = GObject . castForeignPtr . unSourceLanguageManager
  unsafeCastGObject = SourceLanguageManager . castForeignPtr . unGObject

castToSourceLanguageManager :: GObjectClass obj => obj -> SourceLanguageManager
castToSourceLanguageManager = castTo gTypeSourceLanguageManager "SourceLanguageManager"

gTypeSourceLanguageManager :: GType
gTypeSourceLanguageManager =
  {# call fun unsafe gtk_source_language_manager_get_type #}

-- *************************************************************** SourceGutter

{#pointer *SourceGutter foreign newtype #} deriving (Eq,Ord)

mkSourceGutter = (SourceGutter, objectUnrefFromMainloop)
unSourceGutter (SourceGutter o) = o

class SourceLanguageManagerClass o => SourceGutterClass o
toSourceGutter :: SourceGutterClass o => o -> SourceGutter
toSourceGutter = unsafeCastGObject . toGObject

instance SourceGutterClass SourceGutter
instance SourceLanguageManagerClass SourceGutter
instance GObjectClass SourceGutter where
  toGObject = GObject . castForeignPtr . unSourceGutter
  unsafeCastGObject = SourceGutter . castForeignPtr . unGObject

castToSourceGutter :: GObjectClass obj => obj -> SourceGutter
castToSourceGutter = castTo gTypeSourceGutter "SourceGutter"

gTypeSourceGutter :: GType
gTypeSourceGutter =
  {# call fun unsafe gtk_source_gutter_get_type #}

-- ********************************************************** SourceStyleObject

{#pointer *GtkSourceStyle as SourceStyleObject foreign newtype #} deriving (Eq,Ord)

mkSourceStyleObject = (SourceStyleObject, objectUnrefFromMainloop)
unSourceStyleObject (SourceStyleObject o) = o

class GObjectClass o => SourceStyleObjectClass o
toSourceStyleObject :: SourceStyleObjectClass o => o -> SourceStyleObject
toSourceStyleObject = unsafeCastGObject . toGObject

instance SourceStyleObjectClass SourceStyleObject
instance GObjectClass SourceStyleObject where
  toGObject = GObject . castForeignPtr . unSourceStyleObject
  unsafeCastGObject = SourceStyleObject . castForeignPtr . unGObject

castToSourceStyleObject :: GObjectClass obj => obj -> SourceStyleObject
castToSourceStyleObject = castTo gTypeSourceStyleObject "SourceStyleObject"

gTypeSourceStyleObject :: GType
gTypeSourceStyleObject =
  {# call fun unsafe gtk_source_style_get_type #}

-- ********************************************************** SourceStyleScheme

{#pointer *SourceStyleScheme foreign newtype #} deriving (Eq,Ord)

mkSourceStyleScheme = (SourceStyleScheme, objectUnrefFromMainloop)
unSourceStyleScheme (SourceStyleScheme o) = o

class GObjectClass o => SourceStyleSchemeClass o
toSourceStyleScheme :: SourceStyleSchemeClass o => o -> SourceStyleScheme
toSourceStyleScheme = unsafeCastGObject . toGObject

instance SourceStyleSchemeClass SourceStyleScheme
instance GObjectClass SourceStyleScheme where
  toGObject = GObject . castForeignPtr . unSourceStyleScheme
  unsafeCastGObject = SourceStyleScheme . castForeignPtr . unGObject

castToSourceStyleScheme :: GObjectClass obj => obj -> SourceStyleScheme
castToSourceStyleScheme = castTo gTypeSourceStyleScheme "SourceStyleScheme"

gTypeSourceStyleScheme :: GType
gTypeSourceStyleScheme =
  {# call fun unsafe gtk_source_style_scheme_get_type #}

-- *************************************************** SourceStyleSchemeManager

{#pointer *SourceStyleSchemeManager foreign newtype #} deriving (Eq,Ord)

mkSourceStyleSchemeManager = (SourceStyleSchemeManager, objectUnrefFromMainloop)
unSourceStyleSchemeManager (SourceStyleSchemeManager o) = o

class GObjectClass o => SourceStyleSchemeManagerClass o
toSourceStyleSchemeManager :: SourceStyleSchemeManagerClass o => o -> SourceStyleSchemeManager
toSourceStyleSchemeManager = unsafeCastGObject . toGObject

instance SourceStyleSchemeManagerClass SourceStyleSchemeManager
instance GObjectClass SourceStyleSchemeManager where
  toGObject = GObject . castForeignPtr . unSourceStyleSchemeManager
  unsafeCastGObject = SourceStyleSchemeManager . castForeignPtr . unGObject

castToSourceStyleSchemeManager :: GObjectClass obj => obj -> SourceStyleSchemeManager
castToSourceStyleSchemeManager = castTo gTypeSourceStyleSchemeManager "SourceStyleSchemeManager"

gTypeSourceStyleSchemeManager :: GType
gTypeSourceStyleSchemeManager =
  {# call fun unsafe gtk_source_style_scheme_manager_get_type #}

