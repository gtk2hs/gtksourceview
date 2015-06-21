{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Enums
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
module Graphics.UI.Gtk.SourceView.Enums (
-- * Enums
   SourceCompletionActivation (..),
#if GTK_MAJOR_VERSION < 3
   SourceSearchFlags (..),
#endif
   SourceSmartHomeEndType (..),
   SourceDrawSpacesFlags (..),
   SourceViewGutterPosition (..)
) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Flags

{# context lib="gtk" prefix="gtk" #}

{# enum SourceCompletionActivation {underscoreToCase} deriving(Eq,Bounded,Show,Read) #}

#if GTK_MAJOR_VERSION < 3
{# enum SourceSearchFlags {underscoreToCase} deriving(Eq,Bounded,Show,Read) #}

instance Flags SourceSearchFlags
#endif

{# enum SourceSmartHomeEndType {underscoreToCase} deriving (Eq, Bounded, Show, Read) #}

{# enum SourceDrawSpacesFlags {underscoreToCase} deriving (Eq, Bounded, Show, Read) #}

instance Flags SourceDrawSpacesFlags

{# enum SourceViewGutterPosition {underscoreToCase} deriving (Eq, Bounded, Show, Read) #}

