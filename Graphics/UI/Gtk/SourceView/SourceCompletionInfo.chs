{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SourceCompletionInfo
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
module Graphics.UI.Gtk.SourceView.SourceCompletionInfo (
-- * Description
-- | This object can be used to show a calltip or help for the .* current completion proposal.

-- * Types
   SourceCompletionInfo,
   SourceCompletionInfoClass,

-- * Methods
   sourceCompletionInfoNew,
   sourceCompletionInfoMoveToIter,
#if GTK_MAJOR_VERSION < 3
   sourceCompletionInfoSetSizing,
#endif
   sourceCompletionInfoSetWidget,
   sourceCompletionInfoGetWidget,
#if GTK_MAJOR_VERSION < 3
   sourceCompletionInfoProcessResize,
#endif

-- * Attributes
   sourceCompletionInfoMaxHeight,
   sourceCompletionInfoMaxWidth,
   sourceCompletionInfoShrinkHeight,
   sourceCompletionInfoShrinkWidth,

-- * Signals
   sourceCompletionInfoBeforeShow,
) where
import Control.Monad    (liftM)
import Data.Maybe (fromMaybe)

import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
import System.Glib.Attributes
import System.Glib.FFI
import System.Glib.Properties
import System.Glib.UTFString

{#import Graphics.UI.Gtk.SourceView.Signals#}
{#import Graphics.UI.Gtk.SourceView.Types#}

{# context lib="gtk" prefix="gtk" #}

-- |
sourceCompletionInfoNew :: IO SourceCompletionInfo
sourceCompletionInfoNew = 
  makeNewObject mkSourceCompletionInfo $
  {#call gtk_source_completion_info_new #}

-- | Moves the 'SourceCompletionInfo' to iter. If iter is 'Nothing' info is moved to the cursor
-- position. Moving will respect the 'Gravity' setting of the info window and will ensure the line at
-- iter is not occluded by the window.
sourceCompletionInfoMoveToIter :: SourceCompletionInfoClass info => info 
                               -> TextView -- ^ @view@ A 'TextView' on which the info window should be positioned 
                               -> Maybe TextIter -- ^ @iter@ A 'TextIter' or 'Nothing'
                               -> IO ()
sourceCompletionInfoMoveToIter info view iter =
  {#call gtk_source_completion_info_move_to_iter #}
    (toSourceCompletionInfo info)
    view
    (fromMaybe (TextIter nullForeignPtr) iter)

#if GTK_MAJOR_VERSION < 3
-- | Set sizing information for the info window. If @shrinkWidth@ or @shrinkHeight@ is 'True', the info
-- window will try to resize to fit the window contents, with a maximum size given by width and
-- height. Setting width or height to -1 removes the maximum size of respectively the width and height
-- of the window.
sourceCompletionInfoSetSizing :: SourceCompletionInfoClass info => info 
                              -> Int -- ^ @width@         The maximum/requested width of the window (-1 to default)     
                              -> Int -- ^ @height@        The maximum/requested height of the window (-1 to default)    
                              -> Bool -- ^ @shrinkWidth@  Whether to shrink the width of the window to fit its contents 
                              -> Bool -- ^ @shrinkHeight@ Whether to shrink the height of the window to fit its contents
                              -> IO ()
sourceCompletionInfoSetSizing info width height shrinkWidth shrinkHeight = 
  {#call gtk_source_completion_info_set_sizing #}
     (toSourceCompletionInfo info)
     (fromIntegral width)
     (fromIntegral height)
     (fromBool shrinkWidth)
     (fromBool shrinkHeight)
#endif

-- | Sets the content widget of the info window. If widget does not fit within the size requirements of
-- the window, a 'ScrolledWindow' will automatically be created and added to the window.
sourceCompletionInfoSetWidget :: (SourceCompletionInfoClass info, WidgetClass widget) => info 
                              -> widget
                              -> IO ()
sourceCompletionInfoSetWidget info widget =
  {#call gtk_source_completion_info_set_widget #}
    (toSourceCompletionInfo info)
    (toWidget widget)

-- | Get the current content widget.
sourceCompletionInfoGetWidget :: SourceCompletionInfoClass info => info 
                              -> IO Widget -- ^ returns The current content widget. 
sourceCompletionInfoGetWidget info =
  makeNewObject mkWidget $
  {#call gtk_source_completion_info_get_widget #}
     (toSourceCompletionInfo info)

#if GTK_MAJOR_VERSION < 3
-- |
sourceCompletionInfoProcessResize :: SourceCompletionInfoClass info => info 
                                  -> IO ()
sourceCompletionInfoProcessResize info =
  {#call gtk_source_completion_info_process_resize #}
     (toSourceCompletionInfo info)
#endif

-- | The maximum allowed height.
-- 
-- Allowed values: >= GMaxulong
-- 
-- Default value: -1
sourceCompletionInfoMaxHeight :: SourceCompletionInfoClass info => Attr info Int
sourceCompletionInfoMaxHeight = newAttrFromIntProperty "max-height"

-- | The maximum allowed width.
-- 
-- Allowed values: >= GMaxulong
-- 
-- Default value: -1
sourceCompletionInfoMaxWidth :: SourceCompletionInfoClass info => Attr info Int
sourceCompletionInfoMaxWidth = newAttrFromIntProperty "max-width"

-- | Whether the window should shrink height to fit the contents.
-- 
-- Default value: 'True'
sourceCompletionInfoShrinkHeight :: SourceCompletionInfoClass info => Attr info Bool
sourceCompletionInfoShrinkHeight = newAttrFromBoolProperty "shrink-height"

-- | Whether the window should shrink width to fit the contents.
-- 
-- Default value: 'True'
sourceCompletionInfoShrinkWidth :: SourceCompletionInfoClass info => Attr info Bool
sourceCompletionInfoShrinkWidth = newAttrFromBoolProperty "shrink-width"

-- |
sourceCompletionInfoBeforeShow :: SourceCompletionInfoClass info => Signal info (IO ())
sourceCompletionInfoBeforeShow = Signal $ connect_NONE__NONE "before-show"
