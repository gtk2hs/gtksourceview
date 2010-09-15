{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SourceGutter
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
module Graphics.UI.Gtk.SourceView.SourceGutter (
-- * Description
-- | The 'SourceGutter' object represents the left and right gutters of the text view. It is used by
-- 'SourceView' to draw the line numbers and category marks that might be present on a line. By
-- packing additional 'CellRenderer' objects in the gutter, you can extend the gutter with your own
-- custom drawings.
-- 
-- The gutter works very much the same way as cells rendered in a 'TreeView'. The concept is similar,
-- with the exception that the gutter does not have an underlying 'TreeModel'. Instead, you should use
-- 'sourceGutterSetCellDataFunc' to set a callback to fill in any of the cell renderers
-- properties, given the line for which the cell is to be rendered. Renderers are inserted into the
-- gutter at a certain position. 
-- The builtin line number renderer is at position
-- 'SourceViewGutterPositionLines (-30)' and the marks renderer is at
-- 'SourceViewGutterPositionMarks (-20)'. You can use these values to position custom renderers
-- accordingly. The width of a cell renderer can be specified as either fixed (using
-- 'cellRendererSetFixedSize') or dynamic, in which case you must set
-- 'sourceGutterSetCellSizeFunc'. This callback is used to set the properties of the renderer
-- such that @gtkCellRendererGetSize@ yields the maximum width of the cell.

-- * Types
    SourceGutter,
    SourceGutterClass,

-- * Methods
    sourceGutterGetWindow,
    sourceGutterInsert,
    sourceGutterReorder,
    sourceGutterRemove,
    sourceGutterQueueDraw,
    sourceGutterSetCellDataFunc,
    sourceGutterSetCellSizeFunc,

-- * Attributes
    sourceGutterView,
    sourceGutterWindowType,

-- * Signals
    sourceGutterCellActivated,
    sourceGutterQueryTooltip,
) where

import Control.Monad	(liftM)
import Control.Monad.Reader ( runReaderT )

import Graphics.UI.Gtk.Gdk.EventM (EventM, EAny)
import Graphics.UI.Gtk.Multiline.TextView (TextWindowType (..))
import Graphics.UI.GtkInternals  ( TextIter, mkTextIterCopy )
import System.Glib.Attributes
import System.Glib.FFI
import System.Glib.GObject	(makeNewGObject)
import System.Glib.Properties
import System.Glib.UTFString

{#import Graphics.UI.Gtk.SourceView.Signals#}
{#import Graphics.UI.Gtk.SourceView.Types#}

{# context lib="gtk" prefix="gtk" #}

{#pointer SourceGutterDataFunc#}

foreign import ccall "wrapper" mkSourceGutterDataFunc ::
  (Ptr SourceGutter -> Ptr CellRenderer -> {#type gint#} -> {#type gboolean#} -> Ptr () -> IO ())
  -> IO SourceGutterDataFunc

{#pointer SourceGutterSizeFunc#}

foreign import ccall "wrapper" mkSourceGutterSizeFunc ::
  (Ptr SourceGutter -> Ptr CellRenderer -> Ptr () -> IO ())
  -> IO SourceGutterSizeFunc

-- | Get the 'Window' of the gutter. The window will only be available when the gutter has at least one,
-- non-zero width, cell renderer packed.
sourceGutterGetWindow :: SourceGutterClass sg => sg -> IO (Maybe DrawWindow)
sourceGutterGetWindow sb =
    maybeNull (makeNewGObject mkDrawWindow) $
    {#call gtk_source_gutter_get_window #} (toSourceGutter sb)

-- | Inserts renderer into gutter at position.
sourceGutterInsert :: (CellRendererClass cell, SourceGutterClass sg) => sg
                   -> cell -- ^ @renderer@ a 'CellRenderer'      
                   -> Int -- ^ @position@ the renderers position 
                   -> IO ()
sourceGutterInsert gutter renderer position =
  {#call gtk_source_gutter_insert #}
     (toSourceGutter gutter)
     (toCellRenderer renderer)
     (fromIntegral position)

-- | Reorders renderer in gutter to new position.
sourceGutterReorder :: (CellRendererClass cell, SourceGutterClass sg) => sg
                    -> cell -- ^ @renderer@ a 'CellRenderer'         
                    -> Int -- ^ @position@ the new renderer position 
                    -> IO ()
sourceGutterReorder gutter renderer position =
  {#call gtk_source_gutter_reorder #}
     (toSourceGutter gutter)
     (toCellRenderer renderer)
     (fromIntegral position)

-- | Removes renderer from gutter.
sourceGutterRemove :: (CellRendererClass cell, SourceGutterClass sg) => sg
                   -> cell -- ^ @renderer@ a 'CellRenderer' 
                   -> IO ()
sourceGutterRemove gutter renderer =
  {#call gtk_source_gutter_remove #}
     (toSourceGutter gutter)
     (toCellRenderer renderer)

-- | Invalidates the drawable area of the gutter. You can use this to force a redraw of the gutter if
-- something has changed and needs to be redrawn.
sourceGutterQueueDraw :: SourceGutterClass sg => sg -> IO ()
sourceGutterQueueDraw sb =
  {#call gtk_source_gutter_queue_draw #} (toSourceGutter sb)

-- | Sets the 'SourceGutterDataFunc' to use for renderer. This function is used to setup the cell
-- renderer properties for rendering the current cell.
sourceGutterSetCellDataFunc :: (SourceGutterClass sg, CellRendererClass cell)
                              => sg
                              -> cell
                              -> (CellRenderer -> Int -> Bool -> IO ())
                              -> IO ()
sourceGutterSetCellDataFunc gutter cell func = do
  funcPtr <- mkSourceGutterDataFunc $ \_ c line currentLine _ -> do
    func (toCellRenderer cell)
         (fromIntegral line)
         (toBool currentLine)
  {#call gtk_source_gutter_set_cell_data_func #}
     (toSourceGutter gutter)
     (toCellRenderer cell)
     funcPtr
     (castFunPtrToPtr funcPtr)
     destroyFunPtr

-- | Sets the 'SourceGutterSizeFunc' to use for renderer. This function is used to setup the cell
-- renderer properties for measuring the maximum size of the cell.
sourceGutterSetCellSizeFunc :: (SourceGutterClass gutter, CellRendererClass cell)
                              => gutter
                              -> cell
                              -> (CellRenderer -> IO ())
                              -> IO ()
sourceGutterSetCellSizeFunc gutter cell func = do
  funcPtr <- mkSourceGutterSizeFunc $ \ _ c _ -> do
    func (toCellRenderer cell)
  {#call gtk_source_gutter_set_cell_size_func #}
     (toSourceGutter gutter)
     (toCellRenderer cell)
     funcPtr
     (castFunPtrToPtr funcPtr)
     destroyFunPtr

-- | The 'SourceView' of the gutter
sourceGutterView :: SourceGutterClass sg => Attr sg SourceView
sourceGutterView = newAttrFromObjectProperty "view"
                   {#call pure unsafe gtk_source_view_get_type #}

-- | The text window type on which the window is placed
-- 
-- Default value: 'TextWindowPrivate'
sourceGutterWindowType :: SourceGutterClass sg => Attr sg TextWindowType
sourceGutterWindowType = newAttrFromEnumProperty "window-type"
                         {#call pure unsafe gtk_text_window_type_get_type #}
                         
-- | Emitted when a cell has been activated (for instance when there was a button press on the cell). The
-- signal is only emitted for cells that have the activatable property set to 'True'.
sourceGutterCellActivated :: SourceGutterClass sg => Signal sg (CellRenderer -> TextIter -> EventM EAny ())                         
sourceGutterCellActivated =
  Signal (\after obj fun -> 
           connect_OBJECT_PTR_BOXED__NONE "cell-activated" mkTextIterCopy after obj
                                   (\cr eventPtr iter -> runReaderT (fun cr iter) eventPtr)
         )

-- | Emitted when a tooltip is requested for a specific cell. Signal handlers can return 'True' to notify
-- the tooltip has been handled.
sourceGutterQueryTooltip :: SourceGutterClass sg => Signal sg (CellRenderer -> TextIter -> Tooltip -> IO Bool)
sourceGutterQueryTooltip = 
    Signal $ connect_OBJECT_BOXED_OBJECT__BOOL "query-tooltip" mkTextIterCopy

