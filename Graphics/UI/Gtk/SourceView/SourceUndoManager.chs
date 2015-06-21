{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceUndoManager
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
module Graphics.UI.Gtk.SourceView.SourceUndoManager (
-- * Description
-- | The 'SourceUndoManager' interface can be implemented to provide custom undo management to a
-- 'SourceBuffer'. Use 'sourceBufferSetUndoManager' to install a custom undo manager for a
-- particular source buffer.
-- 
-- Use 'sourceUndoManagerCanUndoChanged' and 'sourceUndoManagerCanRedoChanged' when
-- respectively the undo state or redo state of the undo stack has changed.

-- * Types  
    SourceUndoManager,
    SourceUndoManagerClass,
  
-- * Methods  
    sourceUndoManagerCanUndo,
    sourceUndoManagerCanRedo,
    sourceUndoManagerUndo,
    sourceUndoManagerRedo,
    sourceUndoManagerBeginNotUndoableAction,
    sourceUndoManagerEndNotUndoableAction,

-- * Signals
    sourceUndoManagerCanRedoChanged,
    sourceUndoManagerCanUndoChanged,
) where

import Control.Monad    (liftM)

import System.Glib.Attributes
import System.Glib.FFI
import System.Glib.Properties
import System.Glib.UTFString

{#import Graphics.UI.Gtk.SourceView.Signals#}
{#import Graphics.UI.Gtk.SourceView.Types#}

{# context lib="gtk" prefix="gtk" #}

-- | Get whether there are undo operations available.
sourceUndoManagerCanUndo :: SourceUndoManagerClass sum => sum
                         -> IO Bool  -- ^ returns 'True' if there are undo operations available, 'False' otherwise 
sourceUndoManagerCanUndo sm =
  liftM toBool $
  {#call gtk_source_undo_manager_can_undo #} (toSourceUndoManager sm)

-- | Get whether there are redo operations available.
sourceUndoManagerCanRedo :: SourceUndoManagerClass sum => sum
                         -> IO Bool  -- ^ returns 'True' if there are redo operations available, 'False' otherwise 
sourceUndoManagerCanRedo sm =
  liftM toBool $
  {#call gtk_source_undo_manager_can_redo #} (toSourceUndoManager sm)

-- | Perform a single undo. Calling this function when there are no undo operations available is an
-- error. Use @gtkSourceUndoManagerCanUndo@ to find out if there are undo operations available.
sourceUndoManagerUndo :: SourceUndoManagerClass sum => sum -> IO ()
sourceUndoManagerUndo sm =
  {#call gtk_source_undo_manager_undo #} (toSourceUndoManager sm)

-- | Perform a single redo. Calling this function when there are no redo operations available is an
-- error. Use @gtkSourceUndoManagerCanRedo@ to find out if there are redo operations available.
sourceUndoManagerRedo :: SourceUndoManagerClass sum => sum -> IO ()
sourceUndoManagerRedo sm =
  {#call gtk_source_undo_manager_redo #} (toSourceUndoManager sm)

-- | Begin a not undoable action on the buffer. All changes between this call and the call to
-- @gtkSourceUndoManagerEndNotUndoableAction@ cannot be undone. This function should be
-- re-entrant.
sourceUndoManagerBeginNotUndoableAction :: SourceUndoManagerClass sum => sum -> IO ()
sourceUndoManagerBeginNotUndoableAction sm =
  {#call gtk_source_undo_manager_begin_not_undoable_action #} (toSourceUndoManager sm)

-- | Ends a not undoable action on the buffer.
sourceUndoManagerEndNotUndoableAction :: SourceUndoManagerClass sum => sum -> IO ()
sourceUndoManagerEndNotUndoableAction sm =
  {#call gtk_source_undo_manager_end_not_undoable_action #} (toSourceUndoManager sm)

-- | Emitted when the ability to redo has changed.
--
sourceUndoManagerCanRedoChanged :: SourceUndoManagerClass sum => Signal sum (IO ())
sourceUndoManagerCanRedoChanged = Signal $ connect_NONE__NONE "can-redo-changed"

-- | Emitted when the ability to undo has changed.
--
sourceUndoManagerCanUndoChanged :: SourceUndoManagerClass sum => Signal sum (IO ())
sourceUndoManagerCanUndoChanged = Signal $ connect_NONE__NONE "can-undo-changed"
