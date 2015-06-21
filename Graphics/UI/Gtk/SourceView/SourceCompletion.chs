{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SourceCompletion
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
module Graphics.UI.Gtk.SourceView.SourceCompletion (
-- * Types
    SourceCompletion,
    SourceCompletionClass,

-- * Methods
    sourceCompletionAddProvider,
    sourceCompletionRemoveProvider,
    sourceCompletionGetProviders,
    sourceCompletionShow,
    sourceCompletionHide,
    sourceCompletionGetInfoWindow,
    sourceCompletionCreateContext,
    sourceCompletionMoveWindow,
    sourceCompletionBlockInteractive,
    sourceCompletionUnblockInteractive,

-- * Attributes
    sourceCompletionAccelerators,
    sourceCompletionAutoCompleteDelay,
    sourceCompletionProposalPageSize,
    sourceCompletionProviderPageSize,
    sourceCompletionRememberInfoVisibility,
    sourceCompletionSelectOnShow,
    sourceCompletionShowHeaders,
    sourceCompletionShowIcons,
    sourceCompletionView,

-- * Signals
    sourceCompletionActivateProposal,
    sourceCompletionHideSignal,
    sourceCompletionMoveCursor,
    sourceCompletionMovePage,
    sourceCompletionPopulateContext,
    sourceCompletionShowSignal,
) where

import Control.Monad    (liftM)
import Data.Maybe (fromMaybe)

import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
import Graphics.UI.Gtk.General.Enums (ScrollStep (..))
import Graphics.UI.Gtk.Multiline.TextView (TextWindowType (..))
import System.Glib.Attributes
import System.Glib.FFI
import System.Glib.GError
import System.Glib.GList                (fromGList, withGList)
import System.Glib.GObject      (makeNewGObject)
import System.Glib.Properties
import System.Glib.UTFString

{#import Graphics.UI.Gtk.SourceView.Signals#}
{#import Graphics.UI.Gtk.SourceView.Types#}

{# context lib="gtk" prefix="gtk" #}

-- | Add a new 'SourceCompletionProvider' to the completion object. This will add a reference provider,
-- so make sure to unref your own copy when you no longer need it.
sourceCompletionAddProvider :: SourceCompletionClass sc => sc 
                            -> SourceCompletionProvider
                            -> IO Bool -- ^ returns    'True' if provider was successfully added, otherwise if error is provided, it will be set with the error and     
sourceCompletionAddProvider sc provider = 
  liftM toBool $
  propagateGError $ \gErrorPtr -> 
      {# call gtk_source_completion_add_provider #}  
          (toSourceCompletion sc)
          provider
          gErrorPtr

-- | Remove provider from the completion.
sourceCompletionRemoveProvider :: SourceCompletionClass sc => sc 
                               -> SourceCompletionProvider
                               -> IO Bool  -- ^ returns    'True' if provider was successfully removed, otherwise if error is provided, it will be set with the error and   
sourceCompletionRemoveProvider sc provider =
  liftM toBool $
  propagateGError $ \gErrorPtr -> 
      {#call gtk_source_completion_remove_provider #}
         (toSourceCompletion sc)
         provider
         gErrorPtr
      
-- | Get list of providers registered on completion. The returned list is owned by the completion and
-- should not be freed.
sourceCompletionGetProviders :: SourceCompletionClass sc => sc -> IO [SourceCompletionProvider]
sourceCompletionGetProviders sc = do
  glist <- {#call gtk_source_completion_get_providers #} (toSourceCompletion sc)
  glistPtrs <- fromGList glist
  mapM (makeNewGObject mkSourceCompletionProvider . return) glistPtrs  

-- | Starts a new completion with the specified 'SourceCompletionContext' and a list of potential
-- candidate providers for completion.
sourceCompletionShow :: SourceCompletionClass sc => sc
                     -> [SourceCompletionProvider] -- ^ @providers@  A list of 'SourceCompletionProvider' 
                     -> SourceCompletionContext -- ^ @context@    The 'SourceCompletionContext' with which to start the completion
                     -> IO Bool -- ^ returns    'True' if it was possible to the show completion window.           
sourceCompletionShow sc providers context =
  liftM toBool $
  withForeignPtrs (map unSourceCompletionProvider providers) $ \providersPtr ->
  withGList providersPtr $ \glist ->
  {#call gtk_source_completion_show #}
    (toSourceCompletion sc)
    glist
    context

-- | Hides the completion if it is active (visible).
sourceCompletionHide :: SourceCompletionClass sc => sc -> IO ()
sourceCompletionHide sc =
  {#call gtk_source_completion_hide #} (toSourceCompletion sc)

-- | The info widget is the window where the completion displays optional extra information of the
-- proposal.
sourceCompletionGetInfoWindow :: SourceCompletionClass sc => sc -> IO SourceCompletionInfo
sourceCompletionGetInfoWindow sc =
  makeNewObject mkSourceCompletionInfo $
  {#call gtk_source_completion_get_info_window #}
    (toSourceCompletion sc)

-- | Create a new 'SourceCompletionContext' for completion. The position at which the completion using
-- the new context will consider completion can be provider by position. If position is 'Nothing', the
-- current cursor position will be used.
sourceCompletionCreateContext :: SourceCompletionClass sc => sc
                              -> Maybe TextIter
                              -> IO SourceCompletionContext
sourceCompletionCreateContext sc iter = 
  makeNewGObject mkSourceCompletionContext $
  {#call gtk_source_completion_create_context #}
    (toSourceCompletion sc)
    (fromMaybe (TextIter nullForeignPtr) iter)

-- | Move the completion window to a specific iter.
sourceCompletionMoveWindow :: SourceCompletionClass sc => sc
                           -> TextIter 
                           -> IO ()
sourceCompletionMoveWindow sc iter = 
  {#call gtk_source_completion_move_window #}
    (toSourceCompletion sc)
    iter

-- | Block interactive completion. This can be used to disable interactive completion when inserting or
-- deleting text from the buffer associated with the completion. Use
-- 'sourceCompletionUnblockInteractive' to enable interactive completion again.
sourceCompletionBlockInteractive :: SourceCompletionClass sc => sc -> IO ()
sourceCompletionBlockInteractive sc =
  {#call gtk_source_completion_block_interactive #}
     (toSourceCompletion sc)

-- | Unblock interactive completion. This can be used after using 'sourceCompletionBlockInteractive'
-- to enable interactive completion again.
sourceCompletionUnblockInteractive :: SourceCompletionClass sc => sc -> IO ()
sourceCompletionUnblockInteractive sc =
  {#call gtk_source_completion_unblock_interactive #}
     (toSourceCompletion sc)

-- | Number of accelerators to show for the first proposals.
-- 
-- Allowed values: <= 10
-- 
-- Default value: 5
--
sourceCompletionAccelerators :: SourceCompletionClass sc => Attr sc Int
sourceCompletionAccelerators = newAttrFromIntProperty "accelerators"

-- | Determines the popup delay (in milliseconds) at which the completion will be shown for interactive
-- completion.
-- 
-- Default value: 250
--
sourceCompletionAutoCompleteDelay :: SourceCompletionClass sc => Attr sc Int
sourceCompletionAutoCompleteDelay = newAttrFromIntProperty "auto-complete-delay"

-- | The scroll page size of the proposals in the completion window.
-- 
-- Allowed values: >= 1
-- 
-- Default value: 5
--
sourceCompletionProposalPageSize :: SourceCompletionClass sc => Attr sc Int
sourceCompletionProposalPageSize = newAttrFromIntProperty "proposal-page-size"

-- | The scroll page size of the provider pages in the completion window.
-- 
-- Allowed values: >= 1
-- 
-- Default value: 5
--
sourceCompletionProviderPageSize :: SourceCompletionClass sc => Attr sc Int
sourceCompletionProviderPageSize = newAttrFromIntProperty "provider-page-size"

-- | Determines whether the visibility of the info window should be saved when the completion is hidden,
-- and restored when the completion is shown again.
-- 
-- Default value: 'False'
--
sourceCompletionRememberInfoVisibility :: SourceCompletionClass sc => Attr sc Bool
sourceCompletionRememberInfoVisibility = newAttrFromBoolProperty "remember-info-visibility"

-- | Determines whether the first proposal should be selected when the completion is first shown.
-- 
-- Default value: 'True'
--
sourceCompletionSelectOnShow :: SourceCompletionClass sc => Attr sc Bool
sourceCompletionSelectOnShow = newAttrFromBoolProperty "select-on-show"

-- | Determines whether provider headers should be shown in the proposal list if there is more than one
-- provider with proposals.
-- 
-- Default value: 'True'
sourceCompletionShowHeaders :: SourceCompletionClass sc => Attr sc Bool
sourceCompletionShowHeaders = newAttrFromBoolProperty "show-headers"

-- | Determines whether provider and proposal icons should be shown in the completion popup.
-- 
-- Default value: 'True'
--
sourceCompletionShowIcons :: SourceCompletionClass sc => Attr sc Bool
sourceCompletionShowIcons = newAttrFromBoolProperty "show-icons"

-- | The 'SourceView' bound to the completion object.
--
sourceCompletionView :: SourceCompletionClass sc => Attr sc SourceView
sourceCompletionView = newAttrFromObjectProperty "view"
                       {#call pure unsafe gtk_source_view_get_type #}

-- | The 'activateProposal' signal is a keybinding signal which gets emitted when the user initiates a
-- proposal activation.
-- 
-- Applications should not connect to it, but may emit it with @gSignalEmitByName@ if they need to
-- control the proposal activation programmatically.
sourceCompletionActivateProposal :: SourceCompletionClass sc => Signal sc (IO ())
sourceCompletionActivateProposal = 
  Signal $ connect_NONE__NONE "activate-proposal"

-- | Emitted when the completion window is hidden. The default handler will actually hide the window.
sourceCompletionHideSignal :: SourceCompletionClass sc => Signal sc (IO ())
sourceCompletionHideSignal =
  Signal $ connect_NONE__NONE "hide"

-- | The 'moveCursor' signal is a keybinding signal which gets emitted when the user initiates a cursor
-- movement.
-- 
-- Applications should not connect to it, but may emit it with @gSignalEmitByName@ if they need to
-- control the cursor programmatically.
sourceCompletionMoveCursor :: SourceCompletionClass sc => Signal sc (ScrollStep -> Int -> IO ())
sourceCompletionMoveCursor =
  Signal $ connect_ENUM_INT__NONE "move-cursor"

-- | The 'movePage' signal is a keybinding signal which gets emitted when the user initiates a page
-- movement (i.e. switches between provider pages).
-- 
-- Applications should not connect to it, but may emit it with @gSignalEmitByName@ if they need to
-- control the page selection programmatically.
sourceCompletionMovePage :: SourceCompletionClass sc => Signal sc (ScrollStep -> Int -> IO ())
sourceCompletionMovePage =
  Signal $ connect_ENUM_INT__NONE "move-page"

-- | Emitted just before starting to populate the completion with providers. You can use this signal to
-- add additional attributes in the context.
sourceCompletionPopulateContext :: SourceCompletionClass sc => Signal sc (SourceCompletionContext -> IO ())
sourceCompletionPopulateContext = Signal $ connect_OBJECT__NONE "populate-context"

-- | Emitted when the completion window is shown. The default handler will actually show the window.
sourceCompletionShowSignal :: SourceCompletionClass sc => Signal sc (IO ())
sourceCompletionShowSignal =
  Signal $ connect_NONE__NONE "show"
