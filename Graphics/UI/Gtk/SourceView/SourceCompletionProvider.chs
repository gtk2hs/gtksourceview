{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SourceCompletionProvider
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
module Graphics.UI.Gtk.SourceView.SourceCompletionProvider (
-- * Description
-- | You must implement this interface to provide proposals to 'SourceCompletion'

-- * Types
   SourceCompletionProvider,
   SourceCompletionProviderClass,

-- * Methods
   sourceCompletionProviderGetName,
   sourceCompletionProviderGetIcon,
   sourceCompletionProviderGetInteractiveDelay,
   sourceCompletionProviderGetPriority,
   sourceCompletionProviderGetInfoWidget,
   sourceCompletionProviderGetActivation,
   sourceCompletionProviderGetStartIter,
   sourceCompletionProviderMatch,
   sourceCompletionProviderUpdateInfo,
   sourceCompletionProviderPopulate,
   sourceCompletionProviderActivateProposal,
) where

import Control.Monad    (liftM)

import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
import Graphics.UI.Gtk.SourceView.Enums
import System.Glib.Attributes
import System.Glib.FFI
import System.Glib.GObject      (wrapNewGObject)
import System.Glib.Properties
import System.Glib.UTFString

{#import Graphics.UI.Gtk.SourceView.Signals#}
{#import Graphics.UI.Gtk.SourceView.Types#}

{# context lib="gtk" prefix="gtk" #}

-- | Get the name of the provider. This should be a translatable name for display to the user. For
-- example: _("Document word completion provider").
sourceCompletionProviderGetName :: (SourceCompletionProviderClass scp, GlibString string) => scp
                                -> IO string -- ^ returns  A new string containing the name of the provider.
sourceCompletionProviderGetName scp =
  {#call gtk_source_completion_provider_get_name #}
    (toSourceCompletionProvider scp)
  >>= peekUTFString

-- | Get the icon of the provider.
sourceCompletionProviderGetIcon :: SourceCompletionProviderClass scp => scp
                                -> IO (Maybe Pixbuf)
sourceCompletionProviderGetIcon scp =
  maybeNull (wrapNewGObject mkPixbuf) $
  {#call gtk_source_completion_provider_get_icon #}
    (toSourceCompletionProvider scp)

-- | Get the delay in milliseconds before starting interactive completion for this provider. A value of
-- -1 indicates to use the default value as set by 'autoCompleteDelay'.
sourceCompletionProviderGetInteractiveDelay :: SourceCompletionProviderClass scp => scp
                                            -> IO Int -- ^ returns  the interactive delay in milliseconds.
sourceCompletionProviderGetInteractiveDelay scp =
  liftM fromIntegral $
  {#call gtk_source_completion_provider_get_interactive_delay #}
     (toSourceCompletionProvider scp)

-- | Get the provider priority. The priority determines the order in which proposals appear in the
-- completion popup. Higher priorities are sorted before lower priorities. The default priority is 0.
sourceCompletionProviderGetPriority :: SourceCompletionProviderClass scp => scp
                                    -> IO Int -- ^ returns  the provider priority.
sourceCompletionProviderGetPriority scp =
  liftM fromIntegral $
  {#call gtk_source_completion_provider_get_priority #}
    (toSourceCompletionProvider scp)

-- | Get a customized info widget to show extra information of a proposal. This allows for customized
-- widgets on a proposal basis, although in general providers will have the same custom widget for all
-- their proposals and proposal can be ignored.  The implementation of this function is optional. If
-- implemented, 'sourceCompletionProviderUpdateInfo' MUST also be implemented. If not
-- implemented, the default 'sourceCompletionProposalGetInfo' will be used to display extra
-- information about a 'SourceCompletionProposal'.
sourceCompletionProviderGetInfoWidget :: SourceCompletionProviderClass scp => scp
                                      -> SourceCompletionProposal -- ^ @proposal@ The currently selected 'SourceCompletionProposal'
                                     -> IO Widget -- ^ returns  a custom 'Widget' to show extra information about proposal.
sourceCompletionProviderGetInfoWidget scp proposal =
  makeNewObject mkWidget $
  {#call gtk_source_completion_provider_get_info_widget #}
    (toSourceCompletionProvider scp)
    proposal

-- | Get with what kind of activation the provider should be activated.
sourceCompletionProviderGetActivation :: SourceCompletionProviderClass scp => scp
                                      -> IO SourceCompletionActivation
sourceCompletionProviderGetActivation scp =
    liftM (toEnum . fromIntegral) $
    {#call gtk_source_completion_provider_get_activation #}
      (toSourceCompletionProvider scp)

-- | Get the 'TextIter' at which the completion for proposal starts. When implemented, the completion
-- can use this information to position the completion window accordingly when a proposal is selected
-- in the completion window.
sourceCompletionProviderGetStartIter :: SourceCompletionProviderClass scp => scp
                                     -> SourceCompletionContext
                                     -> SourceCompletionProposal
                                     -> IO (Maybe TextIter)
sourceCompletionProviderGetStartIter scp context proposal = do
  iter <- makeEmptyTextIter
  success <- liftM toBool $ {#call gtk_source_completion_provider_get_start_iter #}
                (toSourceCompletionProvider scp)
                context
                proposal
                iter
  if success
     then return (Just iter)
     else return Nothing

-- | Get whether the provider match the context of completion detailed in context.
sourceCompletionProviderMatch :: SourceCompletionProviderClass scp => scp
                              -> SourceCompletionContext
                              -> IO Bool  -- ^ returns  'True' if provider matches the completion context, 'False' otherwise
sourceCompletionProviderMatch scp context =
  liftM toBool $
  {#call gtk_source_completion_provider_match #}
   (toSourceCompletionProvider scp)
   context

-- | Update extra information shown in info for proposal. This should be implemented if your provider
-- sets a custom info widget for proposal. This function MUST be implemented when
-- 'sourceCompletionProviderGetInfoWidget' is implemented.
sourceCompletionProviderUpdateInfo :: SourceCompletionProviderClass scp => scp
                                   -> SourceCompletionProposal
                                   -> SourceCompletionInfo
                                   -> IO ()
sourceCompletionProviderUpdateInfo scp proposal info =
  {#call gtk_source_completion_provider_update_info #}
    (toSourceCompletionProvider scp)
    proposal
    info

-- | Populate context with proposals from provider
sourceCompletionProviderPopulate :: SourceCompletionProviderClass scp => scp
                                 -> SourceCompletionContext
                                 -> IO ()
sourceCompletionProviderPopulate scp context =
  {#call gtk_source_completion_provider_populate #}
     (toSourceCompletionProvider scp)
     context

-- | Activate proposal at iter. When this functions returns 'False', the default activation of proposal
-- will take place which replaces the word at iter with the label of proposal.
sourceCompletionProviderActivateProposal :: SourceCompletionProviderClass scp => scp
                                         -> SourceCompletionProposal
                                         -> TextIter
                                         -> IO Bool -- ^ returns  'True' to indicate that the proposal activation has been handled, 'False' otherwise.
sourceCompletionProviderActivateProposal scp proposal iter =
  liftM toBool $
  {#call gtk_source_completion_provider_activate_proposal #}
    (toSourceCompletionProvider scp)
    proposal
    iter
