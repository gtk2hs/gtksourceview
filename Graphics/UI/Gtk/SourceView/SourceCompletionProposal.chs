{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SourceCompletionProposal
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
module Graphics.UI.Gtk.SourceView.SourceCompletionProposal (
-- * Description
-- | The proposal interface represents a completion item in the completion window. It provides
-- information on how to display the completion item and what action should be taken when the
-- completion item is activated.

-- * Types
   SourceCompletionProposal,
   SourceCompletionProposalClass,

-- * Methods
   sourceCompletionProposalGetLabel,
   sourceCompletionProposalGetMarkup,
   sourceCompletionProposalGetText,
   sourceCompletionProposalGetIcon,
   sourceCompletionProposalGetInfo,
   sourceCompletionProposalHash,
   sourceCompletionProposalEqual,

-- * Signals
   sourceCompletionProposalChanged,
) where

import Control.Monad    (liftM)

import System.Glib.Attributes
import System.Glib.FFI
import System.Glib.GObject      (wrapNewGObject)
import System.Glib.Properties
import System.Glib.UTFString

{#import Graphics.UI.Gtk.SourceView.Signals#}
{#import Graphics.UI.Gtk.SourceView.Types#}

{# context lib="gtk" prefix="gtk" #}

-- | Gets the label of proposal. The label is shown in the list of proposals as plain text. If you need
-- any markup (such as bold or italic text), you have to implement
-- 'sourceCompletionProposalGetMarkup'.
sourceCompletionProposalGetLabel :: (SourceCompletionProposalClass scp, GlibString string) => scp
                                 -> IO string -- ^ returns  A new string containing the label of proposal.
sourceCompletionProposalGetLabel scp =
  {#call gtk_source_completion_proposal_get_label #}
    (toSourceCompletionProposal scp)
  >>= peekUTFString

-- | Gets the label of proposal with markup. The label is shown in the list of proposals and may contain
-- markup. This will be used instead of 'sourceCompletionProposalGetLabel' if implemented.
sourceCompletionProposalGetMarkup :: (SourceCompletionProposalClass scp, GlibString string) => scp
                                  -> IO string -- ^ returns  A new string containing the label of proposal with markup.
sourceCompletionProposalGetMarkup scp =
  {#call gtk_source_completion_proposal_get_markup #}
    (toSourceCompletionProposal scp)
  >>= peekUTFString

-- | Gets the text of proposal. The text that is inserted into the text buffer when the proposal is
-- activated by the default activation. You are free to implement a custom activation handler in the
-- provider and not implement this function.
sourceCompletionProposalGetText :: (SourceCompletionProposalClass scp, GlibString string) => scp
                                -> IO string -- ^ returns  A new string containing the text of proposal.
sourceCompletionProposalGetText scp =
  {#call gtk_source_completion_proposal_get_text #}
    (toSourceCompletionProposal scp)
  >>= peekUTFString

-- | Gets the icon of proposal.
sourceCompletionProposalGetIcon :: SourceCompletionProposalClass scp => scp
                                -> IO Pixbuf -- ^ returns  The icon of proposal.
sourceCompletionProposalGetIcon scp =
  wrapNewGObject mkPixbuf $
  {#call gtk_source_completion_proposal_get_icon #}
    (toSourceCompletionProposal scp)

-- | Gets extra information associated to the proposal. This information will be used to present the user
-- with extra, detailed information about the selected proposal.
sourceCompletionProposalGetInfo :: (SourceCompletionProposalClass scp, GlibString string) => scp
                                -> IO string -- ^ returns  A new string containing extra information of proposal or empty if no extra information is associated to proposal.
sourceCompletionProposalGetInfo scp =
  {#call gtk_source_completion_proposal_get_info #}
    (toSourceCompletionProposal scp)
  >>= peekUTFString

-- | Get the hash value of proposal. This is used to (together with 'sourceCompletionProposalEqual')
-- to match proposals in the completion model.
sourceCompletionProposalHash :: SourceCompletionProposalClass scp => scp
                             -> IO Int  -- ^ returns  The hash value of proposal
sourceCompletionProposalHash scp =
  liftM fromIntegral $
  {#call gtk_source_completion_proposal_hash #}
    (toSourceCompletionProposal scp)

-- | Get whether two proposal objects are the same. This is used to (together with
-- 'sourceCompletionProposalHash') to match proposals in the completion model.
sourceCompletionProposalEqual :: (SourceCompletionProposalClass scp1, SourceCompletionProposalClass scp2)
                              => scp1
                              -> scp2
                              -> IO Bool  -- ^ returns  'True' if proposal and object are the same proposal
sourceCompletionProposalEqual scp1 scp2 =
    liftM toBool $
    {#call gtk_source_completion_proposal_equal #}
      (toSourceCompletionProposal scp1)
      (toSourceCompletionProposal scp2)

-- | Emitted when the proposal has changed. The completion popup will react to this by updating the shown
-- information.
sourceCompletionProposalChanged :: SourceCompletionProposalClass scp => Signal scp (IO ())
sourceCompletionProposalChanged = Signal $ connect_NONE__NONE "changed"

