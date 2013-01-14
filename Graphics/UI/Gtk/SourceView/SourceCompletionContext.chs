{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SourceCompletionContext
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
module Graphics.UI.Gtk.SourceView.SourceCompletionContext (
-- * Types
   SourceCompletionContext,
   SourceCompletionContextClass,

-- * Enums
   SourceCompletionActivation,

-- * Methods
   sourceCompletionContextAddProposals,
   sourceCompletionContextGetIter,

-- * Attributes   
   sourceCompletionContextActivation,
   sourceCompletionContextCompletion,

-- * Signals   
   sourceCompletionContextCancelled,
) where

import Control.Monad	(liftM)

import Graphics.UI.Gtk.SourceView.Enums
import System.Glib.Attributes
import System.Glib.FFI
import System.Glib.GList		(fromGList, withGList)
import System.Glib.Properties
import System.Glib.UTFString

{#import Graphics.UI.Gtk.SourceView.Signals#}
{#import Graphics.UI.Gtk.SourceView.Types#}

{# context lib="gtk" prefix="gtk" #}

-- | Providers can use this function to add proposals to the completion. They can do so asynchronously by
-- means of the finished argument. Providers must ensure that they always call this function with
-- finished set to 'True' once each population (even if no proposals need to be added).
sourceCompletionContextAddProposals :: (SourceCompletionContextClass scc, SourceCompletionProviderClass scp) => scc
                                    -> scp
                                    -> [SourceCompletionProposal] -- ^ @proposals@ The list of proposals to add                      
                                    -> Bool -- ^ @finished@  Whether the provider is finished adding proposals 
                                    -> IO ()
sourceCompletionContextAddProposals scc provider proposals finished = 
    withForeignPtrs (map unSourceCompletionProposal proposals) $ \proposalsPtr ->
#ifdef MIN_VERSION_gtksourceview_3_0
#if MIN_VERSION_gtksourceview_3_0(3,6,0)
#else
    withForeignPtr (unSourceCompletionProvider $ toSourceCompletionProvider provider) $ \providerPtr ->
#endif
#else
    withForeignPtr (unSourceCompletionProvider $ toSourceCompletionProvider provider) $ \providerPtr ->
#endif
    withGList proposalsPtr $ \glist ->
    {#call gtk_source_completion_context_add_proposals #}
      (toSourceCompletionContext scc)
#ifdef MIN_VERSION_gtksourceview_3_0
#if MIN_VERSION_gtksourceview_3_0(3,6,0)
      (toSourceCompletionProvider provider)
#else
      (castPtr providerPtr)
#endif
#else
      (castPtr providerPtr)
#endif
      glist
      (fromBool finished)

-- | Get the iter at which the completion was invoked. Providers can use this to determine how and if to
-- match proposals.
sourceCompletionContextGetIter :: SourceCompletionContextClass scc => scc
                               -> IO TextIter
sourceCompletionContextGetIter scc = do
  iter <- makeEmptyTextIter
  {#call gtk_source_completion_context_get_iter #}
    (toSourceCompletionContext scc)
    iter
  return iter

-- | The completion activation
sourceCompletionContextActivation :: SourceCompletionContextClass scc => Attr scc SourceCompletionActivation
sourceCompletionContextActivation = newAttrFromEnumProperty "activation"
                                    {#call pure unsafe gtk_source_completion_activation_get_type #}

-- | The 'SourceCompletion' associated with the context.
sourceCompletionContextCompletion :: SourceCompletionContextClass scc => Attr scc SourceCompletion
sourceCompletionContextCompletion = newAttrFromObjectProperty "completion"
                                    {#call pure unsafe gtk_source_completion_get_type #}

-- | Emitted when the current population of proposals has been cancelled. Providers adding proposals
-- asynchronously should connect to this signal to know when to cancel running proposal queries.
sourceCompletionContextCancelled :: SourceCompletionContextClass scc => Signal scc (IO ())
sourceCompletionContextCancelled = Signal $ connect_NONE__NONE "cancelled"
