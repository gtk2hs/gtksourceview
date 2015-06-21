{-# LANGUAGE CPP #-}
module Graphics.UI.Gtk.SourceView (
        module Graphics.UI.Gtk.SourceView.SourceBuffer,
        module Graphics.UI.Gtk.SourceView.SourceCompletion,
        module Graphics.UI.Gtk.SourceView.SourceCompletionContext,
        module Graphics.UI.Gtk.SourceView.SourceCompletionInfo,
        module Graphics.UI.Gtk.SourceView.SourceCompletionItem,
        module Graphics.UI.Gtk.SourceView.SourceCompletionProposal,
        module Graphics.UI.Gtk.SourceView.SourceCompletionProvider,
        module Graphics.UI.Gtk.SourceView.SourceGutter,
#if GTK_MAJOR_VERSION < 3
        module Graphics.UI.Gtk.SourceView.SourceIter,
#endif
        module Graphics.UI.Gtk.SourceView.SourceLanguage,
        module Graphics.UI.Gtk.SourceView.SourceLanguageManager,
        module Graphics.UI.Gtk.SourceView.SourceMark,
        module Graphics.UI.Gtk.SourceView.SourceStyle,
        module Graphics.UI.Gtk.SourceView.SourceStyleScheme,
        module Graphics.UI.Gtk.SourceView.SourceStyleSchemeManager,
        module Graphics.UI.Gtk.SourceView.SourceUndoManager,
        module Graphics.UI.Gtk.SourceView.SourceView,
        ) where

import Graphics.UI.Gtk.SourceView.SourceBuffer
import Graphics.UI.Gtk.SourceView.SourceCompletion
import Graphics.UI.Gtk.SourceView.SourceCompletionContext
import Graphics.UI.Gtk.SourceView.SourceCompletionInfo
import Graphics.UI.Gtk.SourceView.SourceCompletionItem
import Graphics.UI.Gtk.SourceView.SourceCompletionProposal
import Graphics.UI.Gtk.SourceView.SourceCompletionProvider
import Graphics.UI.Gtk.SourceView.SourceGutter
#if GTK_MAJOR_VERSION < 3
import Graphics.UI.Gtk.SourceView.SourceIter
#endif
import Graphics.UI.Gtk.SourceView.SourceLanguage
import Graphics.UI.Gtk.SourceView.SourceLanguageManager
import Graphics.UI.Gtk.SourceView.SourceMark
import Graphics.UI.Gtk.SourceView.SourceStyle
import Graphics.UI.Gtk.SourceView.SourceStyleScheme
import Graphics.UI.Gtk.SourceView.SourceStyleSchemeManager
import Graphics.UI.Gtk.SourceView.SourceUndoManager
import Graphics.UI.Gtk.SourceView.SourceView
