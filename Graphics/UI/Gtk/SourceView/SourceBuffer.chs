{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceBuffer
--
--  Author : Peter Gavin, Andy Stewart
--  derived from sourceview bindings by Axel Simon and Duncan Coutts
--
--  Created: 18 December 2008
--
--  Copyright (C) 2003-2008 Peter Gavin, Duncan Coutts, Axel Simon
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
module Graphics.UI.Gtk.SourceView.SourceBuffer (
-- * Types
  SourceBuffer,
  SourceBufferClass,

-- * Methods
  castToSourceBuffer,
  sourceBufferNew,
  sourceBufferNewWithLanguage,
  sourceBufferSetHighlightSyntax,
  sourceBufferGetHighlightSyntax,
  sourceBufferSetLanguage,
  sourceBufferGetLanguage,
  sourceBufferSetHighlightMatchingBrackets,
  sourceBufferGetHighlightMatchingBrackets,
  sourceBufferSetStyleScheme,
  sourceBufferGetStyleScheme,
  sourceBufferSetMaxUndoLevels,
  sourceBufferGetMaxUndoLevels,
  sourceBufferGetCanUndo,
  sourceBufferGetCanRedo,
  sourceBufferUndo,
  sourceBufferRedo,
  sourceBufferBeginNotUndoableAction,
  sourceBufferEndNotUndoableAction,
  sourceBufferCreateSourceMark,
  sourceBufferGetSourceMarksAtLine,
  sourceBufferGetSourceMarksAtIter,
  sourceBufferRemoveSourceMarks,
  sourceBufferForwardIterToSourceMark,
  sourceBufferBackwardIterToSourceMark,
  sourceBufferEnsureHighlight,

-- * Attributes
  sourceBufferCanRedo,
  sourceBufferCanUndo,
  sourceBufferHighlightMatchingBrackets,
  sourceBufferHighlightSyntax,
  sourceBufferLanguage,
  sourceBufferSourceStyleScheme,
  sourceBufferMaxUndoLevels,

-- * Signals
  sourceBufferHighlightUpdated,
  sourceBufferRedoSignal,
  sourceBufferUndoSignal,
  sourceBufferSourceMarkUpdated,
  ) where

import Control.Monad	(liftM)
import Data.Maybe    (fromMaybe)

import System.Glib.FFI
import System.Glib.GList
import System.Glib.GObject              (constructNewGObject,
					 makeNewGObject)
{#import System.Glib.Properties#}
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import Graphics.UI.Gtk.SourceView.Signals#}
{#import Graphics.UI.Gtk.Multiline.TextIter#}

{#import Graphics.UI.Gtk.SourceView.SourceMark#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new 'SourceBuffer', possibly
-- taking a 'TextTagTable'.
--
sourceBufferNew :: Maybe TextTagTable -> IO SourceBuffer
sourceBufferNew tt = constructNewGObject mkSourceBuffer $
  {#call unsafe source_buffer_new#} 
  (fromMaybe (TextTagTable nullForeignPtr) tt)

-- | Create a new 'SourceBuffer'
-- with a 'SourceLanguage'.
--
sourceBufferNewWithLanguage :: SourceLanguage -> IO SourceBuffer
sourceBufferNewWithLanguage lang = constructNewGObject mkSourceBuffer $
  {#call unsafe source_buffer_new_with_language#} lang

-- | Controls whether syntax is highlighted in the buffer. If highlight is 'True', the text will be
-- highlighted according to the syntax patterns specified in the language set with
-- 'sourceBufferSetLanguage'. If highlight is 'False', syntax highlighting is disabled and all the
-- 'TextTag' objects that have been added by the syntax highlighting engine are removed from the
-- buffer.
sourceBufferSetHighlightSyntax :: SourceBuffer  -- ^ @buffer@    a 'SourceBuffer'.                                       
                               -> Bool  -- ^ @highlight@ 'True' to enable syntax highlighting, 'False' to disable it. 
                               -> IO ()
sourceBufferSetHighlightSyntax sb newVal =
  {#call unsafe source_buffer_set_highlight_syntax#} sb (fromBool newVal)
  
-- | Determines whether syntax highlighting is activated in the source buffer.
sourceBufferGetHighlightSyntax :: SourceBuffer 
                               -> IO Bool  -- ^ returns 'True' if syntax highlighting is enabled, 'False' otherwise. 
sourceBufferGetHighlightSyntax sb = liftM toBool $
  {#call unsafe source_buffer_get_highlight_syntax#} sb

-- | Associate a 'SourceLanguage' with the source buffer. If language is not-'Nothing' and syntax
-- highlighting is enabled (see 'sourceBufferSetHighlightSyntax', the syntax patterns defined
-- in language will be used to highlight the text contained in the buffer. If language is 'Nothing', the
-- text contained in the buffer is not highlighted.
sourceBufferSetLanguage :: SourceBuffer 
                        -> SourceLanguage  -- ^ @language@ a 'SourceLanguage' to set, or 'Nothing'. 
                        -> IO ()
sourceBufferSetLanguage sb lang =
  {#call unsafe source_buffer_set_language#} sb lang
  
-- | Returns the 'SourceLanguage' associated with the buffer, see 'sourceBufferSetLanguage'. The
-- returned object should not be unreferenced by the user.
sourceBufferGetLanguage :: SourceBuffer 
                        -> IO SourceLanguage -- ^ returns 'SourceLanguage' associated with the buffer, or 'Nothing'. 
sourceBufferGetLanguage sb = makeNewGObject mkSourceLanguage $
  {#call unsafe source_buffer_get_language#} sb

-- | Controls the bracket match highlighting function in the buffer. If activated, when you position your
-- cursor over a bracket character (a parenthesis, a square bracket, etc.) the matching opening or
-- closing bracket character will be highlighted. You can specify the style with the
-- 'sourceBufferSetBracketMatchStyle' function.
sourceBufferSetHighlightMatchingBrackets :: SourceBuffer 
                                         -> Bool  -- ^ @highlight@ 'True' if you want matching brackets highlighted. 
                                         -> IO ()
sourceBufferSetHighlightMatchingBrackets sb newVal =
  {#call unsafe source_buffer_set_highlight_matching_brackets#} sb (fromBool newVal)
  
-- | Determines whether bracket match highlighting is activated for the source buffer.
sourceBufferGetHighlightMatchingBrackets :: SourceBuffer 
                                         -> IO Bool  -- ^ returns 'True' if the source buffer will highlight matching brackets. 
sourceBufferGetHighlightMatchingBrackets sb = liftM toBool $
  {#call unsafe source_buffer_get_highlight_matching_brackets#} sb

-- | Sets style scheme used by the buffer. If scheme is 'Nothing' no style scheme is used.
sourceBufferSetStyleScheme :: SourceBuffer 
                           -> SourceStyleScheme  -- ^ @scheme@ style scheme.      
                           -> IO ()
sourceBufferSetStyleScheme sb sss =
    {#call unsafe source_buffer_set_style_scheme#} sb sss

-- | Returns the 'SourceStyleScheme' currently used in buffer.
sourceBufferGetStyleScheme :: SourceBuffer 
                           -> IO SourceStyleScheme -- ^ returns the 'SourceStyleScheme' set by 'sourceBufferSetStyleScheme', or 'Nothing'.
sourceBufferGetStyleScheme sb = makeNewGObject mkSourceStyleScheme $
  {#call unsafe source_buffer_get_style_scheme#} sb

-- | Sets the number of undo levels for user actions the buffer will track. If the number of user actions
-- exceeds the limit set by this function, older actions will be discarded.
-- 
-- If @maxUndoLevels@ is -1, no limit is set.
-- 
-- A new action is started whenever the function 'textBufferBeginUserAction' is called. In
-- general, this happens whenever the user presses any key which modifies the buffer, but the undo
-- manager will try to merge similar consecutive actions, such as multiple character insertions into
-- one action. But, inserting a newline does start a new action.
sourceBufferSetMaxUndoLevels :: SourceBuffer 
                             -> Int  -- ^ @maxUndoLevels@ the desired maximum number of undo levels. 
                             -> IO ()
sourceBufferSetMaxUndoLevels sb newVal =
  {#call unsafe source_buffer_set_max_undo_levels#} sb (fromIntegral newVal)
  
-- | Determines the number of undo levels the buffer will track for buffer edits.
sourceBufferGetMaxUndoLevels :: SourceBuffer 
                             -> IO Int -- ^ returns the maximum number of possible undo levels or -1 if no limit is set.
sourceBufferGetMaxUndoLevels sb = liftM fromIntegral $
  {#call unsafe source_buffer_get_max_undo_levels#} sb

-- | Determines whether a source buffer can undo the last action.
sourceBufferGetCanUndo :: SourceBuffer 
                       -> IO Bool -- ^ returns 'True' if it's possible to undo the last action. 
sourceBufferGetCanUndo sb = liftM toBool $
  {#call unsafe source_buffer_can_undo#} sb
  
-- | Determines whether a source buffer can redo the last action (i.e. if the last operation was an
-- undo).
sourceBufferGetCanRedo :: SourceBuffer 
                       -> IO Bool -- ^ returns 'True' if a redo is possible. 
sourceBufferGetCanRedo sb = liftM toBool $
  {#call unsafe source_buffer_can_redo#} sb

-- | Undoes the last user action which modified the buffer. Use 'sourceBufferCanUndo' to check
-- whether a call to this function will have any effect.
-- 
-- Actions are defined as groups of operations between a call to 'textBufferBeginUserAction'
-- and 'textBufferEndUserAction' on the
-- same line.
sourceBufferUndo :: SourceBuffer -> IO ()
sourceBufferUndo sb =
  {#call source_buffer_undo#} sb
  
-- | Redoes the last undo operation. Use 'sourceBufferCanRedo' to check whether a call to this
-- function will have any effect.
sourceBufferRedo :: SourceBuffer -> IO ()
sourceBufferRedo sb =
  {#call source_buffer_redo#} sb

-- | Marks the beginning of a not undoable action on the buffer, disabling the undo manager. Typically
-- you would call this function before initially setting the contents of the buffer (e.g. when loading
-- a file in a text editor).
-- 
-- You may nest 'sourceBufferBeginNotUndoableAction' /
-- 'sourceBufferEndNotUndoableAction' blocks.
sourceBufferBeginNotUndoableAction :: SourceBuffer -> IO ()
sourceBufferBeginNotUndoableAction sb =
  {#call source_buffer_begin_not_undoable_action#} sb
  
-- | Marks the end of a not undoable action on the buffer. When the last not undoable block is closed
-- through the call to this function, the list of undo actions is cleared and the undo manager is
-- re-enabled.
sourceBufferEndNotUndoableAction :: SourceBuffer -> IO ()
sourceBufferEndNotUndoableAction sb =
  {#call source_buffer_end_not_undoable_action#} sb

-- | Creates a marker in the buffer of the given type.
--
-- *  A marker is
--    semantically very similar to a 'Graphics.UI.Gtk.Multiline.TextMark',
--    except it has a type
--    which is used by the 'SourceView' displaying the buffer to show a
--    pixmap on the left margin, at the line the marker is in.  Because
--    of this, a marker is generally associated to a line and not a
--    character position.  Markers are also accessible through a position
--    or range in the buffer.
--
-- *  Markers are implemented using 'Graphics.UI.Gtk.Multiline.TextMark',
--    so all characteristics
--    and restrictions to marks apply to markers too.  These includes
--    life cycle issues and 'Graphics.UI.Gtk.Multiline.TextMark.onMarkSet'
--    and 'Graphics.UI.Gtk.Multiline.TextMark.onMarkDeleted' signal
--    emissions.
--
-- *  Like a 'Graphics.UI.Gtk.Multiline.TextMark', a 'SourceMarker'
--    can be anonymous if the
--    passed name is @Nothing@.  Also, the buffer owns the markers so you
--    shouldn't unreference it.

sourceBufferCreateSourceMark :: SourceBuffer -- the buffer
                         -> Maybe String -- the name of the mark
                         -> String -- the category of the mark
                         -> TextIter -> IO SourceMark
sourceBufferCreateSourceMark sb name category iter =
  makeNewGObject mkSourceMark $
  maybeWith withCString name       $ \strPtr1 ->
  withCString category $ \strPtr2 ->
  {#call source_buffer_create_source_mark#} sb strPtr1 strPtr2 iter

-- | Returns the list of marks of the given category at line. If category is empty, all marks at line are
-- returned.
sourceBufferGetSourceMarksAtLine :: SourceBuffer  -- ^ @buffer@   a 'SourceBuffer'.             
                                 -> Int -- ^ @line@     a line number.                 
                                 -> String -- ^ @category@ category to search for or empty 
                                 -> IO [SourceMark]
sourceBufferGetSourceMarksAtLine buffer line category = 
  withCString category $ \categoryPtr ->
  {#call gtk_source_buffer_get_source_marks_at_line #}
    buffer 
    (fromIntegral line)
    categoryPtr
  >>= readGSList
  >>= mapM (\markPtr -> makeNewGObject mkSourceMark (return markPtr))

-- | Returns the list of marks of the given category at iter. If category is empty it returns all marks at
-- iter.
sourceBufferGetSourceMarksAtIter :: SourceBuffer -- ^ @buffer@   a 'SourceBuffer'.             
                                 -> TextIter -- ^ @iter@     an iterator.                   
                                 -> String -- ^ @category@ category to search for or empty
                                 -> IO [SourceMark]
sourceBufferGetSourceMarksAtIter buffer iter category =
  withCString category $ \categoryPtr ->
  {#call gtk_source_buffer_get_source_marks_at_iter #}
    buffer
    iter
    categoryPtr
  >>= readGSList
  >>= mapM (\markPtr -> makeNewGObject mkSourceMark (return markPtr))

-- | Remove all marks of category between start and end from the buffer. If category is empty, all marks
-- in the range will be removed.
sourceBufferRemoveSourceMarks :: SourceBuffer -- ^ @buffer@   a 'SourceBuffer'.             
                              -> TextIter -- ^ @start@    a 'TextIter'                  
                              -> TextIter -- ^ @end@      a 'TextIter'                  
                              -> String -- ^ @category@ category to search for or empty
                              -> IO ()
sourceBufferRemoveSourceMarks buffer start end category =
  withCString category $ \categoryPtr ->
  {#call gtk_source_buffer_remove_source_marks #}
     buffer
     start
     end
     categoryPtr

-- | Moves iter to the position of the next 'SourceMark' of the given category. Returns 'True' if iter was
-- moved. If category is empty, the next source mark can be of any category.
sourceBufferForwardIterToSourceMark :: SourceBuffer -- ^ @buffer@   a 'SourceBuffer'.             
                                    -> TextIter -- ^ @iter@     an iterator.                   
                                    -> String -- ^ @category@ category to search for or emtpy
                                    -> IO Bool -- ^ returns  whether iter moved.            
sourceBufferForwardIterToSourceMark buffer iter category =
  liftM toBool $
  withCString category $ \categoryPtr ->
  {#call gtk_source_buffer_forward_iter_to_source_mark #}
    buffer
    iter
    categoryPtr

-- | Moves iter to the position of the previous 'SourceMark' of the given category. Returns 'True' if iter
-- was moved. If category is empty, the previous source mark can be of any category.
sourceBufferBackwardIterToSourceMark :: SourceBuffer  -- ^ @buffer@   a 'SourceBuffer'.             
                                     -> TextIter -- ^ @iter@     an iterator.                   
                                     -> String -- ^ @category@ category to search for or emtpy
                                     -> IO Bool -- ^ returns  whether iter moved.            
sourceBufferBackwardIterToSourceMark buffer iter category =
  liftM toBool $
  withCString category $ \categoryPtr ->
  {#call gtk_source_buffer_backward_iter_to_source_mark #}
    buffer
    iter
    categoryPtr

-- | Forces buffer to analyze and highlight the given area synchronously.
-- 
-- Note
-- 
-- This is a potentially slow operation and should be used only when you need to make sure that some
-- text not currently visible is highlighted, for instance before printing.
sourceBufferEnsureHighlight :: SourceBuffer 
                            -> TextIter  -- ^ @start@  start of the area to highlight. 
                            -> TextIter  -- ^ @end@    end of the area to highlight.   
                            -> IO ()
sourceBufferEnsureHighlight sb start end =
    {#call source_buffer_ensure_highlight#} sb start end

-- | Whether Redo operation is possible.
-- 
-- Default value: 'False'
-- 
sourceBufferCanRedo :: ReadAttr SourceBuffer Bool
sourceBufferCanRedo = readAttrFromBoolProperty "can-redo"

-- | Whether Undo operation is possible.
-- 
-- Default value: 'False'
sourceBufferCanUndo :: ReadAttr SourceBuffer Bool
sourceBufferCanUndo = readAttrFromBoolProperty "can-undo"

-- | Whether to highlight matching brackets in the buffer.
-- 
-- Default value: 'True'
--
sourceBufferHighlightMatchingBrackets :: Attr SourceBuffer Bool
sourceBufferHighlightMatchingBrackets = newAttrFromBoolProperty "highlight-matching-brackets"

-- | Whether to highlight syntax in the buffer.
-- 
-- Default value: 'True'
--
sourceBufferHighlightSyntax :: Attr SourceBuffer Bool
sourceBufferHighlightSyntax = newAttrFromBoolProperty "highlight-matching-brackets"

-- | Language object to get highlighting patterns from.
-- 
sourceBufferLanguage :: Attr SourceBuffer (Maybe SourceLanguage)
sourceBufferLanguage = newAttrFromMaybeObjectProperty "language" gTypeSourceLanguage

-- | Number of undo levels for the buffer. -1 means no limit. This property will only affect the default
-- undo manager.
-- 
-- Allowed values: >= GMaxulong
-- 
-- Default value: 1000
--
sourceBufferMaxUndoLevels :: Attr SourceBuffer Int
sourceBufferMaxUndoLevels = newAttrFromIntProperty "max-undo-levels"

-- | Style scheme. It contains styles for syntax highlighting, optionally foreground, background, cursor
-- color, current line color, and matching brackets style.
-- 
sourceBufferSourceStyleScheme :: Attr SourceBuffer (Maybe SourceStyleScheme)
sourceBufferSourceStyleScheme = newAttrFromMaybeObjectProperty "style-scheme" gTypeSourceStyleScheme

-- |
--
sourceBufferHighlightUpdated :: Signal SourceBuffer (TextIter -> TextIter -> IO ())
sourceBufferHighlightUpdated = Signal $ connect_BOXED_BOXED__NONE "highlight-updated" mkTextIterCopy mkTextIterCopy

-- |
--
sourceBufferRedoSignal :: Signal SourceBuffer (IO ())
sourceBufferRedoSignal = Signal $ connect_NONE__NONE "redo"

-- |
--
sourceBufferUndoSignal :: Signal SourceBuffer (IO ())
sourceBufferUndoSignal = Signal $ connect_NONE__NONE "undo"

-- | The 'sourceBufferMarkUpdated' signal is emitted each time a mark is added to, moved or removed from the
-- buffer.
--
sourceBufferSourceMarkUpdated :: Signal SourceBuffer (TextMark -> IO ())
sourceBufferSourceMarkUpdated = Signal $ connect_OBJECT__NONE "source-mark-updated" 
