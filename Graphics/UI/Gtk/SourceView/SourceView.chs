{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SourceView
--
--  Author : Peter Gavin
--  derived from sourceview bindings by Axel Simon and Duncan Coutts
--
--  Created: 18 December 2008
--
--  Copyright (C) 2004-2008 Peter Gavin, Duncan Coutts, Axel Simon
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
module Graphics.UI.Gtk.SourceView.SourceView (
-- * Description
-- | 'SourceView' is the main object of the gtksourceview library. It provides a text view which syntax
-- highlighting, undo/redo and text marks. Use a 'SourceBuffer' to display text with a 'SourceView'.

-- * Types
  SourceView,
  SourceViewClass,

-- * Enums
  SourceSmartHomeEndType(..),
  SourceDrawSpacesFlags(..),
  SourceViewGutterPosition (..),

-- * Methods  
  castToSourceView,
  sourceViewNew,
  sourceViewNewWithBuffer,
  sourceViewSetAutoIndent,
  sourceViewGetAutoIndent,
  sourceViewSetIndentOnTab,
  sourceViewGetIndentOnTab,
  sourceViewSetIndentWidth,
  sourceViewGetIndentWidth,
  sourceViewSetInsertSpacesInsteadOfTabs,
  sourceViewGetInsertSpacesInsteadOfTabs,
  sourceViewSetSmartHomeEnd,
  sourceViewGetSmartHomeEnd,
#if GTK_MAJOR_VERSION < 3
  sourceViewSetMarkCategoryPriority,
  sourceViewGetMarkCategoryPriority,
  sourceViewSetMarkCategoryIconFromPixbuf,
  sourceViewSetMarkCategoryIconFromStock,
  sourceViewSetMarkCategoryIconFromIconName,
  sourceViewSetMarkCategoryBackground,
  sourceViewGetMarkCategoryBackground,
#endif
  sourceViewSetHighlightCurrentLine,
  sourceViewGetHighlightCurrentLine,
  sourceViewSetShowLineMarks,
  sourceViewGetShowLineMarks,
  sourceViewSetShowLineNumbers,
  sourceViewGetShowLineNumbers,
  sourceViewSetShowRightMargin,
  sourceViewGetShowRightMargin,
  sourceViewSetRightMarginPosition,
  sourceViewGetRightMarginPosition,
  sourceViewSetTabWidth,
  sourceViewGetTabWidth,
  sourceViewSetDrawSpaces,
  sourceViewGetDrawSpaces,
  sourceViewGetGutter,

-- * Attributes  
  sourceViewAutoIndent,
  sourceViewCompletion,
  sourceViewDrawSpaces,
  sourceViewHighlightCurrentLine,
  sourceViewIndentOnTab,
  sourceViewIndentWidth,
  sourceViewInsertSpacesInsteadOfTabs,
  sourceViewRightMarginPosition,
  sourceViewShowLineNumbers,
  sourceViewShowRightMargin,
  sourceViewSmartHomeEnd,
  sourceViewTabWidth,

-- * Signals
  sourceViewUndo,
  sourceViewRedo,
  sourceViewMoveLines,
  sourceViewShowCompletion,
  sourceViewLineMarkActivated,

-- * Deprecated
#if GTK_MAJOR_VERSION < 3
#ifndef DISABLE_DEPRECATED
  sourceViewSetMarkCategoryPixbuf,
  sourceViewGetMarkCategoryPixbuf,
#endif
#endif
  ) where

import Control.Monad	(liftM)
import Control.Monad.Reader ( runReaderT )
import Data.Maybe    (fromMaybe)

import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Abstract.Widget (Color)
import Graphics.UI.Gtk.Gdk.EventM (EventM, EAny)
import Graphics.UI.Gtk.Multiline.TextView (TextWindowType (..))
import Graphics.UI.Gtk.SourceView.Enums
import Graphics.UI.GtkInternals  ( TextIter, mkTextIterCopy )
import System.Glib.GObject	(wrapNewGObject, makeNewGObject)
import System.Glib.Attributes
import System.Glib.FFI

{#import Graphics.UI.Gtk.SourceView.Signals#}
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import System.Glib.Properties#}

{# context lib="gtk" prefix="gtk" #}

-- | Create a new 'SourceView' widget with a default 'SourceBuffer'.
--
sourceViewNew :: IO SourceView
sourceViewNew = makeNewObject mkSourceView $ liftM castPtr
  {#call unsafe source_view_new#}

-- | Create a new 'SourceView'
-- widget with the given 'SourceBuffer'.
--
sourceViewNewWithBuffer :: SourceBuffer -> IO SourceView
sourceViewNewWithBuffer sb = makeNewObject mkSourceView $ liftM castPtr $
  {#call source_view_new_with_buffer#} sb

-- | If 'True' auto indentation of text is enabled.
--
sourceViewSetAutoIndent :: SourceViewClass sv => sv 
                        -> Bool  -- ^ @enable@ whether to enable auto indentation. 
                        -> IO ()
sourceViewSetAutoIndent sv enable =
  {#call source_view_set_auto_indent#} (toSourceView sv) (fromBool enable)
  
-- | Returns whether auto indentation of text is enabled.
--
sourceViewGetAutoIndent :: SourceViewClass sv => sv 
                        -> IO Bool  -- ^ returns 'True' if auto indentation is enabled. 
sourceViewGetAutoIndent sv = liftM toBool $
  {#call unsafe source_view_get_auto_indent#} (toSourceView sv)

-- | If 'True', when the tab key is pressed and there is a selection, the selected text is indented of one
-- level instead of being replaced with the \t characters. Shift+Tab unindents the selection.
--
sourceViewSetIndentOnTab :: SourceViewClass sv => sv 
                         -> Bool  -- ^ @enable@ whether to indent a block when tab is pressed. 
                         -> IO ()
sourceViewSetIndentOnTab sv enable =
  {#call source_view_set_indent_on_tab#} (toSourceView sv) (fromBool enable)
  
-- | Returns whether when the tab key is pressed the current selection should get indented instead of
-- replaced with the \t character.
--
sourceViewGetIndentOnTab :: SourceViewClass sv => sv 
                         -> IO Bool  -- ^ returns 'True' if the selection is indented when tab is pressed. 
sourceViewGetIndentOnTab sv = liftM toBool $
  {#call unsafe source_view_get_indent_on_tab#} (toSourceView sv)

-- | Sets the number of spaces to use for each step of indent. If width is -1, the value of the
-- 'tabWidth' property will be used.
--
sourceViewSetIndentWidth :: SourceViewClass sv => sv 
                         -> Int  -- ^ @width@ indent width in characters. 
                         -> IO ()
sourceViewSetIndentWidth sv width =
  {#call source_view_set_indent_width#} (toSourceView sv) (fromIntegral width)

-- | Returns the number of spaces to use for each step of indent. See 'sourceViewSetIndentWidth'
-- for details.
--
sourceViewGetIndentWidth :: SourceViewClass sv => sv 
                         -> IO Int -- ^ returns indent width.    
sourceViewGetIndentWidth sv = liftM fromIntegral $
  {#call unsafe source_view_get_indent_width#} (toSourceView sv)

-- | If 'True' any tabulator character inserted is replaced by a group of space characters.
--
sourceViewSetInsertSpacesInsteadOfTabs :: SourceViewClass sv => sv 
                                       -> Bool  -- ^ @enable@ whether to insert spaces instead of tabs. 
                                       -> IO ()
sourceViewSetInsertSpacesInsteadOfTabs sv enable =
  {#call source_view_set_insert_spaces_instead_of_tabs#} (toSourceView sv) (fromBool enable)
  
-- | Returns whether when inserting a tabulator character it should be replaced by a group of space
-- characters.
--
sourceViewGetInsertSpacesInsteadOfTabs :: SourceViewClass sv => sv 
                                       -> IO Bool  -- ^ returns 'True' if spaces are inserted instead of tabs. 
sourceViewGetInsertSpacesInsteadOfTabs sv = liftM toBool $
  {#call unsafe source_view_get_insert_spaces_instead_of_tabs#} (toSourceView sv)

-- | Set the desired movement of the cursor when HOME and END keys are pressed.
--
sourceViewSetSmartHomeEnd :: SourceViewClass sv => sv 
                          -> SourceSmartHomeEndType  -- ^ @smartHe@ the desired behavior among 'SourceSmartHomeEndType'. 
                          -> IO ()
sourceViewSetSmartHomeEnd sv newVal =
  {#call source_view_set_smart_home_end#} (toSourceView sv) (fromIntegral $ fromEnum newVal)
  
-- | Returns a 'SourceSmartHomeEndType' end value specifying how the cursor will move when HOME and END
-- keys are pressed.
--
sourceViewGetSmartHomeEnd :: SourceViewClass sv => sv 
                          -> IO SourceSmartHomeEndType -- ^ returns a 'SourceSmartHomeEndTypeend' value. 
sourceViewGetSmartHomeEnd sv = liftM (toEnum . fromIntegral) $
  {#call unsafe source_view_get_smart_home_end#} (toSourceView sv)

-- | If show is 'True' the current line is highlighted.
--
sourceViewSetHighlightCurrentLine :: SourceViewClass sv => sv 
                                  -> Bool  -- ^ @show@ whether to highlight the current line 
                                  -> IO ()
sourceViewSetHighlightCurrentLine sv newVal =
  {#call source_view_set_highlight_current_line#} (toSourceView sv) (fromBool newVal)
  
-- | Returns whether the current line is highlighted
--
sourceViewGetHighlightCurrentLine :: SourceViewClass sv => sv 
                                  -> IO Bool  -- ^ returns 'True' if the current line is highlighted. 
sourceViewGetHighlightCurrentLine sv = liftM toBool $
  {#call unsafe source_view_get_highlight_current_line#} (toSourceView sv)

-- | If 'True' line marks will be displayed beside the text.
--
sourceViewSetShowLineMarks :: SourceViewClass sv => sv 
                           -> Bool  -- ^ @show@ whether line marks should be displayed. 
                           -> IO ()
sourceViewSetShowLineMarks sv newVal =
  {#call source_view_set_show_line_marks#} (toSourceView sv) (fromBool newVal)
  
-- | Returns whether line marks are displayed beside the text.
--
sourceViewGetShowLineMarks :: SourceViewClass sv => sv 
                           -> IO Bool  -- ^ returns 'True' if the line marks are displayed. 
sourceViewGetShowLineMarks sv = liftM toBool $
  {#call unsafe source_view_get_show_line_marks#} (toSourceView sv)

-- | If 'True' line numbers will be displayed beside the text.
--
sourceViewSetShowLineNumbers :: SourceViewClass sv => sv 
                             -> Bool  -- ^ @show@ whether line numbers should be displayed. 
                             -> IO ()
sourceViewSetShowLineNumbers sv newVal =
  {#call source_view_set_show_line_numbers#} (toSourceView sv) (fromBool newVal)
  
-- | Returns whether line numbers are displayed beside the text.
--
sourceViewGetShowLineNumbers :: SourceViewClass sv => sv 
                             -> IO Bool  -- ^ returns 'True' if the line numbers are displayed. 
sourceViewGetShowLineNumbers sv = liftM toBool $
  {#call unsafe source_view_get_show_line_numbers#} (toSourceView sv)

-- | If 'True' a right margin is displayed
--
sourceViewSetShowRightMargin :: SourceViewClass sv => sv 
                             -> Bool  -- ^ @show@ whether to show a right margin. 
                             -> IO ()
sourceViewSetShowRightMargin sv newVal =
  {#call source_view_set_show_right_margin#} (toSourceView sv) (fromBool newVal)
  
-- | Returns whether a right margin is displayed.
--
sourceViewGetShowRightMargin :: SourceViewClass sv => sv 
                             -> IO Bool -- ^ returns 'True' if the right margin is shown. 
sourceViewGetShowRightMargin sv = liftM toBool $
  {#call source_view_get_show_right_margin#} (toSourceView sv)
  
-- | Sets the position of the right margin in the given view.
--
sourceViewSetRightMarginPosition :: SourceViewClass sv => sv 
                                 -> Word -- ^ @pos@  the width in characters where to position the right margin.  
                                 -> IO ()
sourceViewSetRightMarginPosition sv margin =
  {#call source_view_set_right_margin_position#} (toSourceView sv) (fromIntegral margin)
  
-- | Gets the position of the right margin in the given view.
--
sourceViewGetRightMarginPosition :: SourceViewClass sv => sv 
                                 -> IO Int  -- ^ returns the position of the right margin. 
sourceViewGetRightMarginPosition sv = liftM fromIntegral $
  {#call unsafe source_view_get_right_margin_position#} (toSourceView sv)

-- | Sets the width of tabulation in characters.
--
sourceViewSetTabWidth :: SourceViewClass sv => sv 
                      -> Int  -- ^ @width@ width of tab in characters. 
                      -> IO ()
sourceViewSetTabWidth sv width =
  {#call source_view_set_tab_width#} (toSourceView sv) (fromIntegral width)
  
-- | Returns the width of tabulation in characters.
--
sourceViewGetTabWidth :: SourceViewClass sv => sv 
                      -> IO Int -- ^ returns width of tab.     
sourceViewGetTabWidth sv = liftM fromIntegral $
  {#call unsafe source_view_get_tab_width#} (toSourceView sv)

-- | Set if and how the spaces should be visualized. Specifying flags as 0 will disable display of
-- spaces.
sourceViewSetDrawSpaces :: SourceViewClass sv => sv
                        -> SourceDrawSpacesFlags -- ^ @flags@ 'SourceDrawSpacesFlags' specifing how white spaces should be displayed
                        -> IO ()
sourceViewSetDrawSpaces view flags =
  {#call gtk_source_view_set_draw_spaces #}  
    (toSourceView view)
    (fromIntegral $ fromEnum flags)

-- | Returns the 'SourceDrawSpacesFlags' specifying if and how spaces should be displayed for this view.
sourceViewGetDrawSpaces :: SourceViewClass sv => sv
                        -> IO SourceDrawSpacesFlags -- ^ returns the 'SourceDrawSpacesFlags', 0 if no spaces should be drawn. 
sourceViewGetDrawSpaces view =                        
  liftM (toEnum . fromIntegral) $
  {#call gtk_source_view_get_draw_spaces #}
     (toSourceView view)

-- | Returns the 'SourceGutter' object associated with @windowType@ for view. Only 'TextWindowLeft'
-- and 'TextWindowRight' are supported, respectively corresponding to the left and right
-- gutter. The line numbers and mark category icons are rendered in the gutter corresponding to
-- 'TextWindowLeft'.
sourceViewGetGutter :: SourceViewClass sv => sv 
                    -> TextWindowType  -- ^ @windowType@ the gutter window type 
                    -> IO SourceGutter
sourceViewGetGutter sv windowType =
  makeNewGObject mkSourceGutter $
  {#call gtk_source_view_get_gutter #}
    (toSourceView sv)
    (fromIntegral $ fromEnum windowType)

#if GTK_MAJOR_VERSION < 3
-- | Set the priority for the given mark category. When there are multiple marks on the same line, marks
-- of categories with higher priorities will be drawn on top.
--
sourceViewSetMarkCategoryPriority :: SourceViewClass sv => sv 
                                  -> String  -- ^ @category@ a mark category.              
                                  -> Int  -- ^ @priority@ the priority for the category 
                                  -> IO ()
sourceViewSetMarkCategoryPriority sv markerType priority = withCString markerType $ \strPtr ->
  {#call source_view_set_mark_category_priority#} (toSourceView sv) strPtr (fromIntegral priority)

-- | Gets the priority which is associated with the given category.
--
sourceViewGetMarkCategoryPriority :: SourceViewClass sv => sv 
                                  -> String  -- ^ @category@ a mark category.   
                                  -> IO Int -- ^ returns  the priority or if category exists but no priority was set, it defaults to 0.
sourceViewGetMarkCategoryPriority sv markerType = withCString markerType $ \strPtr ->
  liftM fromIntegral $
  {#call unsafe source_view_get_mark_category_priority#} (toSourceView sv) strPtr

-- | Sets the icon to be used for category to pixbuf. If pixbuf is 'Nothing', the icon is unset.
sourceViewSetMarkCategoryIconFromPixbuf :: SourceViewClass sv => sv
                                        -> String -- ^ @category@ a mark category.     
                                        -> Maybe Pixbuf -- ^ @pixbuf@   a 'Pixbuf' or 'Nothing'. 
                                        -> IO ()
sourceViewSetMarkCategoryIconFromPixbuf sv category pixbuf =
  withCString category $ \categoryPtr ->
  {#call gtk_source_view_set_mark_category_icon_from_pixbuf #}
     (toSourceView sv)
     categoryPtr
     (fromMaybe (Pixbuf nullForeignPtr) pixbuf)

-- | Sets the icon to be used for category to the stock item @stockId@. If @stockId@ is 'Nothing', the icon is
-- unset.
sourceViewSetMarkCategoryIconFromStock :: SourceViewClass sv => sv
                                       -> String -- ^ @category@ a mark category.      
                                       -> Maybe String -- ^ @stockId@ the stock id or 'Nothing'. 
                                       -> IO ()
sourceViewSetMarkCategoryIconFromStock sv category stockId =
  withCString category $ \categoryPtr ->
  maybeWith withCString stockId $ \stockIdPtr ->
  {#call gtk_source_view_set_mark_category_icon_from_stock #}
    (toSourceView sv)
    categoryPtr
    stockIdPtr

-- | Sets the icon to be used for category to the named theme item name. If name is 'Nothing', the icon is
-- unset.
sourceViewSetMarkCategoryIconFromIconName :: SourceViewClass sv => sv
                                       -> String -- ^ @category@ a mark category.      
                                       -> Maybe String -- ^ @name@     the themed icon name or 'Nothing'. 
                                       -> IO ()
sourceViewSetMarkCategoryIconFromIconName sv category name =
  withCString category $ \categoryPtr ->
  maybeWith withCString name $ \namePtr ->
  {#call gtk_source_view_set_mark_category_icon_from_icon_name #}
    (toSourceView sv)
    categoryPtr
    namePtr

-- | Sets given background color for mark category. If color is 'Nothing', the background color is unset.
sourceViewSetMarkCategoryBackground :: SourceViewClass sv => sv
                                    -> String -- ^ @category@ a mark category.                      
                                    -> Maybe Color  -- ^ @color@    background color or 'Nothing' to unset it. 
                                    -> IO ()
sourceViewSetMarkCategoryBackground sv category color =
  let withMB :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
      withMB Nothing f = f nullPtr
      withMB (Just x) f = with x f
  in withCString category $ \categoryPtr ->
     withMB color $ \colorPtr ->
     {#call gtk_source_view_set_mark_category_background #}
       (toSourceView sv)
       categoryPtr
       (castPtr colorPtr)

-- | Gets the background color associated with given category.
sourceViewGetMarkCategoryBackground :: SourceViewClass sv => sv
                                    -> String -- ^ @category@ a mark category.
                                    -> Color -- ^ @dest@     destination 'Color' structure to fill in.
                                    -> IO Bool -- ^ returns  'True' if background color for category was set and dest is set to a valid color, or 'False' otherwise.
sourceViewGetMarkCategoryBackground sv category color = 
    liftM toBool $
    withCString category $ \ categoryPtr ->
    with color $ \ colorPtr ->
    {#call gtk_source_view_get_mark_category_background #}
      (toSourceView sv)
      categoryPtr
      (castPtr colorPtr)
#endif

-- | Whether to enable auto indentation.
-- 
-- Default value: 'False'
--
sourceViewAutoIndent :: SourceViewClass sv => Attr sv Bool
sourceViewAutoIndent = newAttrFromBoolProperty "auto-indent"

-- | The completion object associated with the view.
--
sourceViewCompletion :: SourceViewClass sv => ReadAttr sv SourceCompletion
sourceViewCompletion = readAttrFromObjectProperty "completion"
                       {#call pure unsafe gtk_source_completion_get_type #}

-- | Set if and how the spaces should be visualized.
--
sourceViewDrawSpaces :: SourceViewClass sv => Attr sv SourceDrawSpacesFlags
sourceViewDrawSpaces = newAttrFromEnumProperty "draw-spaces" {#call fun gtk_source_draw_spaces_flags_get_type#}

-- | Whether to highlight the current line.
-- 
-- Default value: 'False'
--
sourceViewHighlightCurrentLine :: SourceViewClass sv => Attr sv Bool
sourceViewHighlightCurrentLine = newAttrFromBoolProperty "highlight-current-line"

-- | Whether to indent the selected text when the tab key is pressed.
-- 
-- Default value: 'True'
--
sourceViewIndentOnTab :: SourceViewClass sv => Attr sv Bool
sourceViewIndentOnTab = newAttrFromBoolProperty "indent-on-tab"

-- | Width of an indentation step expressed in number of spaces.
-- 
-- Allowed values: [GMaxulong,32]
-- 
-- Default value: -1
--
sourceViewIndentWidth :: SourceViewClass sv => Attr sv Int
sourceViewIndentWidth = newAttrFromIntProperty "indent-width"

-- | Whether to insert spaces instead of tabs.
-- 
-- Default value: 'False'
--
sourceViewInsertSpacesInsteadOfTabs :: SourceViewClass sv => Attr sv Bool
sourceViewInsertSpacesInsteadOfTabs = newAttrFromBoolProperty "insert-spaces-instead-of-tabs"

-- | Position of the right margin.
-- 
-- Allowed values: [1,200]
-- 
-- Default value: 80
--
sourceViewRightMarginPosition :: SourceViewClass sv => Attr sv Int
sourceViewRightMarginPosition = newAttrFromUIntProperty "right-margin-position"

-- | Whether to display line numbers
-- 
-- Default value: 'False'
--
sourceViewShowLineNumbers :: SourceViewClass sv => Attr sv Bool
sourceViewShowLineNumbers = newAttrFromBoolProperty "show-line-numbers"

-- | Whether to display line mark pixbufs
-- 
-- Default value: 'False'
--
sourceViewShowRightMargin :: SourceViewClass sv => Attr sv Bool
sourceViewShowRightMargin = newAttrFromBoolProperty "show-right-margin"

-- | Set the behavior of the HOME and END keys.
-- 
-- Default value: 'SourceSmartHomeEndDisabled'
-- 
-- Since 2.0
--
sourceViewSmartHomeEnd :: SourceViewClass sv => Attr sv SourceSmartHomeEndType
sourceViewSmartHomeEnd = newAttrFromEnumProperty "smart-home-end" {#call fun gtk_source_smart_home_end_type_get_type#}

-- | Width of an tab character expressed in number of spaces.
-- 
-- Allowed values: [1,32]
-- 
-- Default value: 8
--
sourceViewTabWidth :: SourceViewClass sv => Attr sv Int
sourceViewTabWidth = newAttrFromUIntProperty "tab-width"

-- |
--
sourceViewUndo :: SourceViewClass sv => Signal sv (IO ())
sourceViewUndo = Signal $ connect_NONE__NONE "undo"

-- |
--
sourceViewRedo :: SourceViewClass sv => Signal sv (IO ())
sourceViewRedo = Signal $ connect_NONE__NONE "redo"

-- | The 'moveLines' signal is a keybinding which gets emitted when the user initiates moving a
-- line. The default binding key is Alt+Up/Down arrow. And moves the currently selected lines, or the
-- current line by count. For the moment, only count of -1 or 1 is valid.
sourceViewMoveLines :: SourceViewClass sv => Signal sv (Bool -> Int -> IO ())
sourceViewMoveLines = Signal $ connect_BOOL_INT__NONE "move-lines"

-- | The 'showCompletion' signal is a keybinding signal which gets emitted when the user initiates a
-- completion in default mode.
-- 
-- Applications should not connect to it, but may emit it with @gSignalEmitByName@ if they need to
-- control the default mode completion activation.
sourceViewShowCompletion :: SourceViewClass sv => Signal sv (IO ())
sourceViewShowCompletion = Signal $ connect_NONE__NONE "show-completion"

-- | Emitted when a line mark has been activated (for instance when there was a button press in the line
-- marks gutter). You can use iter to determine on which line the activation took place.
sourceViewLineMarkActivated :: SourceViewClass sv => Signal sv (TextIter -> EventM EAny ())
sourceViewLineMarkActivated = 
  Signal (\after obj fun -> 
           connect_BOXED_PTR__NONE "line-mark-activated" mkTextIterCopy after obj
                                   (\iter eventPtr -> runReaderT (fun iter) eventPtr)
         )

#if GTK_MAJOR_VERSION < 3
-- * Deprecated
#ifndef DISABLE_DEPRECATED
-- | 'sourceViewSetMarkCategoryPixbuf' is deprecated and should not be used in newly-written
-- code. Use 'sourceViewSetMarkCategoryIconFromPixbuf' instead
--
sourceViewSetMarkCategoryPixbuf :: SourceViewClass sv => sv -> String -> Pixbuf -> IO ()
sourceViewSetMarkCategoryPixbuf sv markerType marker = withCString markerType $ \strPtr ->
  {#call source_view_set_mark_category_pixbuf#} (toSourceView sv) strPtr marker

-- | 'sourceViewGetMarkCategoryPixbuf' is deprecated and should not be used in newly-written code.
--
sourceViewGetMarkCategoryPixbuf :: SourceViewClass sv => sv -> String -> IO Pixbuf
sourceViewGetMarkCategoryPixbuf sv markerType = withCString markerType $ \strPtr ->
  wrapNewGObject mkPixbuf $
  {#call unsafe source_view_get_mark_category_pixbuf#} (toSourceView sv) strPtr
#endif
#endif

