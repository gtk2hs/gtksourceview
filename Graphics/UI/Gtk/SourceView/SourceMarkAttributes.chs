{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceMarkAttributes
--
--  Author : Hamish Mackenzie
--
--  Created: 20 June 2015
--
--  Copyright (C) 2015 Hamish Mackenzie
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
-- Maintainer  : http://github.com/gtk2hs
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
module Graphics.UI.Gtk.SourceView.SourceMarkAttributes (
-- * Description
-- | `SourceMarkAttributes` is an object specifying attributes used by
-- a `SourceView` to visually show lines marked with `SourceMarks` of
-- a specific category. It allows you to define a background color of
-- a line, an icon shown in gutter and tooltips.
--
-- The background color is used as a background of a line where a mark
-- is placed and it can be set with `sourceMarkAttributesSetBackground`.
-- To check if any custom background color was defined and what color
-- it is, use `sourceMarkAttributesGetBackground`.
--
-- An icon is a graphic element which is shown in the gutter of a view.
-- An example use is showing a red filled circle in a debugger to show
-- that a breakpoint was set in certain line. To get an icon that will
-- be placed in a gutter, first a base for it must be specified and
-- then `sourceMarkAttributesRenderIcon` must be called. There are
-- several ways to specify a base for an icon:
--
--     * `sourceMarkAttributesSetIconName`
--     * `sourceMarkAttributesSetGIcon`
--     * `sourceMarkAttributesSetPixbuf`
--
--  Using any of the above functions overrides the one used earlier.
-- But note that a getter counterpart of earlier used function can
-- still return some value, but it is just not used when rendering
-- the proper icon.
--
-- To provide meaningful tooltips for a given mark of a category, you
-- should connect to `queryTooltipText` or `queryTooltipMarkup` where
-- the latter takes precedence.

-- * Types
  SourceMarkAttributes,
  SourceMarkAttributesClass,

-- * Methods
  castToSourceMarkAttributes,
  sourceMarkAttributesNew,
  sourceMarkAttributesSetBackground,
  sourceMarkAttributesGetBackground,
  sourceMarkAttributesSetIconName,
  sourceMarkAttributesGetIconName,
  sourceMarkAttributesSetPixbuf,
  sourceMarkAttributesGetPixbuf,
  sourceMarkAttributesRenderIcon,
  sourceMarkAttributesGetTooltipText,
  sourceMarkAttributesGetTooltipMarkup,

-- * Attributes
  sourceMarkAttributesBackground,
  sourceMarkAttributesIconName,
  sourceMarkAttributesPixbuf,

-- * Signals
  queryTooltipMarkup,
  queryTooltipText
) where

import Control.Applicative
import Prelude
import Control.Monad (liftM)
import System.Glib.GObject (makeNewGObject)
import System.Glib.Attributes
import System.Glib.FFI
import System.Glib.UTFString

import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk (RGBA)

{#import Graphics.UI.Gtk.SourceView.Signals#}
{#import Graphics.UI.Gtk.SourceView.Types#}

{# context lib="gtk" prefix="gtk" #}

-- | Creates a new source mark attributes.
--
sourceMarkAttributesNew :: IO SourceMarkAttributes
sourceMarkAttributesNew = makeNewObject mkSourceMarkAttributes $ liftM castPtr
  {# call unsafe source_mark_attributes_new #}

-- | Sets background color.
--
sourceMarkAttributesSetBackground :: SourceMarkAttributesClass attributes => attributes -> Maybe RGBA -> IO ()
sourceMarkAttributesSetBackground attributes background =
  maybeWith with background $ \ backgroundPtr ->
  {# call unsafe source_mark_attributes_set_background #}
    (toSourceMarkAttributes attributes)
    (castPtr backgroundPtr)

-- | Gets background color.
--
sourceMarkAttributesGetBackground :: SourceMarkAttributesClass attributes => attributes -> IO (Maybe RGBA)
sourceMarkAttributesGetBackground attributes =
  alloca $ \ rgbaPtr -> do
  isSet <- {# call gtk_source_mark_attributes_get_background #}
                (toSourceMarkAttributes attributes)
                (castPtr rgbaPtr)
  if isSet /= 0 then Just <$> peek rgbaPtr else return Nothing

-- | Sets a name of an icon to be used as a base for rendered icon.
--
sourceMarkAttributesSetIconName :: (SourceMarkAttributesClass attributes, GlibString iconName)
                                => attributes
                                -> Maybe iconName
                                -> IO ()
sourceMarkAttributesSetIconName attributes iconName =
  maybeWith withUTFString iconName $ \iconNamePtr ->
    {# call gtk_source_mark_attributes_set_icon_name #}
        (toSourceMarkAttributes attributes)
        iconNamePtr

-- | Gets a name of an icon to be used as a base for rendered icon.
--
sourceMarkAttributesGetIconName :: (SourceMarkAttributesClass attributes, GlibString iconName)
                                => attributes
                                -> IO (Maybe iconName)
sourceMarkAttributesGetIconName attributes =
    {# call gtk_source_mark_attributes_get_icon_name #}
        (toSourceMarkAttributes attributes)
        >>= maybePeek peekUTFString

-- | Sets a pixbuf to be used as a base for rendered icon.
--
sourceMarkAttributesSetPixbuf :: SourceMarkAttributesClass attributes
                              => attributes
                              -> Maybe Pixbuf
                              -> IO ()
sourceMarkAttributesSetPixbuf attributes pixbuf =
    {# call source_mark_attributes_set_pixbuf #}
        (toSourceMarkAttributes attributes)
        (maybe (Pixbuf nullForeignPtr) id pixbuf)

-- | Gets a pixbuf to be used as a base for rendered icon.
--
sourceMarkAttributesGetPixbuf :: SourceMarkAttributesClass attributes
                              => attributes
                              -> IO (Maybe Pixbuf)
sourceMarkAttributesGetPixbuf attributes =
  maybeNull (makeNewGObject mkPixbuf) $
    {# call gtk_source_mark_attributes_get_pixbuf #}
        (toSourceMarkAttributes attributes)

-- | Renders an icon of given size. The base of the icon is set by the
-- last call to one of: `sourceMarkAttributesSetPixbuf`,
-- `sourceMarkAttributesSetGIcon` or `sourceMarkAttributessetIconName`.
--
sourceMarkAttributesRenderIcon :: (SourceMarkAttributesClass attributes, WidgetClass widget)
                               => attributes
                               -> widget  -- ^ @widget@ of which style settings may be used.
                               -> Int     -- ^ @size@ of the redered icon. Cannot be lower than 1
                               -> IO (Maybe Pixbuf)
sourceMarkAttributesRenderIcon attributes widget size =
  maybeNull (makeNewGObject mkPixbuf) $
    {# call gtk_source_mark_attributes_render_icon #}
        (toSourceMarkAttributes attributes)
        (toWidget widget)
        (fromIntegral size)

-- | Queries for a tooltip by emitting a `queryTooltipText` signal. The tooltip is a plain text.
--
sourceMarkAttributesGetTooltipText :: (SourceMarkAttributesClass attributes, SourceMarkClass mark, GlibString text)
                                   => attributes
                                   -> mark
                                   -> IO (Maybe text)
sourceMarkAttributesGetTooltipText attributes mark =
    {# call gtk_source_mark_attributes_get_tooltip_text #}
        (toSourceMarkAttributes attributes)
        (toSourceMark mark)
        >>= maybePeek readUTFString

-- | Queries for a tooltip by emitting a `queryTooltipMarkup` signal. The tooltip may contain a markup.
--
sourceMarkAttributesGetTooltipMarkup :: (SourceMarkAttributesClass attributes, SourceMarkClass mark, GlibString markup)
                                     => attributes
                                     -> mark
                                     -> IO (Maybe markup)
sourceMarkAttributesGetTooltipMarkup attributes mark =
    {# call gtk_source_mark_attributes_get_tooltip_markup #}
        (toSourceMarkAttributes attributes)
        (toSourceMark mark)
        >>= maybePeek readUTFString

-- | A color used for background of a line.
--
sourceMarkAttributesBackground :: SourceMarkAttributesClass attributes => Attr attributes (Maybe RGBA)
sourceMarkAttributesBackground = newAttr sourceMarkAttributesGetBackground sourceMarkAttributesSetBackground

-- | An icon name that may be a base of a rendered icon.
--
sourceMarkAttributesIconName :: (SourceMarkAttributesClass attributes, GlibString iconName) => Attr attributes (Maybe iconName)
sourceMarkAttributesIconName = newAttr sourceMarkAttributesGetIconName sourceMarkAttributesSetIconName

-- | A `Pixbuf` that may be a base of a rendered icon.
--
sourceMarkAttributesPixbuf :: SourceMarkAttributesClass attributes => Attr attributes (Maybe Pixbuf)
sourceMarkAttributesPixbuf = newAttr sourceMarkAttributesGetPixbuf sourceMarkAttributesSetPixbuf


-- | The code should connect to this signal to provide a tooltip for given mark . The tooltip can contain a markup.
--
queryTooltipMarkup :: (SourceMarkAttributesClass self, GlibString markup) => Signal self (SourceMark -> IO markup)
queryTooltipMarkup = Signal (connect_OBJECT__GLIBSTRING "query-tooltip-markup")

-- | The code should connect to this signal to provide a tooltip for given mark . The tooltip should be just a plain text.
--
queryTooltipText :: (SourceMarkAttributesClass self, GlibString text) => Signal self (SourceMark -> IO text)
queryTooltipText = Signal (connect_OBJECT__GLIBSTRING "query-tooltip-text")
