{-# OPTIONS_HADDOCK hide #-}
-- -*-haskell-*-
-- -------------------- automatically generated file - do not edit ------------
--  Callback installers for the GIMP Toolkit (GTK) Binding for Haskell
--
--  Author : Axel Simon
--
--  Created: 1 July 2000
--
--  Copyright (C) 2000-2005 Axel Simon
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
-- #hide

-- These functions are used to connect signals to widgets. They are auto-
-- matically created through HookGenerator.hs which takes a list of possible
-- function signatures that are included in the GTK sources (gtkmarshal.list).
--
-- The object system in the second version of GTK is based on GObject from
-- GLIB. This base class is rather primitive in that it only implements
-- ref and unref methods (and others that are not interesting to us). If
-- the marshall list mentions OBJECT it refers to an instance of this 
-- GObject which is automatically wrapped with a ref and unref call.
-- Structures which are not derived from GObject have to be passed as
-- BOXED which gives the signal connect function a possibility to do the
-- conversion into a proper ForeignPtr type. In special cases the signal
-- connect function use a PTR type which will then be mangled in the
-- user function directly. The latter is needed if a signal delivers a
-- pointer to a string and its length in a separate integer.
--
module Graphics.UI.Gtk.SourceView.Signals (
  module System.Glib.Signals,

  connect_NONE__NONE,
  connect_OBJECT__NONE,
  connect_BOXED_BOXED__NONE,
  connect_BOOL_INT__NONE,
  connect_PTR_BOXED__NONE,
  connect_OBJECT_PTR_BOXED__NONE,
  connect_OBJECT_BOXED_OBJECT__BOOL,
  
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString   (peekUTFString)
import System.Glib.GError      (failOnGError)
{#import System.Glib.Signals#}
{#import System.Glib.GObject#} 


{#context lib="gtk" prefix="gtk" #}


-- Here are the generators that turn a Haskell function into
-- a C function pointer. The fist Argument is always the widget,
-- the last one is the user g_pointer. Both are ignored.


connect_NONE__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (IO ()) ->
  IO (ConnectId obj)
connect_NONE__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> IO ()
        action _ =
          failOnGError $
          user

connect_OBJECT__NONE :: 
  (GObjectClass a', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> IO ()) ->
  IO (ConnectId obj)
connect_OBJECT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> IO ()
        action _ obj1 =
          failOnGError $
          makeNewGObject (GObject, objectUnref) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1')

connect_BOXED_BOXED__NONE :: 
  GObjectClass obj => SignalName ->
  (Ptr a' -> IO a) -> (Ptr b' -> IO b) -> 
  ConnectAfter -> obj ->
  (a -> b -> IO ()) ->
  IO (ConnectId obj)
connect_BOXED_BOXED__NONE signal boxedPre1 boxedPre2 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> Ptr () -> IO ()
        action _ box1 box2 =
          failOnGError $
          boxedPre2 (castPtr box2) >>= \box2' ->
          boxedPre1 (castPtr box1) >>= \box1' ->
          user box1' box2'

connect_BOOL_INT__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Bool -> Int -> IO ()) ->
  IO (ConnectId obj)
connect_BOOL_INT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Bool -> Int -> IO ()
        action _ bool1 int2 =
          failOnGError $
          user bool1 int2

connect_PTR_BOXED__NONE :: 
  GObjectClass obj => SignalName ->
  (Ptr b' -> IO b) -> 
  ConnectAfter -> obj ->
  (Ptr a -> b -> IO ()) ->
  IO (ConnectId obj)
connect_PTR_BOXED__NONE signal boxedPre2 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr () -> Ptr () -> IO ()
        action _ ptr1 box2 =
          failOnGError $
          boxedPre2 (castPtr box2) >>= \box2' ->
          user (castPtr ptr1) box2'

connect_OBJECT_PTR_BOXED__NONE :: 
  (GObjectClass a', GObjectClass obj) => SignalName ->
  (Ptr c' -> IO c) -> 
  ConnectAfter -> obj ->
  (a' -> Ptr b -> c -> IO ()) ->
  IO (ConnectId obj)
connect_OBJECT_PTR_BOXED__NONE signal boxedPre3 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Ptr () -> Ptr () -> IO ()
        action _ obj1 ptr2 box3 =
          failOnGError $
          boxedPre3 (castPtr box3) >>= \box3' ->
          makeNewGObject (GObject, objectUnref) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') (castPtr ptr2) box3'

connect_OBJECT_BOXED_OBJECT__BOOL :: 
  (GObjectClass a', GObjectClass c', GObjectClass obj) => SignalName ->
  (Ptr b' -> IO b) -> 
  ConnectAfter -> obj ->
  (a' -> b -> c' -> IO Bool) ->
  IO (ConnectId obj)
connect_OBJECT_BOXED_OBJECT__BOOL signal boxedPre2 after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Ptr () -> Ptr GObject -> IO Bool
        action _ obj1 box2 obj3 =
          failOnGError $
          makeNewGObject (GObject, objectUnref) (return obj3) >>= \obj3' ->
          boxedPre2 (castPtr box2) >>= \box2' ->
          makeNewGObject (GObject, objectUnref) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') box2' (unsafeCastGObject obj3')

