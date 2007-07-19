#include "hsgstreamer.h"

void _hs_gst_object_lock (GstObject* obj)
{ GST_OBJECT_LOCK (obj); }

gboolean _hs_gst_object_trylock (GstObject* obj)
{ return GST_OBJECT_TRYLOCK (obj); }

void _hs_gst_object_unlock (GstObject* obj)
{ return GST_OBJECT_UNLOCK (obj); }

void _hs_gst_object_take_ownership (gpointer obj)
{
  GST_OBJECT_LOCK (obj);
  
  if (GST_OBJECT_IS_FLOATING (obj)) {
    GST_OBJECT_FLAG_UNSET (obj, GST_OBJECT_FLOATING);
  } else {
    gst_object_ref (obj);
  }
  
  GST_OBJECT_UNLOCK (obj);
}

GstMessageType _hs_gst_message_get_message_type (GstMessage *message)
{ return GST_MESSAGE_TYPE (message); }

void _hs_gst_structure_make_immutable (GstStructure *structure)
{
  static gint refcount = 2;
  gst_structure_set_parent_refcount (structure, &refcount);
}

gsize _hs_gst_segment_sizeof (void)
{
  return sizeof (GstSegment);
}
