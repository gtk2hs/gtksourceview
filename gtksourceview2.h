#include "gtk2hs_macros.h"
#include <gtksourceview/gtksourcebuffer.h>
#if GTK_MAJOR_VERSION < 3
#include <gtksourceview/gtksourceiter.h>
#endif
#include <gtksourceview/gtksourcelanguage.h>
#include <gtksourceview/gtksourcelanguagemanager.h>
#include <gtksourceview/gtksourcestyle.h>
#include <gtksourceview/gtksourcestylescheme.h>
#include <gtksourceview/gtksourcestyleschememanager.h>
#include <gtksourceview/gtksourceview.h>
#include <gtksourceview/gtksourceview-typebuiltins.h>
#include <gtksourceview/gtksourceundomanager.h>
#include <gtksourceview/gtksourcecompletionitem.h>
#include <gtksourceview/gtksourcegutter.h>
#include <gtksourceview/gtksourcecompletionprovider.h>
#include <gtksourceview/gtksourcecompletionproposal.h>
#include <gtksourceview/gtksourcemark.h>
#include <gtksourceview/gtksourcecompletioninfo.h>
