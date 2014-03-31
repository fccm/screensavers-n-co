/* ocamlsaver, Copyright (c) 2010 Florent Monnier <blue_prawn@linux-nantes.org>
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.  No representations are made about the suitability of this
 * software for any purpose.  It is provided "as is" without express or 
 * implied warranty.
 */

#define DEFAULTS    "*delay:        30000       \n" \
                    "*count:        30          \n" \
                    "*showFPS:      False       \n" \
                    "*wireframe:    False       \n" \

#define refresh_mlsave 0
#define release_mlsave 0
#undef countof
#define countof(x) (sizeof((x))/sizeof((*x)))

#include "xlockmore.h"
#include "colors.h"
#include "tube.h"
#include "rotator.h"
#include "gltrackball.h"
#include <ctype.h>

#include <caml/mlvalues.h>
#include <caml/callback.h>


#ifdef USE_GL /* whole file */

#define DEF_SPEED       "0.05"

typedef struct {
  GLXContext *glx_context;

} mlsave_state;

static mlsave_state *bps = NULL;

static GLfloat speed;

static XrmOptionDescRec opts[] = {
  { "-speed",  ".speed",  XrmoptionSepArg, 0 },
};

static argtype vars[] = {
  {&speed,     "speed",  "Speed",  DEF_SPEED,  t_Float},
};

ENTRYPOINT ModeSpecOpt mlsave_opts = {countof(opts), opts, countof(vars), vars, NULL};


static void init_closure(int num_screens, int screen, int width, int height) {
    static value * closure_f = NULL;
    value args[4];
    if (closure_f == NULL)
        closure_f = caml_named_value("init_callback");
    args[0] = Val_int(num_screens);
    args[1] = Val_int(screen);
    args[2] = Val_int(width);
    args[3] = Val_int(height);
    caml_callbackN(*closure_f, 4, args);
}

static void draw_closure(int screen) {
  static value * closure_f = NULL;
  if (closure_f == NULL)
    closure_f = caml_named_value("draw_callback");
  caml_callback(*closure_f, Val_int(screen));
}

static void reshape_closure(int screen, int width, int height) {
  static value * closure_f = NULL;
  if (closure_f == NULL)
    closure_f = caml_named_value("reshape_callback");
  caml_callback3(*closure_f, Val_int(screen), Val_int(width), Val_int(height));
}

static int event_closure(int screen, char key) {
  static value * closure_f = NULL;
  value ret;
  if (closure_f == NULL)
    closure_f = caml_named_value("event_callback");
  ret = caml_callback2(*closure_f, Val_int(screen), Val_int(key));
  return Int_val(ret);
}


ENTRYPOINT void
reshape_mlsave (ModeInfo *mi, int width, int height)
{
  reshape_closure(MI_SCREEN(mi), width, height);
}


ENTRYPOINT Bool
mlsave_handle_event (ModeInfo *mi, XEvent *ev)
{
  /* mlsave_state *bp = &bps[MI_SCREEN(mi)]; */

  if (ev->xany.type == KeyPress) {
    return event_closure(MI_SCREEN(mi), XKeycodeToKeysym(mi->dpy, ev->xkey.keycode, 0));
  }

  return False;
}


ENTRYPOINT void 
init_mlsave (ModeInfo *mi)
{
  mlsave_state *bp;
  /* int wire = MI_IS_WIREFRAME(mi); */

  if (!bps) {
    bps = (mlsave_state *)
      calloc (MI_NUM_SCREENS(mi), sizeof(mlsave_state));
    if (!bps) {
      fprintf(stderr, "%s: out of memory\n", progname);
      exit(1);
    }
  }

  bp = &bps[MI_SCREEN(mi)];
  bp->glx_context = init_GL(mi);

  {
    char * argv[] = { "", NULL };
    caml_main( argv );
  }
  init_closure(MI_NUM_SCREENS(mi), MI_SCREEN(mi), MI_WIDTH(mi), MI_HEIGHT(mi));

  reshape_mlsave (mi, MI_WIDTH(mi), MI_HEIGHT(mi));
}


ENTRYPOINT void
draw_mlsave (ModeInfo *mi)
{
  mlsave_state *bp = &bps[MI_SCREEN(mi)];
  Display *dpy = MI_DISPLAY(mi);
  Window window = MI_WINDOW(mi);

  if (!bp->glx_context)
    return;

  glXMakeCurrent(dpy, window, *(bp->glx_context));

  draw_closure(MI_SCREEN(mi));

  if (mi->fps_p) do_fps (mi);
  glFinish();
  glXSwapBuffers(dpy, window);
}

XSCREENSAVER_MODULE_2 ("OCamlSaver", ocamlsaver, mlsave)

#endif /* USE_GL */
