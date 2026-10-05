#include "fixture.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct FX_Opaque { int fd; };
static char last_log[256];
static FX_Callback g_cb;
static void *g_ud;

int fx_add(int a, int b) { return a + b; }
const char *fx_name(void) { return "fixture"; }
char *fx_dup(const char *s) { return strdup(s); }
const char *fx_const_name(const char *const s) { return s; }
FX_Point fx_origin(void) { FX_Point p = {3, 4}; return p; }
int fx_area(FX_Rect r) { return (int)(r.size[0] * r.size[1]) + r.origin.x + r.tag; }
void fx_set_callback(FX_Callback cb, void *userdata) { g_cb = cb; g_ud = userdata; }
FX_Opaque *fx_open(const char *path) { (void)path; FX_Opaque *o = malloc(sizeof *o); o->fd = 7; return o; }
void fx_close(FX_Opaque *o) { free(o); }
time_t fx_mtime(FX_Opaque *o) { return (time_t)o->fd * 1000; }
size_t fx_len(const char *s) { return strlen(s); }
void fx_log(const char *fmt, ...) { va_list ap; va_start(ap, fmt); vsnprintf(last_log, sizeof last_log, fmt, ap); va_end(ap); }
const char *fx_last_log(void) { return last_log; }
void fx_vlog(const char *fmt, va_list ap) { vsnprintf(last_log, sizeof last_log, fmt, ap); }
int fx_value(FX_Value v) { return v.i; }
int fx_tagged(FX_Tagged t) { return t.kind; }
int fx_bits(FX_Bits b) { return (int)(b.a + b.b); }
static void handler(int sig) { (void)sig; }
void (*fx_handler(int sig))(int) { (void)sig; return handler; }
void fx_wait(int ms) { (void)ms; }
void fx_say(const char *s, int n) { snprintf(last_log, sizeof last_log, "%s/%d", s, n); }
void fx_log_blocking(const char *fmt, ...) { (void)fmt; }
int fx_sum_points(FX_Point p, ...) { return p.x + p.y; }
