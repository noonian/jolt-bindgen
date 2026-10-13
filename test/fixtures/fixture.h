/* A header exercising every shape jolt-bindgen classifies. fixture.c implements
   it, so the generated bindings can be loaded and called by the tests. */
#ifndef FIXTURE_H
#define FIXTURE_H
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stddef.h>
#include <time.h>

/* object-like macros: the probe reports each one's value and type */
#define FX_VERSION 3
#define FX_NAME "fixture"
#define FX_SCALE 1.5
#define FX_BIG 18446744073709551615ULL
#define FX_FLAG (1u << 3)
#define FX_NEGATIVE (-7)
#define FX_TRUE ((_Bool)1)
#define FX_THIRD (1.0 / 3)               /* folded by the compiler, not a literal */
#define FX_ESCAPED "caf\u00e9 \"q\"\n" "end" /* escapes, UTF-8, concatenation */
#define FX_NULL ((void *)0)              /* a pointer: no jolt constant */
#define FX_TYPE_NAME FX_Point            /* not an expression: pruned */
#define FX_EXCLUDED 1                    /* listed in :macro-exclude */
#define FX_FUNCTION_LIKE(x) ((x) + 1)    /* function-like: never a candidate */
#define FXX_OTHER_PREFIX 9               /* outside :macro-prefixes */

typedef enum FX_Mode { FX_MODE_A, FX_MODE_B = 5, FX_MODE_C } FX_Mode;
typedef enum { FX_ANON_X = 1, FX_ANON_Y } FX_Anon;
enum { FX_LOOSE_A = 3, FX_LOOSE_B, FX_LOOSE_U = 1u << 4, FX_LOOSE_N }; /* never named: constants only */

typedef struct FX_Point { int32_t x; int32_t y; } FX_Point;
typedef struct FX_Rect { FX_Point origin; float size[2]; uint8_t tag; } FX_Rect;
typedef union FX_Value { int32_t i; double d; } FX_Value;
typedef struct FX_Tagged { int kind; FX_Value value; } FX_Tagged;
typedef struct FX_Bits { unsigned a : 3; unsigned b : 5; } FX_Bits;
typedef struct FX_Anonymous { int kind; union { int i; float f; }; } FX_Anonymous;
typedef struct FX_Named { int kind; union { int i; float f; } u; } FX_Named;
typedef struct FX_Matrix { float m[3][4]; } FX_Matrix;
typedef struct { float x, y; } FX_Vec2;  /* named only by its typedef */
struct FX_Plain { int a; double b; };    /* no typedef: named only by its tag */
typedef struct FX_Opaque FX_Opaque;
typedef bool (*FX_Callback)(void *userdata, FX_Point *p);
typedef void (*FX_Nested)(int code, void (*inner)(int, int), void *userdata);
typedef void (*FX_VaCallback)(const char *fmt, va_list ap);

extern int fx_counter;
static inline int fx_twice(int x) { return x * 2; }

int fx_add(int a, int b);
const char *fx_name(void);
char *fx_dup(const char *s);
const char *fx_const_name(const char *const s);
FX_Point fx_origin(void);
int fx_area(FX_Rect r);
void fx_set_callback(FX_Callback cb, void *userdata);
FX_Opaque *fx_open(const char *path);
void fx_close(FX_Opaque *o);
time_t fx_mtime(FX_Opaque *o);
size_t fx_len(const char *s);
void fx_log(const char *fmt, ...);
const char *fx_last_log(void);
void fx_vlog(const char *fmt, va_list ap);
int fx_value(FX_Value v);
int fx_tagged(FX_Tagged t);
int fx_bits(FX_Bits b);
void (*fx_handler(int sig))(int);
void fx_wait(int ms);
void fx_say(const char *s, int n);
void fx_log_blocking(const char *fmt, ...);
int fx_sum_points(FX_Point p, ...);
FX_Vec2 fx_vec2(float x, float y);
double fx_plain(struct FX_Plain p);
#endif
