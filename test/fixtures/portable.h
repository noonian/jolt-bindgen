/* A header that needs no system header of its target, so any target can be
   extracted from any machine. What it declares differs by platform in each
   of the ways a binding can: a type, a value, a layout and a declaration. */
#ifndef PORTABLE_H
#define PORTABLE_H
#include <stdint.h>
#include <stddef.h>

#ifdef _WIN32
#define PT_CALL __cdecl
typedef long long pt_off;
#define PT_SEPARATOR "\\"
int pt_windows_only(void);
#else
#define PT_CALL
typedef long pt_off;
#define PT_SEPARATOR "/"
#endif
#define PT_VERSION 2

typedef struct PT_Entry { char tag; long size; void *next; } PT_Entry;
typedef void (PT_CALL *PT_Visit)(const PT_Entry *entry, void *userdata);

pt_off pt_tell(void *file);
size_t pt_count(const PT_Entry *entry);
#endif
