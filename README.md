# native-gen

A binding generator for the [jolt](https://jolt-lang.net) runtime: clang's
AST in, jolt namespaces out. Input is a header set and a manifest naming the
functions, structs, enums and constants wanted; output is a `.jolt`
namespace of `jolt.ffi/defcfn` forms, `ffi/layout` descriptors and
constants, a record of what clang saw for each target (`gen/targets/`), a
generated layout test, and a report of what was skipped and why. Object-like
macros and struct layouts are evaluated by generated C probes that clang
parses and never runs, so the compiler decides each constant's value and type
and each field's offset, for whatever target it is parsing for; a typedef
from another header (`time_t`) resolves to the type clang desugars it to;
functions listed as blocking are bound with
`:blocking` and their `const char *` parameters as pointers; variadic
functions are bound with the bare `:&` marker, unless they are blocking or
pass an aggregate by value, which jolt does not combine with it; a `va_list`
parameter, or a union passed by value (alone or inside a struct), has no
binding and is reported; records it cannot describe (bitfields, anonymous
unions) are left out and reported. A struct or enum named only by its typedef
takes that name, and an enum nothing names still gives its constants. A static
function, a global variable, a callback type with no jolt spelling and a
wanted macro that cannot be a constant are not bound, and each is in the
report with the reason.

It is the tool the sdl3, joltc-jlt and cimgui-jlt bindings are generated
with, and not part of any of them. A binding project uses it through an
alias:

```clojure
:aliases {:gen {:extra-deps {jlt-commons/native-gen {:local/root "../native-gen"}}
                :main-opts ["-m" "noonian.jolt.native-gen.main" "gen/manifest.edn"]}}
```

and regenerates with `jolt -M:gen`. The generated files are committed like
a lockfile, so a regeneration after an upgrade shows the API change as a
diff. Needs clang on the path.

## Targets

Generation has two stages. Extraction runs clang for one target and writes
what it saw to `gen/targets/<target>.edn`: the declarations as clang spells
them, the macros and the struct layouts it folded. Emission merges every
file in that directory into the namespaces and needs no compiler. A binding's
targets are the files there.

A run always extracts the machine it is on. `:targets` in the manifest names
others to extract from the same machine, which works when the headers need
no system header of the target's (`stdint.h` and `stddef.h` come with clang;
`stdio.h` and `time.h` do not), and such a file is marked `:cross? true`.
A target that cannot be parsed from here is named in the report, and is
extracted by running the generator on that platform and committing its
file; a file extracted on its own platform is never replaced by a view of
it from another. `--check` extracts the current machine afresh, compares
it with the file on record, and exits non-zero when they differ, which is
the job for a CI run on each platform:

```sh
jolt -M:gen --check
```

The targets are `darwin-arm64`, `darwin-x86_64`, `linux-x86_64`,
`linux-arm64`, `windows-x86_64` and `android-arm64`. `:include-dirs` may be a
map from a target, or from its operating system as a keyword, to its
directories.

With one target the namespaces are that target's definitions and nothing
else. With several, what the targets define alike is written once, and what
they define differently (a `time_t` that is a `long` here and a `long long`
there, a macro behind an `#ifdef`) is chosen when the namespace loads, by
`native-gen-target`; the report lists each under `:divergent`. A function
only some targets declare is written for all of them, since jolt binds a
function when it is called. The layout test checks the target it runs on.

Such a namespace refuses to load on a target nobody extracted: a binding
that was never checked there is a guess at an ABI. In an emergency
`NATIVE_GEN_TARGET=linux-x86_64` loads it as that target anyway, and the
layout test then says how far the guess is off.

The tests run the header under `test/fixtures` through the generator, build
it into a shared library and call the generated bindings, so they need clang
too:

```sh
jolt test
```
