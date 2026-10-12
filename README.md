# native-gen

A binding generator for the [jolt](https://jolt-lang.net) runtime: clang's
AST in, jolt namespaces out. Input is a header set and a manifest naming the
functions, structs, enums and constants wanted; output is a `.jolt`
namespace of `jolt.ffi/defcfn` forms, `ffi/layout` descriptors and
constants, the struct layouts measured by the C compiler (`gen/layouts.edn`),
a generated layout test, and a report of what was skipped and why. Object-like
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
                :main-opts ["-m" "native-gen.main" "gen/manifest.edn"]}}
```

and regenerates with `jolt -M:gen`. The generated files are committed like
a lockfile, so a regeneration after an upgrade shows the API change as a
diff. Needs clang on the path.

The tests run the header under `test/fixtures` through the generator, build
it into a shared library and call the generated bindings, so they need clang
too:

```sh
jolt test
```
