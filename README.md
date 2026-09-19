# native-gen

A binding generator for the [jolt](https://jolt-lang.net) runtime: clang's
AST in, jolt namespaces out. Input is a header set and a manifest naming the
functions, structs, enums and constants wanted; output is a `.jolt`
namespace of `jolt.ffi/defcfn` forms, `ffi/layout` descriptors and
constants, the struct layouts measured by the C compiler (`gen/layouts.edn`),
a generated layout test, and a report of what was skipped and why. Object-like
macros are evaluated by a generated C probe, so the compiler decides each
constant's value and type; functions listed as blocking are bound with
`:blocking` and their `const char *` parameters as pointers; variadic
functions are skipped; records it cannot describe (bitfields, anonymous
unions) are left out and reported.

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

```sh
jolt -A:test -e "(require 'native-gen.json-test 'clojure.test) (clojure.test/run-tests 'native-gen.json-test)"
```
