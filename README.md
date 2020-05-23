# !!! Deprecated !!!

This library is deprecated, because it will no longer work as of PureScript v0.13.8. The reason for this is that it depended on an internal compiler API, namely the format of the externs.json files. As of v0.13.8, the compiler no longer writes these files. I do not intend to update it any longer, because I believe it would be better for the compiler to take responsibility for the feature which it enabled (namely, typechecking the entry points of programs). See https://github.com/purescript/purescript/issues/3621

# purescript-externs-check

This library gives you a way to reach into an externs file — a file created by
the PureScript compiler and placed alongside the CommonJS output — and check if
a given value exported by a given module is suitable for use as a program's
entry point.

Docs are [on Pursuit](https://pursuit.purescript.org/packages/purescript-externs-check/).
