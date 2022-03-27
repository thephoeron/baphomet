# BAPHOMET

Extensible definition forms supporting packages-as-types convention for Common Lisp.

## History

The `BAPHOMET` library began life as a fork of [DEMACS][], went through a few
name changes, took inspiration from [DEFINER][] and [CL-DEF][], then got
hoodwinked into adopting the packages-as-types convention for the Blackfire and
Quicksilver projects.

[DEMACS]: https://github.com/vy/demacs/
[DEFINER]: http://common-lisp.net/project/cl-def/
[CL-DEF]: http://common-lisp.net/project/definer/

### 03-27-2022

The present codebase was in the midst of yet another massive rewrite, so the
documentation has been excluded from the present code-dump due to its appalling
inapplicability.

The current code-base doesn't compile and load, but I hope to get it working for
the next Quicklisp release.

I've excluded the following type-categories from this code-dump, because they
depend on or are dependencies for unreleased Black Brane software:

- primitives
- environments
- namespaces
- readtables
- interfaces
- sequences
- categories
- domains
- models
- analogues
- metamachines
- machines

Some code was extracted from `BAPHOMET` and moved to the Quicksilver quantum
compiler suite, but it will be returned to this library because it's needed here
as part of the support for the packages-as-types convention.

Additionally, in the process of this rewrite the code-base got a little
disorganized:

- macro-definers are in `definition-forms/functions.lisp`
- first-class evaluation operators are in `types.lisp`
- `def:fexpr` is in `canonical-forms.lisp`
- the parser module doesn't follow `BAPHOMET` definer syntax
- the `BAPHOMET` and `DEF` packages still export symbols that no longer exist in
  this code-dump

## Authors

- @thephoeron
- @vy

## License

Copyright &copy; 2008&ndash;2022, the Authors

Released under the MIT License. See [LICENSE](./LICENSE) for more information.
