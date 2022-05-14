# BAPHOMET

Extensible definition forms supporting packages-as-types convention for Common Lisp.

## About

Using the `BAPHOMET` library requires careful consideration as it may involve
significant rewrites to your code-base. It offers many advantages for writing
Common Lisp software, but demands sacrifice, discipline, and utter devotion in
exchange.

What you gain:

- static-typing syntax for Common Lisp
- extended type model
- consistent definition form syntax
- language-oriented programming support

What you lose:

- the packages-as-namespaces idiom
- clean and tidy interoperability with other Lisp libraries

Definer macros try to do the right thing, but at present the library doesn't
hijack package interning for symbols to force type-validation. That means you
can bypass the provided interface (whether accidentally or intentionally) to
intern symbols in the wrong type-packages, change the value of a symbol to an
invalid type for that package, and all sorts of other madness. However, if you
stick to the interface and follow the packages-as-types convention devoutly,
you shouldn't run into any trouble while I nitpick over the best way to enforce
type-validation on a package basis.

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

## Support

This library is built and tested on macOS with the following Lisp implementations:

- LispWorks 8.0.0 (64-bit)
- SBCL 2.2.4
- Clozure-CL 1.12.1
- GNU CLISP 2.49.92
- ECL 21.2.1
- ABCL 1.9.0

## Authors

- @thephoeron
- @vy

## License

Copyright &copy; 2008&ndash;2022, the Authors

Released under the MIT License. See [LICENSE](./LICENSE) for more information.
