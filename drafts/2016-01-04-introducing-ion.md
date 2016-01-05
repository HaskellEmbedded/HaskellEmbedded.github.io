---
title: Introducing Ion
date: January 4, 2016
author: Chris Hodapp
tags: haskell
---

To-do:

- Get Ion onto Hackage.
- Get a Nix build into Ion and maybe Ivory.
- Write a post on Ivory?
- Move the big paragraph in the first section to an appendix or
something?

Background: Atom & Ivory {#background}
====

Earlier this year, I wrote a [few](./2015-02-17-atom-examples.html)
[posts](./2015-02-20-atom-part-2-probes.lhs) on [Atom][].  Remember
Atom?  If not, those posts might give some useful background.

At [some work](./2015-02-06-how-i-got-here.md) at my (former) job, I
was already using Atom in conjunction with [Ivory][], but those two
libraries really aren't made for interfacing with each other.  Atom
predates Ivory, but they both model certain features of the C
language, and as a result have many near-identical-but-incompatible
constructs.

As a result, I was resorting to hacks like giving Atom and Ivory
variables the same C name and relying on some incidental details of
how to refer to functions.  This works, but it is ugly and
error-prone, and it also bypasses proper typing.  If I mismatch the
names, the C code will probably fail to build.  If I mismatch the
types (for instance, my Atom variable is a `uint32_t` by way of
[V Word32][atom_val] and my Ivory variable is a `int16_t` by way of
[Sint16][]), the generated C code may contain some subtle errors.  In
either case, Haskell sees no problems in type-checking, because I'm
only coincidentally coupling the two variables via the generated C
code.

Sometime after this, I decided to re-implement Atom's functionality
in a more Ivory-friendly way.  I looked around in the Atom source code
first with the aim of adding an Ivory backend to it, however, I
quickly gave up on this.  The internals were a bit too dense for me to
follow, so I quickly gave up on this.

Ion
====

Here, then, is where I started writing the [Ion][] library from
scratch.  The name 'Ion' is a pun that's a reference to 'Atom' and
meaning loosely that while Atom is more standalone, Ion exists bonded
to another library, Ivory.

For the most part, I liked the way that Atom worked, and I wanted Ion
to behave very similarly.

[Ivory]: https://github.com/GaloisInc/ivory
[Atom]: https://hackage.haskell.org/package/atom
[atom_val]: http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Expressions.html#t:V
[Sint16]: https://hackage.haskell.org/package/ivory-0.1.0.0/docs/Ivory-Language.html#t:Sint16
[Ion]: https://github.com/HaskellEmbedded/ion
