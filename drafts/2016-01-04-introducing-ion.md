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

Background: Atom & Ivory {#background}
====

Earlier this year, I wrote a [few](./2015-02-17-atom-examples.html)
[posts](./2015-02-20-atom-part-2-probes.lhs) on [Atom][].  Remember
Atom?  If not, those posts might give some useful background.

At [some work](./2015-02-06-how-i-got-here.md) at my (former) job, I
was already using Atom in conjunction with [Ivory][], but those two
libraries really weren't made for interfacing with each other.  Atom
predates Ivory, but they both model certain features of the C
language, and as a result have many near-identical-but-incompatible
constructs.  For some boring details on this, see the
[section on this hackery](#hackery) 

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
to behave very similarly.  Particularly, I liked the Atom monad

Appendix: Atom & Ivory hackery {#hackery}
====

Atom and Ivory both generate C code, and to that end, both express
many of the same features of the C programming language - very central
things like expressions, variables, conditionals, and the C type
system.  However, they have different purposes in mind (otherwise, why
would I be using both?) and were never really meant to interface with
each other.  The way they model those features then is identical or
similar, but incompatible.

However, I needed the two libraries to generate C code in the same
build, to be able to refer to Atom variables from Ivory (and vice
versa), and to have something in Ivory responsible for calling Atom's
main tick function.

I ended up resorting to hacks like giving Atom and Ivory variables the
same C name and relying on some incidental details of how to refer to
functions.  This worked, but it was ugly and error-prone, and it also
bypassed proper typing.  If I mismatched the names, the C code would
probably fail to build.  If I mismatched the types (for instance, my
Atom variable is a `uint32_t` by way of [V Word32][atom_val] and my
Ivory variable is a `int16_t` by way of [Sint16][]), the generated C
code might have had some subtle errors.  In either case, Haskell saw
no problems in type-checking, because I was only coincidentally
coupling the two variables via the generated C code.  This could be
particularly nasty when I was trying to match pointer types properly,
and fudging the variable's name as `&foo`.

As an aside, if I remember right, a fair number of the bugs discovered
in the code were a direct result of me bypassing the type system in
this manner.

[Ivory]: https://github.com/GaloisInc/ivory
[Atom]: https://hackage.haskell.org/package/atom
[atom_val]: http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Expressions.html#t:V
[Sint16]: https://hackage.haskell.org/package/ivory-0.1.0.0/docs/Ivory-Language.html#t:Sint16
[Ion]: https://github.com/HaskellEmbedded/ion
