---
title: Scraps of other posts
date: 2015-05-09
author: Chris Hodapp
---

Ion to-do
====
Ion post to-do list
- I'd like Ion to be on Hackage. Does Ion require latest Ivory, or
does the Hackage version work fine?  If Hackage version is fine, then
I need not touch Ivory.  If newer is needed, then perhaps I should get
Ivory into NixOS.
- How do I get a nix specification into 'official' channels?
- It might be good to get both Ion and Ivory into NixOS.

Ivory thoughts
====

What would I aim to change in Ivory?
- Separate procedure's interface and implementation.
- Make a cleaner way to get at multiple representations of the same
thing.  For instance: 16-bit integer == 2 x uint8\_t == uint8\_t*
- Clean up some of the type trickery any way possible!  It wants to be
replaced with dependent types.
- Compatibility outside of GHC?
- Remove all the half-assed, ill-explained kludges.

I don't foresee maintenance happening on it, or further development,
unless I do it myself or push hard for it.

My realistic options:
- Re-implement more cleanly in Haskell.
- Re-implement more cleanly in something with dependent types, like
Idris.
- Fork it and fix it in ways that may not be palatable to the
maintainers - and maintain it myself (or with HaskellEmbedded).

My complaint is all the unfinished code there that no one who
understands has made a priority to finish or support, including the
original authors.

My aim is that the package itself, or something implementing similar
functionality, be organized and compatible enough that it may survive
in the future in a useful form.

Ideas
====
Some ideas:

- What my experiences were prior to this
- My results with Ivory
- More specific Ivory examples?  I could probably create a simplified example.
- Atom & concurrency - some examples here would be most helpful.
- What about the Ruby creator quote that I quite like?
- For Ivory: What I ran into with C forcing a coupling between
  compilation unit and local representation.

Stuff that might be out of the scope of this blog:

- Why did I like Ruby?
- Why did I like Python?
- What did I find interesting about Lisp?

Links to cover:

- http://techoverflow.net/blog/2014/07/28/using-quasiquotation-for-more-readable-atom-code/
- http://techoverflow.net/blog/2014/07/28/yet-another-atom-arduino-blinker/

Big Gaping Holes in This Example
====
Things I still have not touched:

1. Phases (in any useful sense)
2. Probes
3. Assertions
4. Coverage checking
5. What the Atom compiler outputs
6. What generated code looks like
7. Periods overriding later periods
8. The practical notion that you can apply periods and phases to
   entire sub-rules if you like


Copilot scraps
====

TODO below:
 - Links to Copilot calls
 - Functional Roboticist slides

Copilot and Atom share some similarities (besides the fact that
Copilot can use Atom for code generation).  Both are synchronous,
realtime languages.

As some of the posts on Atom say, Atom's basic unit is a *rule* which
involves some atomic operation.  That rule might have restrictions
such as being active only at certain clock ticks or under certain
conditions.

Copilot's basic unit is a *stream*.  Streams have a value at every
clock tick.  That value might come from some built-in streams (such as
constant values or clocks), from sampling an external variable or
function, or from operations on existing streams (such as adding,
logical AND, or referring to past values).  *Triggers* are the
mechanism for calling anything external.

Copilot also has some decent papers written about it, and it has two
very helpful features that are not present in Atom: pretty-printing
the specification ('prettyPrint'), and interpreting the specification
('interpret') for a given number of cycles.


Abstract scraps
====

I should explore more the notion of 'lifting' a lower-level language
up into Haskell, enabling Haskell as a template, macro, or
metaprogramming language.  (I'm not sure which term fits best there.)

The paper
[Coq: The world’s best macro assembler?](http://research.microsoft.com/en-us/um/people/nick/coqasm.pdf)
uses both 'metaprogramming' and 'macro' to refer to this.  However,
the terms may differ based on the level of the language.  Ivory, for
instance, lifts a representation of the C language up into Haskell,
and it generates C.  Atom generates C, but what it provides in Haskell
is a representation of a more constrained abstraction, so it is not
simply a matter of saying that it lifts the lower-level language up to
the level of Haskell.

Ivory scraps
====

One thing I see is that Ivory has embedded a "dumber" type system into
Haskell's.  Some types have a runtime meaning in Haskell, and those
have a runtime meaning in C in certain cases.  Those cases include
most numeric values, some lists of the same, and strings.

It doesn't include functions, tuples, anything polymorphic, algebraic
data types, records, and really any inductive type (except numeric
lists).

But, some types with a runtime meaning in Haskell also have a
compile-time meaning in C.  That is, C code is a first-class value, as
are C functions, and, to an extent, C files/modules.

Is the confusion/clumsiness because of the similarities to Haskell
that are limited?
