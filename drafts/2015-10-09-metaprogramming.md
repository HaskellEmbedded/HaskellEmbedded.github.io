---
title: (something)
date: October 9, 2015
author: Chris Hodapp
tags: haskell, atom
---

My [last post][] mentioned that some things need some better
explanation, because I'm always trying to re-explain and clarify.

This blog is devoted to the use of Haskell with embedded systems.
What does that even mean?  We interpret it in a pretty open-ended way.
Particularly, we see a couple broad categories (which the slides on
the last page, as well as our [Links][links] page, mirror):

1. *Full Compilation:* Compiling Haskell code to an embedded target.
2. *Limited Compilation:* Compiling some limited subset of Haskell
   code to an embedded target.
3. *Compiled EDSL:* Hosting in Haskell an EDSL and a compiler to an
   embedded target.

As far as I know, I made these categories up.  If anyone happens to
know a more established classification, or anyone who wrote about it
first, please tell me.

The distinction between these isn't rigorous (and I'll explain that
later where I can).  The commonality is that in all cases one uses
Haskell to express something (a program, a circuit, specifications,
call it what you will) for an embedded target.

I exclude things like Cryptol and Idris from this because - while
implemented in Haskell and used for embedded platforms - they are
different languages unto themselves.  We might arbitrarily drop that
distinction in the future if we feel like it.

# Full Compilation

This compiles Haskell code to run directly on an embedded target.  The
[Compiling to Embedded Targets][links-full-compile] section of the
Links page is concerned particularly with this.

The nature of the Haskell language brings some challenges.
Particularly, one must make the Haskell runtime fit on the target and
make the garbage collection behave sanely.

Ajhc ([https://github.com/ajhc/ajhc](https://github.com/ajhc/ajhc)), a
JHC-derived compiler from Kiwamu Okabe of METASEPI, is the only
example of this I found - it could compile and execute on ARM
Cortex-M3/M4.  Kiwamu seems to have written a lot on his experiences
with making Haskell run in this footprint.  His subsequent switch to
the ATS language may be a hint.

# Limited Compilation

This uses an existing compiler for certain stages (such as the parsing
and type-checking), but a custom back-end to actually produce code,
often with a lot of static analysis. This may adapt or disallow
certain constructs (for instance, floating-point, recursive functions,
recursive datatypes:
[CλaSH Unsupported Haskell features][clash-unsupported]).

GHC accomodates this by allowing developers to invoke GHC
functionality, from Haskell, [as a library][ghc-library].
([ghcjs][https://github.com/ghcjs/ghcjs] uses this, as far as I know.)

The [Compiling for FPGA/ASIC][links-fpga] section of the Links page has
a few examples of this.

# Compiled EDSL

The [Code Generation EDSLs][links-codegen] and
[Circuit Design EDSLs][links-circuit] sections of the Links page cover
the copious examples of this.  Atom, the topic of a few of my prior
posts, is in this category.

This category is the one I am most often having to explain. It uses an
EDSL (embedded domain-specific language) inside of Haskell to direct
the process of code generation to a lower-level
representation. (Otherwise called: *compiling*.)

Note that in this case, Haskell code never actually *runs* on the
embedded target.  Rather, it uses specifications in the EDSL to build
a representation of what *will* run there - in other words, a sort of
metaprogramming, using Haskell as a template language or macro
language.

To emphasize: The code that runs on the target is entirely decoupled
from the Haskell runtime.  The Haskell compiler here isn't compiling
anything for the target - it's compiling another compiler.

This is a limitation of one sort:

- Basically all notions of 'runtime' on the embedded target must be
  handled separately.  (Ivory works with this still, for instance
  with the Num typeclass, in some surprising ways.  More on that will
  follow in a future post!)
- This adds the confusion and complication of another stage (possibly
  more) to the process of bringing code/specifications to the embedded
  target.

It's also a benefit of another sort:

- Any Haskell environment compatible with the libraries in question
  should produce the same results (as far as the embedded target
  cares).
- That stage separation also adds a nice opportunity for static
  analysis and optimization.  (For instance, Copilot makes use of this
  to add an interpreter/simulator, SBV uses it to prove or disprove
  given properties about the code, and Atom uses it to verify some
  timing constraints.)

# Commonality

Lumping together these categories might seem like a stretch,
especially considering that the last category involves extra stages
and a shift in how one thinks about the software.

Ponder the following, though:

- A "normal" Haskell program interacts through what is sequenced in
  the `IO` monad (particularly, `main`).
- An Atom specification interacts through what is sequenced in the
  `Atom` monad (particularly, whichever ones you pass to the
  Atom compiler).
- An Ivory program interacts through what is sequenced in the `Ivory`
  monad (particularly, whichever ones you pass to the Ivory compiler).
- A [CλaSH](http://www.clash-lang.org/) description interacts through
  the `Signal` applicative (particularly, `topEntity`).

The trend should be clear: Spanning our three categories, Haskell can
be used in roughly the same way in each.  Ignoring the hand-waviness
of "declarative", in all cases Haskell expresses something
declaratively, and the only difference is whether the Haskell compiler
and runtime handles it, or whether some other compiler does.

[last post]: ../posts/2015-06-09-atom-cincyfp-slides.md
[links]: ../pages/links.html
[links-full-compile]: ../pages/links.html#compiling-to-embedded-targets
[links-fpga]: ../pages/links.html#compiling-for-fpgaasic
[links-circuit]: ../pages/links.html#circuit-design-edsls
[links-codegen]: ../pages/links.html#code-generation-edsls
[clash-unsupported]: http://hackage.haskell.org/package/clash-prelude-0.10/docs/CLaSH-Tutorial.html#unsupported
[ghc-library]: https://wiki.haskell.org/GHC/As_a_library
