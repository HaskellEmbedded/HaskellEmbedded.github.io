---
title: Embedding Haskell: Compilers, and compiling compilers
date: October 9, 2015
author: Chris Hodapp
tags: haskell, ramblings
---

My [last post][] mentioned that some things need some better
explanation, because I'm always trying to re-explain and clarify.

This blog is devoted to the use of Haskell with embedded systems.
What does that even mean? We see a couple broad categories (which the
slides on the last page, as well as our [Links][links] page, mirror):

1. *Full Compilation:* Compiling Haskell code to an embedded target.
2. *Limited Compilation:* Compiling some limited subset of Haskell
   code to an embedded target.
3. *Hosted EDSL & Compiler:* Hosting, in Haskell, an EDSL and a
   compiler to an embedded target.

As far as I know, I made these categories up.  If anyone happens to
know a more established classification, better names, or an example of
who wrote about it first, please tell me.

This might look like a lopsided, arbitrary grouping; it sort of is.
The commonality is that in all cases one uses Haskell to express
something (a program, a circuit, specifications, call it what you
will) for an embedded target.  More on that follows.

I exclude things like Cryptol and Idris from this because - while
implemented in Haskell and used for embedded platforms - they are
different languages unto themselves.  We might arbitrarily drop that
distinction in the future if we feel like it.

# Full Compilation

This is what normally comes to mind when people hear about using
Haskell with embedded systems - compiling the Haskell code to run
directly on an embedded target, bringing along the normal runtime with
it (plus whatever required bootstrapping and support).  The
[Compiling to Embedded Targets][links-full-compile] section of the
Links page is concerned particularly with this.

However, this actually appears to be pretty rare.  The nature of the
Haskell language brings some formidable challenges.  Particularly, one
must make the Haskell runtime fit on the target and make the garbage
collection and lazy evaluation behave in predictable and sane ways.

Ajhc ([https://github.com/ajhc/ajhc](https://github.com/ajhc/ajhc)), a
JHC-derived compiler from Kiwamu Okabe of METASEPI, is the only
example of this I found - it could compile and execute on ARM
Cortex-M3/M4.  Kiwamu has written a lot on his experiences with making
Haskell run in this footprint.  His subsequent switch to the ATS
language may be a hint.

[HaLVM][] from Galois might arguably fit in this category.

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

# Hosted EDSL & Compiler

The [Code Generation EDSLs][links-codegen] and
[Circuit Design EDSLs][links-circuit] sections of the Links page cover
the copious examples of this.  Atom, the topic of a few of my
[prior posts][atom-last-post], is in this category.

This category is the one I am most often having to explain.  It
typically uses an EDSL (embedded domain-specific language) inside of
Haskell to direct the process of code generation to a lower-level
representation.  This is otherwise called: *compiling*.

To emphasize: The code that runs on the target is *entirely decoupled
from the Haskell runtime*.  The Haskell compiler here isn't compiling
anything for the target - it's compiling another compiler and the
input to that compiler.  That input happens to be specifications of
what *will* run on the target.

This is a limitation of one sort:

- Basically all notions of 'runtime' on the embedded target must be
  handled separately.  (Ivory works with this still, for instance with
  the [Num][] typeclass, in some surprising ways.  More on that will
  follow in a future post!)
- This adds the confusion and complication of another stage (possibly
  multiple stages) to the process of bringing code/specifications to
  the embedded target.  This is why I use
  [Shake](http://shakebuild.com/).

It's also a benefit of another sort:

- Any Haskell environment compatible with the libraries in question
  should produce the same results (as far as the embedded target
  cares).  Its runtime does not matter, nor whether the environment
  has any knowledge of the architecture of the embedded target.
- That stage separation also adds a nice opportunity for static
  analysis and optimization.  For instance, Copilot makes use of this
  to add an interpreter/simulator, SBV uses it to prove or disprove
  given properties about the code, and Atom uses it to verify some
  timing constraints.

I said in the [last post][] that in this category Haskell takes on the
role of a metaprogramming or template language.  While this may be
true, I sort of ignored that it's less relevant, because it's the same
in all the other categories.

# Commonality

Lumping together these categories might seem like a stretch,
especially considering that the last category involves extra stages
and a shift in how one thinks about the software.

Ponder the following, though:

- A "normal" Haskell program interacts through what is sequenced in
  the famous [IO][io-monad] monad (particularly, the value called
  `main`).
- An Atom specification interacts through what is sequenced in the
  [Atom][] monad (particularly, whichever values one passes to the
  [Atom compiler][]).
- An Ivory program interacts through what is sequenced in the
  [Ivory eff][] and [Module][ivory-module] monads (particularly,
  whichever values one passes to the
  [Ivory compiler][ivory-compiler]).
- A [CλaSH](http://www.clash-lang.org/) description interacts
  through the [Signal][clash-signal] applicative (particularly, the
  value called [topEntity][clash-topentity]).

Is a trend clear?  (No, it's not monads.  [Signal][clash-signal] is
only applicative, and I suspect Lava behaves similarly.)

That list spans our three categories.  In each of them, one builds up
a program (in a very broad sense) simply by building up a value in
Haskell.  Beyond that, the only real differences are,

- the type of that value,
- what system handles it (the Haskell compiler and runtime, some
  other compiler and maybe runtime, or a combination thereof),
- and the eventual output (native binary, LLVM bitcode, C code, VHDL
  code, assembly language, input to a model checker, etc.).

Ignoring the vague nature of the term, "declarative," this relates
pretty directly to the *declarative* nature of Haskell programs.

Seen from this perspective, one is still *compiling Haskell* to run on
some embedded target.  The compilation just might continue outside of
the system's Haskell compiler, and the running might not involve its
runtime.

[atom compiler]: https://hackage.haskell.org/package/atom-1.0.13/docs/Language-Atom.html#g:2
[atom-last-post]: ../posts/2015-02-17-atom-examples.lhs
[atom]: https://hackage.haskell.org/package/atom-1.0.13/docs/Language-Atom.html#t:Atom
[clash-signal]: https://hackage.haskell.org/package/clash-prelude-0.10/docs/CLaSH-Signal.html#t:Signal
[clash-topentity]: https://hackage.haskell.org/package/clash-prelude-0.10/docs/CLaSH-Annotations-TopEntity.html
[clash-unsupported]: http://hackage.haskell.org/package/clash-prelude-0.10/docs/CLaSH-Tutorial.html#unsupported
[ghc-library]: https://wiki.haskell.org/GHC/As_a_library
[halvm]: https://github.com/galoisinc/halvm
[io-monad]: https://hackage.haskell.org/package/base/docs/System-IO.html
[ivory eff]: https://hackage.haskell.org/package/ivory-0.1.0.0/docs/Ivory-Language.html#t:Ivory
[ivory-compiler]: https://hackage.haskell.org/package/ivory-backend-c-0.1.0.1/docs/Ivory-Compile-C.html
[ivory-module]: https://hackage.haskell.org/package/ivory-0.1.0.0/docs/Ivory-Language.html#t:Module
[last post]: ../posts/2015-06-09-atom-cincyfp-slides.html
[links-circuit]: ../pages/links.html#circuit-design-edsls
[links-codegen]: ../pages/links.html#code-generation-edsls
[links-fpga]: ../pages/links.html#compiling-for-fpgaasic
[links-full-compile]: ../pages/links.html#compiling-to-embedded-targets
[links]: ../pages/links.html
[num]: https://hackage.haskell.org/package/base/docs/Prelude.html#t:Num
