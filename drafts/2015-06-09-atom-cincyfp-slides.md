---
title: Presentation at CincyFP
date: June 9, 2015
author: Chris Hodapp
tags: haskell, atom
---

*Later note: This post is back-dated because while I'd put the slides
 in the repository already, I had not actually linked to them or said
 anything about them.*

The fine people at [CincyFP](https://cincyfp.wordpress.com/), the
local functional programming group, requested that I give a
presentation on some of the embedded Haskell stuff I was doing, and I
did this at the [June meeting][cincyfp-announcement].

Slides from the presentation are up [here][slides-local] (and
[Markdown source][slides-markdown-local]; or look
[in the repo][slides-repo]).  CincyFP seems to have mostly Clojure
aficionados, so in terms of Haskell-heavy details I kept this
presentation as a "part 1" to some possible future presentations.
(Actually, not even that, since the title was "EDSLs &
Metaprogramming" and I barely scratched the surface of that.)

I start walking through a Atom example at slide 9.  While I live-coded
this at the presentation, I also put my example code online
[here][slides-code] (and [StringEmbed.hs][slides-stringembed]).

In trying to explain the different ways Haskell might be used on
embedded systems (around slide 2 or so), I refer to similar categories
as on the [Links][links] page.  I find myself having to explain these
distinctions quite often, so I am going to try to give some more
detail here.

Those categories are:

1. Full compilation, reduced runtime approach
2. Static analysis approach
3. Compiled EDSL approach

As far as I know, I made these categories up.  If anyone happens to
know a more established classification, or anyone who wrote about it
first, please tell me.  Also, I'm excluding things like Cryptol and
Idris that only incidentally involve Haskell.

# Full Compilation, Reduced Runtime

This compiles Haskell code to run directly on an embedded target.  The
[Compiling to Embedded Targets][links-full-compile] section of the
Links page is concerned particularly with this.

I'm aware of few successful examples of this (depending on what one
means by 'embedded'), as the nature of the Haskell language brings
some challenges.  Particularly, one must make the Haskell runtime fit
on the target and make the garbage collection behave sanely.

Ajhc ([https://github.com/ajhc/ajhc](https://github.com/ajhc/ajhc)), a
JHC-derived compiler from Kiwamu Okabe of METASEPI, is the only
example of this I found - it could compile and execute on ARM
Cortex-M3/M4.  His subsequent switch to the ATS language may be a
hint.

# Static Analysis

This uses an existing compiler for certain stages (such as the parsing
and type-checking), but a custom back-end to actually produce code.
This may adapt or disallow certain constructs.

GHC readily accomodates this by allowing developers to invoke GHC
functionality, from Haskell, as a library.
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

[cincyfp-announcement]: https://cincyfp.wordpress.com/2015/06/04/june-meeting-4/
[slides-local]: ../slides/20150609_CincyFP/Slides.html
[slides-markdown-local]: ../slides/20150609_CincyFP/Slides.md
[slides-repo]: https://github.com/HaskellEmbedded/HaskellEmbedded.github.io/tree/master/slides/20150609_CincyFP
[slides-code]: ../slides/20150609_CincyFP/Example.hs
[slides-stringembed]: ../slides/20150609_CincyFP/StringEmbed.hs
[links]: ../pages/links.html
[links-full-compile]: ../pages/links.html#compiling-to-embedded-targets
[links-fpga]: ../pages/links.html#compiling-for-fpgaasic
[links-circuit]: ../pages/links.html#circuit-design-edsls
[links-codegen]: ../pages/links.html#code-generation-edsls
