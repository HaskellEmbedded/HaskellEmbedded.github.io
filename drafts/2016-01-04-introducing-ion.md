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

Last year, I wrote a [few](./2015-02-17-atom-examples.html)
[posts](./2015-02-20-atom-part-2-probes.lhs) on [Atom][].  Remember
Atom?  If not, those posts might give some useful background.

At [some work](./2015-02-06-how-i-got-here.md) at my (former) job, I
was already using Atom in conjunction with [Ivory][], but those two
libraries really weren't made for interfacing with each other.  Atom
predates Ivory, but they both model certain features of the C
language, and as a result have many near-identical-but-incompatible
constructs.  For some boring details on this, see the
[section on this hackery](#hackery).

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
to another library, Ivory.  (I should probably move away from chemistry
puns and leave that up to things like [Rust][] and [Redox][].)

For the most part, I liked the way that Atom worked, and I wanted Ion
to behave very similarly.  Particularly, I liked the way that I could
modularize a specification with the Atom monad, the way that specs
could 'inherit' schedule parameters, the single run-time report giving
the entire schedule of the system, and the checks that Atom's compiler
did to ensure that specifications were sensible.

Ion started here, but diverged somewhat later on.  I didn't manage to
match all the features that are in Atom (and I note some of this in
Ion's documentation), and I started down some other paths.

Async & CPS
====

In the application that was using Ion, I started integrating in some
support for network communication.  This involved many operations of
transmitting a command over a UART, waiting for a reply sometime in
the future which contained the result of that command.  Or, maybe it
didn't - maybe it just contained some minor error, and the command
should be retried.  Or, maybe it was a fatal error, and the only thing
left to do was try to close down the connections, power off the modem,
and power off the UART.  Or, maybe the reply was just total garbage
from the UART.  Or, maybe something left the modem in a weird state,
and it sent no reply at all.

The world of rigid, deterministic timing didn't really have a place
for this sort of uncertainly-timed, non-deterministic, divergent
behavior.  (Actually, I had tried my best to make some similar and
simpler procedures work in Atom.  I made specifications which ran with
the same rigid timing regardless of when operations actually finished,
and to make this reliable, I set that timing to be very slow, and had
parts of the specification disabled if earlier steps failed.  It
worked, but operations took up far more time than needed, and handling
anything more divergent than 'if this failed, don't run that' might be
very messy.)

This also is a bit tricky to handle in C in any context without
threads or coroutines.  It almost always will involve callbacks,
interrupts, or events - some scope starts an asynchronous operation
(e.g. triggering an ADC measurement), and the result comes in the form
of an interrupt handler or callback later being called.  That
callback/interrupt handler/event handler will have to run in a
separate scope - which means that any state that needs to make it
'across' to that handler cannot reside on the stack.  It must be
stored in some other form, and recovered at the handler. (I found out
at some point that this has been [described already][usenix2002], it
is called *stack ripping*, and it comes with event-driven
programming.)

That's annoying as-is, but in my case, I didn't even have a heap from
which to dynamically allocate, so the only remaining option was
static memory.

As a side note, Ivory does provide a nice [coroutines][]
implementation, but I ran into two issues with them: They put every
variable (whether 'live' across a suspend/yield or not) into static
memory, and they are not composable.  More on that later.

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
and cheating a little by writing the variable's name as `&foo` or
`*foo`.

As an aside, if I remember right, a fair number of the bugs discovered
in the code were a direct result of me bypassing the type system in
this manner.

[Ivory]: https://github.com/GaloisInc/ivory
[Atom]: https://hackage.haskell.org/package/atom
[atom_val]: http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Expressions.html#t:V
[Sint16]: https://hackage.haskell.org/package/ivory-0.1.0.0/docs/Ivory-Language.html#t:Sint16
[Ion]: https://github.com/HaskellEmbedded/ion
[usenix2002]: https://www.usenix.org/legacy/events/usenix02/full_papers/adyahowell/adyahowell_html/index.html
[Rust]: https://www.rust-lang.org/
[Redox]: http://www.redox-os.org/
[coroutines]: https://github.com/GaloisInc/ivory/blob/master/ivory/src/Ivory/Language/Coroutine.hs
