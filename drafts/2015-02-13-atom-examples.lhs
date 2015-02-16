---
title: Examples for the Atom library (DRAFT)
author: Chris Hodapp
---

In the [last post](./2015-02-06-how-i-got-here.html) I talked briefly about [Atom](http://hackage.haskell.org/package/atom), a Haskell EDSL from [Tom Hawkins](http://tomahawkins.org/) for hard realtime embedded software. I hope to go into a little more detail here.

# Background
As that post mentioned, I was running into problems handling concurrency in software for my embedded target (a [Nordic nRF51822](https://www.nordicsemi.com/eng/Products/Bluetooth-Smart-Bluetooth-low-energy/nRF51822)). My embedded software had operations throughout that required some fairly specific timing, for instance:

1. Activate some circuit.
2. Wait *at least* 300 milliseconds for that circuit to settle.
3. Trigger the circuit to send a pulse.
4. Wait *exactly* 145 microseconds, and set circuit to receive mode.
5. Wait *at least* a millisecond.
6. Send a command over SPI to trigger a reading.
7. Wait *at most* 1 millisecond for an acknowledgement.
8. Repeat 6 and 7 to read the contents of 3 or 4 other registers.

This was simple enough, except that the processor had to share a dozen similar processes simultaneously. The delay function could delay precisely, but by busy-looping, blocking other execution and wasting power, a precious resource here. Timer callbacks and state machines could share execution delay for longer periods, but at the cost of extra complexity.

I looked at a variety of solutions which implemented schedulers that ran directly on the target, but nearly all of them appeared to either interfere with Nordic's firmware or require too extensive of an implementation. The one that I finally could port, [cocoOS](http://www.cocoos.net/intro.html), had the rather show-stopping issue that its primitives were simply using more resources than I had available.

# Introduction to Atom

At this point I started looking at Atom. Its documentation is a bit lacking, and with no updates on its Hackage page in 2 years, I expect no change in that - which is a shame, because this is substantial industry work that was open-sourced. I'm slowly understanding it from what I can find, mainly:

- Hackage documentation:
    - [Language.Atom.Code](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Code.html)
    - [Language.Atom.Common](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Common.html)
    - [Language.Atom.Example](http://hackage.haskell.org/package/atom-1.0.12/docs/src/Language-Atom-Example.html)
    - [Language.Atom.Expressions](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Expressions.html)
    - [Language.Atom.Language](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html)
- some slides from creator Tom Hawkins in 2008, [Controlling Hybrid Vehicles with Haskell](http://cufp.galois.com/2008/slides/HawkinsTom.pdf). Some names in the example code have since changed, such as `atom` replacing `system`.
- a more formal example from Lee Pike, [An Atomic Fibonacci Server: Exploring the Atom (Haskell) DSL](https://leepike.wordpress.com/2009/05/05/an-atomic-fibonacci-server-exploring-the-atom-haskell-dsl/). His links to code examples appear to be down, but the Wayback Machine has old copies.
- a 2009 blog post from Don Stewart, [Atom : a domain specific language for hard realtime applications](https://archhaskell.wordpress.com/2009/08/01/atom-a-domain-specific-language-for-hard-realtime-applications/)
- two now-offline blog posts from John Van Enk at blog.sw17ch.com, [Atom & Arduino : Some Hacking (pt. 1)](https://web.archive.org/web/20110812162216/http://blog.sw17ch.com/wordpress/?p=84) and [Atom & Arduino : First Program (pt. 2)](https://web.archive.org/web/20110812162107/http://blog.sw17ch.com/wordpress/?p=111)
- [atom-msp430](https://github.com/eightyeight/atom-msp430), 3rd-party code for interfacing with MSP430 microcontrollers.

Atom's approach is a bit different than anything I'd mentioned in the prior section. As Hawkins' slides mention at page 12, Atom's compiler handles scheduling and synchronization, avoiding the need for locks, semaphores, or any kind of scheduling at run-time. As a Haskell EDSL, it also moves the abstraction up into Haskell rather than trying to make abstractions accessible from C, and that is not a trivial benefit.

Lee Pike in his link refers to Atom as a [synchronous language](https://en.wikipedia.org/wiki/Synchronous_programming_language): one specifies rules that fire on clock ticks, and state changes are atomic.

# Examples

This file [2015-02-13-atom-examples.lhs](2015-02-13-atom-examples.lhs) is a Literate Haskell file, so it is runnable.

# References
## Slides
1. Hawkins, T. (2008). Controlling Hybrid Vehicles with Haskell.  [http://cufp.galois.com/2008/slides/HawkinsTom.pdf](http://cufp.galois.com/2008/slides/HawkinsTom.pdf).

## Websites
1. atom: A DSL for embedded hard realtime applications. [http://hackage.haskell.org/package/atom](http://hackage.haskell.org/package/atom) and [https://github.com/tomahawkins/atom](https://github.com/tomahawkins/atom).
2. An Atomic Fibonacci Server: Exploring the Atom (Haskell) DSL. [https://leepike.wordpress.com/2009/05/05/an-atomic-fibonacci-server-exploring-the-atom-haskell-dsl/](https://leepike.wordpress.com/2009/05/05/an-atomic-fibonacci-server-exploring-the-atom-haskell-dsl/).
