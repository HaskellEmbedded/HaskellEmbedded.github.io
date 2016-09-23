---
title: Introducing Ion
date: September 22, 2016
author: Chris Hodapp
tags: haskell
---

To-do:

- Get Ion up to date with Ivory master and hackage version
- Get Ion onto Hackage.
- Get a Nix build into Ion and maybe Ivory (?)
- Get a real example of CPS.
- Get a real example of something with timing.
- Composition-of-CPS example.
- Grep for all TODO elsewhere here

Really Short Version
====

[Ion][] is a Haskell EDSL that I wrote for concurrent, realtime,
embedded programming, targeting the [Ivory][] EDSL.  It's on hackage.
It's rather experimental.

-- TODO: hackage link above

Background: Atom & Ivory {#background}
====

Last year, I wrote a [few](./2015-02-17-atom-examples.html)
[posts](./2015-02-20-atom-part-2-probes.lhs) on [Atom][].  Remember
Atom?  If not, those posts might give some useful background.

At [some work](./2015-02-06-how-i-got-here.md) at my former job, I was
already using Atom in conjunction with [Ivory][], but those two
libraries really weren't made for interfacing with each other.  Atom
predates Ivory, but they both model certain features of the C
language, and as a result have many near-identical-but-incompatible
constructs.  For some boring details on this, see the
[section on this hackery](#hackery).

Sometime after this, I decided to re-implement Atom's functionality in
a more Ivory-friendly way.  I looked around in the Atom source code
first with the aim of adding an Ivory backend to it, however, I
quickly gave up on this as the internals were a bit too dense for me
to follow.

This post is badly-overdue, and for that I apologize.  For more
information, track me down in
[#haskell-embedded](irc://chat.freenode.net/%23haskell-embedded) or
Ion's GitHub.

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

What is Ion?
----

Ion, in brief, is a Haskell EDSL for concurrent, realtime, embedded
programming.  It targets the [Ivory][] EDSL and is coupled closely
with it.

I made Ion to cover two main cases:
* Scheduling tasks ("tasks" loosely just meaning "little bits of
  restricted Ivory code") that needed to execute on very strict
  timing.
* Handling tasks that may need to call other tasks asynchronously, and
  ultimately work with a form of continuation-passing style.

I sort of gloss over the "why?" of the first part because it's almost
all of the same reasons of why Atom exists.  I try to answer the
"why?" of the second part, and to some extent the "how?", below.

Async & CPS
====

In the application that was using Ion, I started integrating in some
support for network communication via a SIM800 GSM modem.  This
involved many operations of transmitting a command over a UART,
waiting for a reply sometime in the future which contained the result
of that command.  Or, maybe it didn't - maybe it just contained some
minor error, and the command should be retried.  Or, maybe it was a
fatal error, and the only thing left to do was try to close down the
connections, power off the modem, and power off the UART.  Or, maybe
the reply was just total garbage from the UART.  Or, maybe something
left the modem in a weird state, and it sent no reply at all.

The world of rigid, deterministic timing didn't really have a place
for this sort of uncertainly-timed, non-deterministic, divergent
behavior (someone's probably codified this into a theorem or
something).  Actually, I had tried my best to make some similar and
simpler procedures work in Atom.  I made specifications which ran with
the same rigid timing regardless of when operations actually finished,
and to make this reliable, I set that timing to be very slow, and had
parts of the specification disabled if earlier steps failed.  It
worked, but operations took up far more time than needed, and handling
anything more divergent than 'if this failed, don't run that' might be
very messy.

This also is a bit tricky to handle in C in any context without
threads or coroutines.  It almost always will involve callbacks,
interrupts, or events - some scope starts an asynchronous operation
(e.g. triggering an ADC measurement), and the result comes in the form
of an interrupt handler or callback later being called.  That
callback/interrupt handler/event handler will have to run in a
separate scope - which means that any state that needs to make it
'across' to that handler cannot reside on the stack.  It must be
stored in some other form, and recovered at the handler. (I found out
at some point that this has been [described already][usenix2002]: it
is called *stack ripping*, and it comes with event-driven
programming.)

That's annoying as-is, but in my case, I didn't even have a heap from
which to dynamically allocate, so the only remaining option was
static memory.

As a side note, Ivory does provide a nice [coroutines][]
implementation, but I ran into two issues with them: They put every
variable (whether 'live' across a suspend/yield or not) into static
memory, and they were not composable.
[An appendix section](#coroutines) gives some more details on this.

I wanted coroutines I could parametrize over other coroutines
(higher-order coroutines?), which seemed to require something like a
coroutine whose 'resume' continuation and 'exit' continuation both
were reified rather than implicit.  I also wanted it to be in a form
that I could interface it with C APIs that wanted function pointers
for callbacks or "normal" functions for interrupts.

I do not have a reference on this, but from memory, one of Oleg
Kiselyov's papers defined a coroutine as something like, "two
continuations calling each other."  After thinking on this a bit, I
realized that coroutines weren't really the appropriate abstraction; I
needed something more general, perhaps like a continuation, because
ultimately what I was dealing with was
[continuation-passing style][cps], and indeed CPS can express other
patterns such as exceptions.  (After reading extensively about
[Control.Monad.Cont][cont], I concluded that I had less of an idea
than when I started on whether I could use `Cont`, `ContT`,
`MonadCont`, or `callCC` to achieve this.  I was leaning towards "no,"
but I still have no idea.)

From here, it was a couple basic abstractions... and then considerable
plumbing to get this into a form that could be practically used with
existing C APIs.  Ivory's procedures rather mimic C functions, and C
functions are rather lacking for things like partial application or
higher-order functions.

Examples
====

The examples here are all rather contrived.  All of the "real" work
that I did with Ion was proprietary code that I can't share, and even
if I could, it wouldn't build to an actual embedded target, except in
the context of the much larger [Shake][] build it was in and the
entire associated toolchain.

However, I've tried to create some representative examples to a
hypothetical C API.  See [Example.hs][] for
these examples in a more buildable form.

Scheduling
----

First, let's tell Ivory about some (nonexistent) C calls in
`something.h`, with corresponding C prototypes indicated above for
those who have no idea how to grok Ivory declarations:

```haskell
-- void foo(int16_t)
foo :: Def ('[Sint16] :-> ())
foo = importProc "foo" "something.h"

-- void bar(int32_t)
bar :: Def ('[Sint32] :-> ())
bar = importProc "bar" "something.h"

-- uint16_t get_value(int32_t)
get_value :: Def ('[Uint8] :-> Uint16)
get_value = importProc "get_value" "something.h"

-- bool get_flag(void)
get_flag :: Def ('[] :-> IBool)
get_flag = importProc "get_flag" "something.h"
```

Here's a top-level spec.  As with Atom, `period` defines what division
of a base rate all of its contents inherit - hence, `variousPhases`
(which I define later) runs at 1/100th of the base rate, unless that's
overridden.

```haskell
simpleSchedule :: Ion ()
simpleSchedule = ion "schedule" $ do
  
  period 100 $ do
    variousPhases

  cond ((>? 10) <$> call get_value) $ do
    ivoryEff $ comment "get_value() > 10"
    cond (call get_flag) $ do
      ivoryEff $ comment "get_value() > 10 && get_flag()"
```

The entire `cond` block is there to illustrate that parts of a spec
can be made conditional.  The argument to `cond` isn't exactly a
boolean, but rather, is an Ivory effect which returns a boolean.  One
denotes the Ivory effects to weave into all of this with `ivoryEff`,
and in this case, the code does the very boring effect of inserting a
C comment with `comment`.

Here is `variousPhases`:

```haskell
variousPhases :: Ion ()
variousPhases = do
    phase 1 $ ivoryEff $ do
      comment "period 100, phase 1"
      call_ foo
    phase 10 $ ion "optional_tag" $ ivoryEff $ do
      comment "period 100, phase 10"
      call_ bar
    disable $ phase 20 $ ivoryEff $ do
      comment "shouldn't even appear in code"
      call_ foo
      call_ bar
    delay 50 $ do
      p <- getSched
      ivoryEff $ do
        comment "Should be phase 100 + 50"
        comment ("Reported sched: " ++ show p)
      delay 10 $ ion "moreDelay" $ do
        p <- getSched
        ivoryEff $ do
          comment "Should be phase 100 + 50 + 10"
          comment ("Reported sched: " ++ show p)
      phase 1 $ do
        ivoryEff $ comment "Should override to phase 1"
    period 1000 $ do
      ivoryEff $ comment "Should override all other period"
```

This is full of `phase`, which behaves more like `exactPhase` from
Atom, specifying that its contents should execute at some specific
offset of the current period.  I don't yet have support for the exact
semantics of Atom's `phase`, which means something more like,
"schedule it then, at the earliest".

`delay` has no exact analogue in Atom, but it simply specifies that
something executes at some relative phase past the phase that was
inherited.  That is, if you nested a series of `delay 1`, they'd all
proceed one tick apart, starting at what phase they inherited.

If we run all this through Ion and Ivory with `ionCompile ivoryOpts
"simpleSchedule" simpleSchedule` then the below C code results:

```c
// module simpleSchedule Source:

#include "simpleSchedule.h"
uint8_t counter_schedule_0 = (uint8_t) 1U;
uint8_t counter_optional_tag_1 = (uint8_t) 10U;
uint8_t counter_schedule_2 = (uint8_t) 50U;
uint8_t counter_moreDelay_3 = (uint8_t) 60U;
uint8_t counter_schedule_4 = (uint8_t) 1U;
uint16_t counter_schedule_5 = (uint16_t) 0U;
uint8_t counter_schedule_6 = (uint8_t) 0U;
uint8_t counter_schedule_7 = (uint8_t) 0U;
void simpleSchedule(void)
{
    /* Auto-generated schedule entry procedure from Ion & Ivory */
    /* Path: schedule */
    if ((bool) ((uint8_t) 0U == counter_schedule_0)) {
        ion_schedule_0();
        counter_schedule_0 = (uint8_t) 99U;
    } else {
        counter_schedule_0 = (uint8_t) (counter_schedule_0 - (uint8_t) 1U);
    }
    /* Path: schedule.optional_tag */
    if ((bool) ((uint8_t) 0U == counter_optional_tag_1)) {
        ion_optional_tag_1();
        counter_optional_tag_1 = (uint8_t) 99U;
    } else {
        counter_optional_tag_1 = (uint8_t) (counter_optional_tag_1 - (uint8_t) 1U);
    }
    /* Path: schedule */
    if ((bool) ((uint8_t) 0U == counter_schedule_2)) {
        ion_schedule_2();
        counter_schedule_2 = (uint8_t) 99U;
    } else {
        counter_schedule_2 = (uint8_t) (counter_schedule_2 - (uint8_t) 1U);
    }
    /* Path: schedule.moreDelay */
    if ((bool) ((uint8_t) 0U == counter_moreDelay_3)) {
        ion_moreDelay_3();
        counter_moreDelay_3 = (uint8_t) 99U;
    } else {
        counter_moreDelay_3 = (uint8_t) (counter_moreDelay_3 - (uint8_t) 1U);
    }
    /* Path: schedule */
    if ((bool) ((uint8_t) 0U == counter_schedule_4)) {
        ion_schedule_4();
        counter_schedule_4 = (uint8_t) 99U;
    } else {
        counter_schedule_4 = (uint8_t) (counter_schedule_4 - (uint8_t) 1U);
    }
    /* Path: schedule */
    if ((bool) ((uint16_t) 0U == counter_schedule_5)) {
        ion_schedule_5();
        counter_schedule_5 = (uint16_t) 999U;
    } else {
        counter_schedule_5 = (uint16_t) (counter_schedule_5 - (uint16_t) 1U);
    }
    /* Path: schedule */
    if ((bool) ((uint8_t) 0U == counter_schedule_6)) {
        ion_schedule_6();
        counter_schedule_6 = (uint8_t) 0U;
    } else {
        counter_schedule_6 = (uint8_t) (counter_schedule_6 - (uint8_t) 1U);
    }
    /* Path: schedule */
    if ((bool) ((uint8_t) 0U == counter_schedule_7)) {
        ion_schedule_7();
        counter_schedule_7 = (uint8_t) 0U;
    } else {
        counter_schedule_7 = (uint8_t) (counter_schedule_7 - (uint8_t) 1U);
    }
}
void ion_schedule_0(void)
{
    /* Auto-generated schedule procedure from Ion & Ivory */
    /* Path: schedule */
    /* Phase: 1 */
    /* Period: 100 */
    /* Action has no conditions */
    /* period 100, phase 1 */
    foo();
}
void ion_optional_tag_1(void)
{
    /* Auto-generated schedule procedure from Ion & Ivory */
    /* Path: schedule.optional_tag */
    /* Phase: 10 */
    /* Period: 100 */
    /* Action has no conditions */
    /* period 100, phase 10 */
    bar();
}
void ion_schedule_2(void)
{
    /* Auto-generated schedule procedure from Ion & Ivory */
    /* Path: schedule */
    /* Phase: 50 */
    /* Period: 100 */
    /* Action has no conditions */
    /* Should be phase 100 + 50 */
    /* Reported sched: Schedule {schedId = 0, schedName = "schedule", schedPath = ["schedule"], schedPhase = 50, schedPeriod = 100, schedAction = [], schedCond = []} */
    ;
}
void ion_moreDelay_3(void)
{
    /* Auto-generated schedule procedure from Ion & Ivory */
    /* Path: schedule.moreDelay */
    /* Phase: 60 */
    /* Period: 100 */
    /* Action has no conditions */
    /* Should be phase 100 + 50 + 10 */
    /* Reported sched: Schedule {schedId = 0, schedName = "moreDelay", schedPath = ["schedule","moreDelay"], schedPhase = 60, schedPeriod = 100, schedAction = [], schedCond = []} */
    ;
}
void ion_schedule_4(void)
{
    /* Auto-generated schedule procedure from Ion & Ivory */
    /* Path: schedule */
    /* Phase: 1 */
    /* Period: 100 */
    /* Action has no conditions */
    /* Should override to phase 1 */
    ;
}
void ion_schedule_5(void)
{
    /* Auto-generated schedule procedure from Ion & Ivory */
    /* Path: schedule */
    /* Phase: 0 */
    /* Period: 1000 */
    /* Action has no conditions */
    /* Should override all other period */
    ;
}
void ion_schedule_6(void)
{
    /* Auto-generated schedule procedure from Ion & Ivory */
    /* Path: schedule */
    /* Phase: 0 */
    /* Period: 1 */
    /* Action has 1 conditions: */
    ;
    
    uint16_t n_r0 = get_value();
    
    if ((bool) (n_r0 > (uint16_t) 10U)) {
        /* get_value() > 10 */
        ;
    }
}
void ion_schedule_7(void)
{
    /* Auto-generated schedule procedure from Ion & Ivory */
    /* Path: schedule */
    /* Phase: 0 */
    /* Period: 1 */
    /* Action has 2 conditions: */
    ;
    
    bool n_r0 = get_flag();
    uint16_t n_r1 = get_value();
    
    if ((bool) (n_r0 && (bool) (n_r1 > (uint16_t) 10U))) {
        /* get_value() > 10 && get_flag() */
        ;
    }
}
```

and this header:
```c
// module simpleSchedule Header:

#include "ivory.h"
extern uint8_t counter_schedule_0;
extern uint8_t counter_optional_tag_1;
extern uint8_t counter_schedule_2;
extern uint8_t counter_moreDelay_3;
extern uint8_t counter_schedule_4;
extern uint16_t counter_schedule_5;
extern uint8_t counter_schedule_6;
extern uint8_t counter_schedule_7;
void simpleSchedule(void);
void ion_schedule_0(void);
void ion_optional_tag_1(void);
void ion_schedule_2(void);
void ion_moreDelay_3(void);
void ion_schedule_4(void);
void ion_schedule_5(void);
void ion_schedule_6(void);
void ion_schedule_7(void);
```

`simpleSchedule` is the most important of this: This is the main
scheduling function ("schedule entry procedure") that must be called
at the base rate through a timer, an interrupt, or something of the
sort for anything to work right.  Often this must be set up outside of
Ivory because Ivory really isn't interested in whatever C/ASM black
magic the timer requires. Also, the name of that function is what we
supplied to `ionCompile`.

It has also produced several variables, all of the `counter_` ones,
which are used for establishing the correct periods and phases.  Take
a look in `simpleSchedule` and the pattern should be clear (as well as
near-identical to how Atom does it, since I basically copied its
method): Each Ivory effect in a particular period/phase context turns
into a branch, a function, and a counter variable.  The counter
variable starts at the respective phase, and each branch is
responsible for resetting it to the respective period, decrementing
the counter, and calling the function that contains the Ivory effects.
Note also that `counter_schedule_5` uses a `uint16_t` - it corresponds
to the `period 1000` block, so a larger counter variable is needed to
fit that range. Note also in `ion_schedule_7` that nesting conditions
with `cond` leads to all of them applying (i.e. logical AND).

Read through the code in some more detail, and it should be clear how
specifications map and flatten out to generated C code.  The names
given in `ion` are for the most part just for documentation purposes;
Ion is perfectly content to generate unique C identifiers without
help.  Note that each point has a sort of "path" that led to it, and
that path determines how it is scheduled.

Timers
----

Here's a shorter example that incorporates Ion's resettable timers
(note the use of `mdo` so we may define the timer first; this will
need `{-# LANGUAGE RecursiveDo #-}`):

```haskell
exampleTimer :: Ion (Def ('[] ':-> ()))
exampleTimer = ion "timer" $ mdo

  -- Timer is initialized with a Uint16; procedure called at
  -- expiration is fixed at compile-time:
  timer1 <- period 1 $ timer (Proxy :: Proxy Uint16) expire

  -- Initialization procedure:
  init <- newProc $ body $ do
    -- Trigger the timer for 1000 ticks:
    startTimer timer1 1000
  
  expire <- newProc $ body $ do
    call_ printf "Timer expired!\r\n"

  return init
```

This makes use of the `Ion` monad to return the entry procedure, which
is required to start the timer counting in the first place.  Also, due
to some limitations, the timer's behavior upon expiration is fixed at
compile-time, though its countdown time is not (hence the 1000 we pass
to `startTimer`).  Rather than calling some external procedure when
the timer expires (as we did in the prior section), we use `newProc`
to define a procedure right there, and Ion takes care of giving it a
name and having Ivory include it.  To simplify things, we put the
timer inside of `period 1` so it counts at the base rate - but if, for
instance, the base rate were 1 millisecond, we might sensibly put the
timer inside period 1000 so that its countdowns are all in seconds.

The resultant C source is below:

```c
// module timer Source:

#include "timer.h"
uint16_t timer_0 = (uint16_t) 0U;
uint8_t counter_decr_0 = (uint8_t) 0U;
void timer_1(void)
{
    timer_0 = (uint16_t) 1000U;
}
void timer_2(void)
{
    printf("Timer expired!\r\n");
}
void timer(void)
{
    /* Auto-generated schedule entry procedure from Ion & Ivory */
    /* Path: timer.timer_0.decr */
    if ((bool) ((uint8_t) 0U == counter_decr_0)) {
        ion_decr_0();
        counter_decr_0 = (uint8_t) 0U;
    } else {
        counter_decr_0 = (uint8_t) (counter_decr_0 - (uint8_t) 1U);
    }
}
void ion_decr_0(void)
{
    /* Auto-generated schedule procedure from Ion & Ivory */
    /* Path: timer.timer_0.decr */
    /* Phase: 0 */
    /* Period: 1 */
    /* Action has no conditions */
    ;
    
    uint16_t n_deref0 = timer_0;
    
    if ((bool) ((uint16_t) 0U == n_deref0)) { } else {
        uint16_t n_cse1 = (uint16_t) (n_deref0 - (uint16_t) 1U);
        
        timer_0 = n_cse1;
        if ((bool) (n_cse1 > (uint16_t) 0U)) { } else {
            timer_2();
        }
    }
}
```

and the header:

```c
// module timer Header:

#include "ivory.h"
extern uint16_t timer_0;
extern uint8_t counter_decr_0;
void timer_1(void);
void timer_2(void);
void timer(void);
void ion_decr_0(void);
```

This should look similar to the last section, but with the addition
now of `timer_0`.  Our procedure with `newProc` was also turned into
the C function `timer_2`.

CPS
----


Conclusions
====

Ion helped me immensely with generating large amounts of C plumbing
whose behavior was very easy to reason about.  It cut both ways, as it
also made it trivial to very quickly occupy a lot of RAM, produce huge
binaries, and generate a lot of very complex code very quickly.

Much of the aforementioned plumbing was to bundle together the
boilerplate that Ivory requires - things like `incl` for every single
function that actually needs to be a C function by way of an Ivory
procedure.  In Haskell-world, one doesn't normally think of having to
explicitly declare each function, but Ivory inherits this C
limitation.  When Ion has to generate a lot of Ivory procedures and
variables (particularly when I'm using it to parametrize these
definitions over something), it has to accumulate all of this somehow
and get it to Ivory's compiler.

I tried to fix the bugs in Ion that pertained to correctness, but it
still has a lot of room for improvement when it comes to efficiency
and comprehensibility of the generated code for scheduling, and part
of that is because it errs on the side of paranoia.

I coupled together in the Ion monad the functionality for the
strictly-timed scheduling, and all of the continuating-passing
rigging.  To me, these felt like two pieces that should have been
separate, however, I wasn't able to find a satisfactory way to do
this.

*Appendix 1: Atom & Ivory hackery* {#hackery}
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

*Appendix 2: Limitations on Coroutines* {#coroutines}
====

Two pernicious limitation I ran into on Ivory's coroutines were an
inability to build up coroutines out of smaller parts, and an
inability to take the *yield* escape hatch that a coroutine provided
and pass it around like a first-class value.  This wasn't a slight
against them - they are coroutines, behaving like coroutines but
inheriting the C-derived limitations that Ivory purposely has, and I
was trying to make them behave like something else.

Consider coroutine A and coroutine B.  As far as control flow goes,
coroutine A can do a few things:
* return back to caller
* suspend itself with *yield*
* resume coroutine B (or call it in the first place)

*(It can't call itself, but that's incidental here; Ivory's coroutines
store a continuation in static memory, so only one can be "live" at
once.)*

However, returning and yielding don't have any meaningful first-class
form.  Coroutine A can't pass its own "return" to coroutine B, and let
B return a value to its caller, nor can it pass its own "yield"
elsewhere and let another context suspend it.

In other words: these coroutines don't provide their own continuations
or an "exit" continuation; that's all handled implicitly.

[Ivory]: https://github.com/GaloisInc/ivory
[Atom]: https://hackage.haskell.org/package/atom
[atom_val]: http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Expressions.html#t:V
[Sint16]: https://hackage.haskell.org/package/ivory-0.1.0.0/docs/Ivory-Language.html#t:Sint16
[Ion]: https://github.com/HaskellEmbedded/ion
[usenix2002]: https://www.usenix.org/legacy/events/usenix02/full_papers/adyahowell/adyahowell_html/index.html
[Rust]: https://www.rust-lang.org/
[Redox]: http://www.redox-os.org/
[coroutines]: https://github.com/GaloisInc/ivory/blob/master/ivory/src/Ivory/Language/Coroutine.hs
[cps]: https://en.wikipedia.org/wiki/Continuation-passing_style
[cont]: https://hackage.haskell.org/package/mtl/docs/Control-Monad-Cont.html
[Shake]: https://hackage.haskell.org/package/shake
[Example.hs]: https://github.com/HaskellEmbedded/ion/blob/master/src/Ivory/Language/Ion/Examples/Example.hs
