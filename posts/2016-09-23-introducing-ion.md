---
title: Introducing Ion
date: September 23, 2016
author: Chris Hodapp
tags: haskell, ivory
---

Really Short Version
====

[Ion][] is a Haskell EDSL that I wrote for concurrent, realtime,
embedded programming, targeting the [Ivory][] EDSL.  I finally
released it to [hackage][ion_hackage].  It's still rather
experimental.

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
information, track me (hodapp) down in
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

- Scheduling tasks ("tasks" loosely just meaning "little bits of
  restricted Ivory code") that needed to execute on very strict
  timing.
- Using continuating-passing style to compose together asychronous
  tasks (particularly those that call around with callbacks).

I sort of gloss over the "why?" of the first part because it's almost
all of the same reasons of why Atom exists.  I try to answer the
"why?" of the second part, and to some extent the "how?", below.

Async & CPS
====

In the application that was using Ion, I started integrating in some
support for network communication via a SIM800 GSM modem.  This
involved many operations of transmitting a command over a UART,
waiting for a reply sometime in the future which contained the result
of that command - perhaps an HTTP payload or something.

Or, maybe it didn't - maybe it just contained some minor error, and
the command should be retried.

Or, maybe it was a fatal error, and the only thing left to do was try
to close down the connections, power off the modem, and power off the
UART.

Or, maybe the reply was just total garbage from the UART.

Or, maybe something left the modem in a weird state, and it sent no
reply at all because it's still waiting for us to do something.

The world of rigid, deterministic timing didn't really have a place
for this sort of uncertainly-timed, non-deterministic, divergent
behavior (someone's probably codified this into a theorem or
something).  Actually, I had tried my best to make some similar and
simpler procedures work in Atom.  I made specifications which ran with
the same rigid timing regardless of when operations actually finished,
and to make this reliable, I set that timing to be very slow, and had
parts of the specification disabled if earlier steps failed.  It
worked, but operations took up far more time than needed, and handling
anything more divergent than *if this fails, don't run that* would
become very messy.

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
[Control.Monad.Cont][cont] and trying to discern whether I could use
`Cont`, `ContT`, `MonadCont`, or `callCC` to achieve this, I decided
that I had even less of an idea than when I started.  I was leaning
towards "no," but I still have no idea.)

In the end, I ignored coroutines completely.  I made a couple basic
abstractions and some plumbing to make them practically usable, and
this worked well for me.  The [CPS](#cps) section gives some further
examples on this.

Examples
====

The examples here are all rather contrived.  All of the "real" work
that I did with Ion was proprietary code that I can't share, and even
if I could, it wouldn't build to an actual embedded target, except in
the context of the much larger [Shake][] build it was in and the
entire associated toolchain.

However, I've tried to create some representative examples to a
hypothetical C API.  See [Example.hs][] for these examples in a more
buildable form (at least as far as the Haskell part goes).

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
magic the timer requires - go do that, hide it, and don't let Ivory
know about it. Also, the name of that function is what we supplied to
`ionCompile`.

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

The next example might be a bit trickier to understand, but I hope
that it both explains both this functionality and what I mean when I
talk about 'composing' things.

I'll start with a smaller piece rather than a larger one this time.
Suppose we're dealing with some kind of async API with `uint16_t
transmit_async(uint16_t opcode, void (*callback)(uint16_t))` - it
transmits `opcode`, it returns some kind of immediate success/error
code as a `uint16_t` (perhaps an error indicates a failure to even
transmit), and when the opcode returns a result, then it calls
`callback` with some payload.

Given the right machinery, this actually composes pretty easily with
continuation-passing style.  Consider the below, which connects this
function to two other callbacks - a different "error" callback
(perhaps we produce our own error code of another format), and a
"success" callback - and returns for us an initialization procedure:

```haskell
exampleSend :: Word16 -- ^ Payload value (or something like that)
               -> Def ('[Uint32] ':-> ()) -- ^ Error callback
               -> Def ('[Uint16] ':-> ()) -- ^ Success callback
               -> Ion (Def ('[] ':-> ()))
exampleSend payload err succ = mdo

  let transmit_async :: Def ('[Uint16, ProcPtr ('[Uint16] :-> ())] :-> Uint32)
      transmit_async = importProc "transmit_async" "foo.h"

  write <- newProc $ body $ do
    comment $ "Transmit value: " ++ show payload
    -- Tell transmit_async to transmit this, and call us back at 'recv'
    -- (which we define after):
    errCode <- call transmit_async (fromIntegral payload) $ procPtr recv
    -- Check for a nonzero error code:
    ifte_ (errCode /=? 0)
      (call_ err errCode)
      $ return ()

  recv <- newProc $ \value -> body $ do
    -- Say that hypothetically we should have received the same value
    -- back, so check this first:
    ifte_ (value /=? fromIntegral payload)
      -- If a mismatch, then call the error handler with some code:
      (call_ err 0x12345678)
      -- Otherwise, call the success handler:
      $ call_ succ value

  return write
```

Remember that timer module we used in the last section, and how it was
parametrized over a callback to be called upon expiration?  We could
probably easily rig this into `exampleSend` so that a failure to
receive a reply within N milliseconds would trigger the error
callback.  (I'm not going to do that - this is just an example of how
one can combine callbacks.)

Slight aside: You might notice the use of `mdo` again.  This is
because otherwise the definitions would have to go in reverse order of
their calls.  Consider the following:

```haskell
foo = do
  call1 <- newProc $ body $ call_ call2
  call2 <- newProc $ body $ call_ call3
  call3 <- newProc $ body $ call_ call4
  call4 <- newProc $ body $ call_ call5
  ...
  callN <- newProc $ body $ retVoid
```

`call1` calls `call2`, which calls `call3`, which calls `call4`, and
so on.  They're in the order in which they'd be called.  However, this
is invalid code, because the definition of `call1` relies on a
definition that comes later.  If we actually reordered these to be
valid, then definitions would be backwards from the order in which
they're called.  The simplest solution I found was just to use `mdo`.

Back to `exampleSend`: Now that it's defined in this format, we can
build it up into larger things. Consider the below definition which
invokes it three times:

```haskell
exampleChain :: Ion (Def ('[] ':-> ()))
exampleChain = mdo
  let error :: Def ('[Uint32] :-> ())
      error = importProc "assert_error" "something.h"

  init <- exampleSend 0x1234 error =<<
          adapt_0_1 =<< exampleSend 0x2345 error =<<
          adapt_0_1 =<< exampleSend 0x3456 error =<<
          adapt_0_1 =<< exampleSend 0x4567 error success

  success <- newProc $ \_ -> body $ do
    call_ printf "All calls succeeded!\r\n"

  return init
```

This chains together 4 calls to `exampleSend` to create a procedure
that will try to send opcodes 0x1234, 0x2345, 0x3456, and 0x4567.  If
any step fails, `assert_error` is called (though we could just as
easily parametrize `exampleChain` over a separate error callback).  If
every step succeeds, it calls the internal procedure `success` (of
course, again, we could parametrize over this callback too).  It
returns the entry procedure which starts this entire process.

I haven't explained `adapt_0_1` yet, but it's just a piece of plumbing
to stick together mismatched C functions.  The success callback takes
a single `Uint16`, but we for whatever reason don't need it, so
`adapt_0_1` throws away that value and just calls the entry procedure
of `exampleSend` (which takes no arguments).

This produces the below C code:

```c
// module exampleChain Source:

#include "exampleChain.h"
void exampleChain_0(void)
{
    /* Transmit value: 17767 */
    ;
    
    uint32_t n_r0 = transmit_async((uint16_t) 17767U, exampleChain_1);
    
    if ((bool) ((uint32_t) 0U != n_r0)) {
        assert_error(n_r0);
    }
}
void exampleChain_1(uint16_t n_var0)
{
    if ((bool) (n_var0 != (uint16_t) 17767U)) {
        assert_error((uint32_t) 305419896U);
    } else {
        exampleChain_11(n_var0);
    }
}
void exampleChain_2(uint16_t n_var0)
{
    exampleChain_0();
}
void exampleChain_3(void)
{
    /* Transmit value: 13398 */
    ;
    
    uint32_t n_r0 = transmit_async((uint16_t) 13398U, exampleChain_4);
    
    if ((bool) ((uint32_t) 0U != n_r0)) {
        assert_error(n_r0);
    }
}
void exampleChain_4(uint16_t n_var0)
{
    if ((bool) (n_var0 != (uint16_t) 13398U)) {
        assert_error((uint32_t) 305419896U);
    } else {
        exampleChain_2(n_var0);
    }
}
void exampleChain_5(uint16_t n_var0)
{
    exampleChain_3();
}
void exampleChain_6(void)
{
    /* Transmit value: 9029 */
    ;
    
    uint32_t n_r0 = transmit_async((uint16_t) 9029U, exampleChain_7);
    
    if ((bool) ((uint32_t) 0U != n_r0)) {
        assert_error(n_r0);
    }
}
void exampleChain_7(uint16_t n_var0)
{
    if ((bool) (n_var0 != (uint16_t) 9029U)) {
        assert_error((uint32_t) 305419896U);
    } else {
        exampleChain_5(n_var0);
    }
}
void exampleChain_8(uint16_t n_var0)
{
    exampleChain_6();
}
void exampleChain_9(void)
{
    /* Transmit value: 4660 */
    ;
    
    uint32_t n_r0 = transmit_async((uint16_t) 4660U, exampleChain_10);
    
    if ((bool) ((uint32_t) 0U != n_r0)) {
        assert_error(n_r0);
    }
}
void exampleChain_10(uint16_t n_var0)
{
    if ((bool) (n_var0 != (uint16_t) 4660U)) {
        assert_error((uint32_t) 305419896U);
    } else {
        exampleChain_8(n_var0);
    }
}
void exampleChain_11(uint16_t n_var0)
{
    printf("All calls succeeded!\r\n");
}
void exampleChain(void)
{
    /* Auto-generated schedule entry procedure from Ion & Ivory */
    ;
}
```

and the below C header:

```c
// module exampleChain Header:

#include "ivory.h"
void exampleChain_0(void);
void exampleChain_1(uint16_t n_var0);
void exampleChain_2(uint16_t n_var0);
void exampleChain_3(void);
void exampleChain_4(uint16_t n_var0);
void exampleChain_5(uint16_t n_var0);
void exampleChain_6(void);
void exampleChain_7(uint16_t n_var0);
void exampleChain_8(uint16_t n_var0);
void exampleChain_9(void);
void exampleChain_10(uint16_t n_var0);
void exampleChain_11(uint16_t n_var0);
void exampleChain(void);
```

Note that this is all done with no static variables.  It has no need
to store a continuation, call stack, or anything of the sort.  Of
course, it's not magic.  You still need to use static variables for
state that lives across calls, and external APIs have to store their
callbacks someplace.

Note one particular limitation here (the same one as with `timer`):
When you parametrize an `Ion` over procedures, those procedures are
fixed at C compile-time.  Ivory may now allow some means of storing
function pointers in such a manner that you could work past this
limitation, but if memory serves me, when I wrote this Ivory had no
way to store a function pointer in static memory.  I accepted this
limitation because it made for much better-defined behavior, as I saw
it.

This is fairly representative of the way that I used this
functionality, just a bit simpler.  When I used it for network
communications, it took a form more akin to exception handling, as it
would run through a sequence of steps for which the proper way to
handle a failure differed depending on step.  In that instance, I had
an entire chain of shutdown steps (e.g. close TCP connection; drop
modem connection; turn off modem; turn off UART; and disable power
source), and callbacks would jump to different parts of it.

Conclusions
====

Ion helped me immensely with generating large amounts of C plumbing
whose behavior was very easy to reason about.  It also cut both ways,
as it made it trivial to very quickly occupy a lot of RAM, produce
huge binaries, and generate a lot of very complex code very quickly.

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

- return back to caller
- suspend itself with *yield*
- resume coroutine B (or call it in the first place)

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
[ion_hackage]: https://hackage.haskell.org/package/ion-1.0.0.0
[usenix2002]: https://www.usenix.org/legacy/events/usenix02/full_papers/adyahowell/adyahowell_html/index.html
[Rust]: https://www.rust-lang.org/
[Redox]: http://www.redox-os.org/
[coroutines]: https://github.com/GaloisInc/ivory/blob/master/ivory/src/Ivory/Language/Coroutine.hs
[cps]: https://en.wikipedia.org/wiki/Continuation-passing_style
[cont]: https://hackage.haskell.org/package/mtl/docs/Control-Monad-Cont.html
[Shake]: https://hackage.haskell.org/package/shake
[Example.hs]: https://github.com/HaskellEmbedded/ion/blob/master/src/Ivory/Language/Ion/Examples/Example.hs
