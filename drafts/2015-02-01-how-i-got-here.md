---
title: How I Got Here (i.e. to using Haskell for embedded code)
author: Chris Hodapp
---

Rough outline:
- How I stumbled into using Haskell
- What my experiences were prior to this
- My results with Ivory
- More specific Ivory examples?  (Or save those for a later post?)  I could probably create a simplified example.
- Atom & concurrency
- "Some background: My undergrad degree is EE. Since 2004, my preferred tool stack was a combination of Python for anything high-level and C for anything lower-level." - Put this elsewhere?
- What about the Ruby quote that I quite like?

One work project since near the begining of 2014 has been the design of an embedded system for which power usage and stability are two primary concerns. It's not controlling an airplane or a nuclear plant - malfunctions will not be catastrophic - but stability still matters.

My boss had decided that for various reasons too numerous and extensive to list here, we needed to have support for Bluetooth Low Energy. Our system at the time ran on the venerable TI MSP430 and I started looking at ways to support BLE from this. All of TI's BLE offerings at the time ran on the 8051 and appeared to require proprietary toolchains. After surveying what else was available, we had eventually decided on the [https://www.nordicsemi.com/eng/Products/Bluetooth-Smart-Bluetooth-low-energy/nRF51822](Nordic nRF51822), a microcontroller based on an ARM Cortex-M0, supported by standard GCC, and providing all the necessary peripherals for BLE - and simply eliminated the MSP430 altogether.

It quickly evolved to a distributed system - some parts were sensors or actuators that were BLE peripherals, another part ran constantly as a BLE central, another part ended up as a more powerful, Internet-connected Linux machine that ran only sparingly.

A few patterns emerged:
 - All parts needed to be speaking the same protocol, which necessitated keeping various structures and constants in sync at the software each part ran.
 - The nRF51822 board acting as the BLE central was responsible for juggling a lot of concurrent operations, because nearly all API calls were async.
 - The nRF51822 boards ran different software, but still shared various bits of code and constants.

The two languages here were C for the nRF51822, and Python for the Linux machine. I tried to build abstractions that could sensibly represent these patterns, but were sufficiently lightweight in C. One was in Python to keep data types in sync; for Linux, it handled it at runtime in Python, and for the microcontroller, it generated C code with the requisite `memcpy` calls and proper offsets and lengths. The C code also had various FSMs (some explicit, some not) for managing the concurrent operations. At later examination they were nearly all ad-hoc coroutines that were 'hiding' a very simple concurrent description in their details.

Seeing that I had ad-hoc coroutines hidden everywhere in my code, I tried to integrate in some lightweight RTOSes that provided some form of cooperative thread or coroutine. I looked at Atomthreads, RIOT OS, Contiki, FreeRTOS, and ChibiOS, but the issue that stopped me in all cases was that they appeared they would interfere with Nordic's firmware, which I required for the Bluetooth Low Energy functionality. Finally, I discovered [http://www.cocoos.net/intro.html](cocoOS) which provided just the cooperative threads and basic messaging primitives, and due to its simplicity required only minimal implementation and nothing very low-level. I quickly ported it and it was working seemingly wonderfully.

I had recently read Joe Armstrong's famous [http://www.erlang.org/download/armstrong_thesis_2003.pdf](thesis on Erlang), and found it very cogent and pragmatic, but still accessible. He defines on page 19 *Concurrency Oriented Programming*. I had a thought that perhaps cocoOS gave me the primitives I needed to let me apply the methodology that Armstrong described, and express the system as a design around concurrent processes. With clean system design in hand and hopeful visions of elegance in my head, I set about working on this - until reaching an abrupt ending when I ran out of memory with around 2 cooperative tasks and almost nothing else compiled in.

Around the same point, I had also extended my Python code generation to generate code for some more complex protocols and refactored it to make use of a very handy library, [https://www.python.org/about/success/cog/](Cog). Cog helped organize and automate things, but my code generation still felt like primitive macros.

Somewhere in here a conclusion started to form in my head: *Sometimes, C is the wrong place for the right abstractions.*  (No, C++ is not some magical remedy to this.) I had put a lot of work into trying to make clean abstractions to make the high-level code comprehensible, cohesive, and maintainable. The problem is that the runtime cost this incurred was too large, and the underlying implementations could be rather ugly too.
