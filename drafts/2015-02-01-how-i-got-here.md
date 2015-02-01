---
title: How I Got Here (i.e. to using Haskell for embedded code)
author: Chris Hodapp
---

Rough outline:
- How I stumbled into using Haskell
- What my experiences were prior to this
- My results with [[https://www.python.org/about/success/cog/](Cog)
- My issues with cocoOS
- My results with Ivory
- More specific Ivory examples?  (Or save those for a later post?)  I could probably create a simplified example.

The problem I ran into was this: I could not get C abstractions that were simultaneously concise and lightweight enough. I was running into some inflexible memory limits. C++ did not appear to have anything of interest either.

I had need for abstractions in a few different areas. I was compiling C code for various target boards, and for the most part simple compile-time definitions were able to handle this. I was handling a lot of concurrency and usually having to juggle around state machines. I was having a lot of repetitive code revolving around serialization and deserialization of data formats in order to handle Bluetooth Low Energy communications and some custom SPI communications.

It's not hard code to write or understand, but it is code where various independent definitions have to be kept in sync with each other, and a failure to do so properly leads to subtle bugs. It is code that hides a simpler representation.

I tackled the issue of repetitive code at first by generating some code from Python, and at some point I migrated this into the very handy tool [[https://www.python.org/about/success/cog/](Cog).

