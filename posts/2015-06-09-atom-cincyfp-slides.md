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
local (to me) functional programming group, requested that I give a
presentation on some of the embedded Haskell stuff I was doing, and I
did this at the [June meeting][cincyfp-announcement] this year.

Slides from the presentation are up [here][slides-local] (and
[Markdown source][slides-markdown-local]; or look
[in the repo][slides-repo]).  CincyFP seems to have mostly Clojure
aficionados, so in terms of Haskell-heavy details I kept this
presentation as a "part 1" to some possible future presentations.
Actually, not even that, since the title was "EDSLs &
Metaprogramming" and I only scratched the surface of that.

I start walking through an Atom example at slide 9.  While I
live-coded this at the presentation, I have no video or audio.
However, I put my example code online [here][slides-code] (and
[StringEmbed.hs][slides-stringembed]).

The slides are fairly terse, and I intend to explain bits of them in
the next few posts.  Particularly, around slide 2, I try to explain a
few different broad ways Haskell might be used on embedded systems,
and this refers to similar categories as on the [Links][links] page.
I find myself having to explain these distinctions quite often, so I
am going to try to explain this better in posts to come.

Slide 6 is a little bit misleading.  The next post tries to patch this
up a bit.

[cincyfp-announcement]: https://cincyfp.wordpress.com/2015/06/04/june-meeting-4/
[slides-local]: ../slides/20150609_CincyFP/Slides.html
[slides-markdown-local]: ../slides/20150609_CincyFP/Slides.md
[slides-repo]: https://github.com/HaskellEmbedded/HaskellEmbedded.github.io/tree/master/slides/20150609_CincyFP
[slides-code]: ../slides/20150609_CincyFP/Example.hs
[slides-stringembed]: ../slides/20150609_CincyFP/StringEmbed.hs
[links]: ../pages/links.html

