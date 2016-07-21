---
layout: default
title: "ReWire Functional Hardware Description"
---

{% comment %} Here is the syntax for figures from jekyll_figure.
{% figure 4Ps pdf 'Your caption here' %}
{% endcomment %}

#### __ReWire: Functional Hardware Description__

[ReWire](https://github.com/mu-chaco/ReWire) is an open source programming language for designing, implementing, and formally verifying hardware artifacts. ReWire is a language for high-level synthesis based in the functional language [Haskell](https://www.haskell.org). Functional languages are a commonly proposed approach to alleviating the well-known FPGA programmability problem---a.k.a., the three P's (Productivity, Performance and Portability). But the ReWire approach takes this one step further with _Provability_.


<img src="{{ site.baseurl }}/images/4Ps.pdf" style="display: block; margin: 0 auto; max-width: 50%;" alt="Screenshot" />

ReWire programs _are_ Haskell programs. That means, every ReWire program _is_ a Haskell program as well. And so, ReWire inherits many of Haskell's fine qualities, including:

* Pure functions and types, monads, equational reasoning, etc.
* Formal semantics supporting formal reasoning.


ReWire programs, however, may all be translated directly to VHDL using the ReWire compiler:

<img src="{{ site.baseurl }}/images/ReWire.pdf" style="display: block; margin: 0 auto; max-width: 75%;" alt="Screenshot" />


Why does all this distinguish us from other research efforts? Read on!

#### __Just Say No! to Semantic Archaeology__

Say you have a hardware design written in a production hardware description language (HDL) like VHDL or Verilog and you need to formally verify some properties of the design and implementation. Where do you start?

Well, you can't reason directly about the HDL code. Why not? _Because the HDL possesses no formal semantics!_ Using traditional formal methods for hardware (e.g., [Kropf 1999](http://dl.acm.org/citation.cfm?id=519876)), one must laboriously formulate a formal model in the logic of a theorem prover. We call this process _semantic archaeology_ and it is expensive, time-consuming and error-prone (i.e., how do you know that your formal specification is related to the concrete HDL design?). Semantic archaeology is the bane of formal methods.

Because ReWire has a formal semantics, one can reason about hardware designs in ReWire directly on the ReWire code itself just as one would about a normal pure functional program. One can also apply tools and techniques from software verification to hardware verification (e.g., as we have with [Coq](https://coq.inria.fr) and [language-based methods in security](http://dl.acm.org/citation.cfm?id=1662663)). For more information, please consult the reading below or send an [email](mailto:rewire-questions@googlegroups.com).




##### __Further Reading__

* _A Programming Model for Reconfigurable Computing Based in Functional Concurrency_, William L. Harrison, Adam Procter, Ian Graves, Michela Becchi, and Gerard Allwein. ReCoSoC 2016 [pdf]({{ site.baseurl }}/assets/papers/recosoc16.pdf) [slides]({{ site.baseurl }}/assets/papers/slides-recosoc16.pdf).
* _Provably Correct Development of Reconfigurable Hardware Designs via Equational Reasoning_, Ian Graves, Adam Procter, William L. Harrison, and Gerard Allwein. FPT 2015 [pdf]({{ site.baseurl }}/assets/papers/fpt15.pdf) [slides]({{ site.baseurl }}/assets/papers/slides-fpt15.pdf).
* _A Principled Approach to Secure Multi-Core Processor Design with ReWire_, Adam Procter, William Harrison, Ian Graves, Michela Becchi, and Gerard Allwein. ACM Transactions on Embedded Computing Systems (to appear) [pdf]({{ site.baseurl }}/assets/papers/tecs16.pdf).
* _Semantics Driven Hardware Design, Implementation, and Verification with ReWire_, Adam Procter, William Harrison, Ian Graves, Michela Becchi, and Gerard Allwein. LCTES 2015 [pdf]({{ site.baseurl }}/assets/papers/lctes15.pdf).
* _Hardware Synthesis from Functional Embedded Domain-Specific Languages:
A Case Study in Regular Expression Compilation_, Ian Graves, Adam Procter, William Harrison, Michela Becchi, and Gerard Allwein. ARC 2015 [pdf]({{ site.baseurl }}/assets/papers/arc15.pdf).

{% comment %}
### Get Started

Start by [creating a new post](http://jekyllrb.com/docs/posts/) one of the categories listed in `_config.yml`. It will appear in the navigation on the left once recompiled. Or use the supplied script to make creating pages easier:

```bash
ruby bin/jekyll-page "Some Page Title" ref
```

#### Don't Forget

- Add your own content to this page (i.e. `index.md`) and change the `title`
- Change `title` and `subtitle` defined in `config.yml` for your site
- Set the `baseurl` in `_config.yml` for your repo if deploying to GitHub pages
{% endcomment %}