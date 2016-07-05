---
layout: page
title: "Introduction"
category: doc
date: 2016-05-03 10:51:20
order: 1
---

ReWire is a [purely functional](https://en.wikipedia.org/wiki/Purely_functional) language for creating secure hardware designs that can be implemented on FPGAs, developed at the [University of Missouri](http://www.missouri.edu/) Center for High Assurance Computing. Traditional hardware description languages like VHDL and Verilog...

The ReWire language is a subset of [Haskell](http://www.haskell.org/) that places some restrictions on the use of recursion. The ReWire compiler `rwc` translates ReWire programs into synthesizable VHDL.

ADD: Who is ReWire good for?

This instruction manual has four parts:

1. a discussion of how to acquire and install ReWire and its dependencies;
2. a "quick start" tutorial that walks the user through the process of compiling and running a simple example program, both in simulation and on hardware;
3. a detailed discussion of the ReWire language; and
4. a detailed case study in the ReWire design flow.

While we aim to make this guide as self-contained as possible, prior knowledge of Haskell and some experience with basic FPGA development will be helpful.

####  Further Reading

* _Provably Correct Development of Reconfigurable Hardware Designs via Equational Reasoning_, Ian Graves, Adam Procter, William L. Harrison, and Gerard Allwein. FPT 2015 [pdf]({{ site.baseurl }}/assets/papers/fpt15.pdf) [slides]({{ site.baseurl }}/assets/papers/slides-fpt15.pdf).
* _A Principled Approach to Secure Multi-Core Processor Design with ReWire_, Adam Procter, William Harrison, Ian Graves, Michela Becchi, and Gerard Allwein. ACM Transactions on Embedded Computing Systems (to appear) [pdf]({{ site.baseurl }}/assets/papers/tecs16.pdf).
* _Semantics Driven Hardware Design, Implementation, and Verification with ReWire_, Adam Procter, William Harrison, Ian Graves, Michela Becchi, and Gerard Allwein. LCTES 2015 [pdf]({{ site.baseurl }}/assets/papers/lctes15.pdf).
* _Hardware Synthesis from Functional Embedded Domain-Specific Languages:
A Case Study in Regular Expression Compilation_, Ian Graves, Adam Procter, William Harrison, Michela Becchi, and Gerard Allwein. ARC 2015 [pdf]({{ site.baseurl }}/assets/papers/arc15.pdf).
