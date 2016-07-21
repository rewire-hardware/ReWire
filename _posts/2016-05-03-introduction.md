---
layout: page
title: "Introduction"
category: doc
date: 2016-05-03 10:51:20
order: 1
---

ReWire is a [purely functional](https://en.wikipedia.org/wiki/Purely_functional) language for creating secure hardware designs that can be implemented on FPGAs, developed at the [University of Missouri](http://www.missouri.edu/) Center for High Assurance Computing. 
The ReWire language is a subset of [Haskell](http://www.haskell.org/) that places some restrictions on the use of recursion. The ReWire compiler `rwc` translates ReWire programs into synthesizable VHDL.

This instruction manual has four parts:

1. a discussion of how to acquire and install ReWire and its dependencies;
2. a "quick start" tutorial that walks the user through the process of compiling and running a simple example program, both in simulation and on hardware;
3. a detailed discussion of the ReWire language; and
4. a detailed case study in the ReWire design flow.

While we aim to make this guide as self-contained as possible, prior knowledge of Haskell and some experience with basic FPGA development will be helpful.
