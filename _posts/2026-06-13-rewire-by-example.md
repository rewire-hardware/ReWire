---
layout: page
title: "ReWire by Example"
category: tut
date: 2026-06-13 00:00:00
order: 1
---

[**ReWire by Example**]({{ site.baseurl }}/tutorial/) is a gentle, example-driven
introduction to the ReWire *language* (as opposed to the [Quick Start]({% post_url 2016-05-03-quick-start %}),
which covers driving the compiler). It builds up from monadic interpreters to
real synthesizable devices, one small program at a time.

[Open the tutorial &rarr;]({{ site.baseurl }}/tutorial/)

The tutorial is maintained as an [mdbook](https://rust-lang.github.io/mdBook/)
in the [main repository](https://github.com/rewire-hardware/ReWire) under
`tutorial/rewire-by-example`, and is rebuilt and published here automatically on
every change. It covers, among other things:

* **Monad Wrangling 101** --- simple arithmetic interpreters, `Identity`, errors
  and `Maybe`, and adding a register.
* **Hello Worlds** --- simple Mealy machines, Fibonacci, and carry-save adders.
* **Cross Bar Switch** --- a larger worked example.

For the full table of contents, [open the tutorial]({{ site.baseurl }}/tutorial/).
