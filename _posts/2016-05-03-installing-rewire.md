---
layout: page
title: "Installing ReWire"
category: doc
date: 2016-05-03 10:51:28
order: 2
---

# Step 1: Install System Requirements

Your system will need to meet a few prerequisites before installing ReWire:

* [Haskell Platform](https://www.haskell.org/platform/prior.html), version 7.10.3. Later versions of GHC (8.0 or greater) will not work with ReWire because these later releases do not yet fully support libraries ReWire relies on.
* Some sort of FPGA development tool suite that supports VHDL. Currently the only tested tools are those from Xilinx:
   - [ISE](http://www.xilinx.com/products/design-tools/ise-design-suite.html)
   - [Vivado](http://www.xilinx.com/products/design-tools/vivado.html)

  Each of these tools is available in a free "WebPACK" version.
* (Optional) An FPGA development board. (If you do not have access to a development board, you may still be able to test out your designs in simulation.)

For the quick start tutorial we will be using ISE and the (sadly discontinued) [Spartan-3E Starter Kit](http://www.xilinx.com/products/boards-and-kits/hw-spar3e-sk-us-g.html).

# Step 2: Acquire ReWire Source

You can download the latest ReWire source code from the [release page](https://github.com/mu-chaco/ReWire/releases) on GitHub.

# Step 3: Build ReWire

Note that depending on your system configuration, the `cabal install` steps below may require administrative privileges.

    $ tar xvzf ReWire-<version>.tar.gz
    $ cd ReWire-<version>
    $ cabal install --only-dependencies
    $ cabal configure
    $ cabal install

If everything works correctly, you should now have an `rwc` binary somewhere in your path.

    $ which rwc
    /Users/me/Library/Haskell/bin/rwc
