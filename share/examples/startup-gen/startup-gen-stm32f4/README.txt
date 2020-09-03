                 S T A R T U P - G E N - S T M 3 2 F 4
                 =====================================

This simple example demonstrates the use of startup-gen and generic ZFP
run-times for bare-board Ada applications.

The example targets the SMT32F4-Discovery board and prints "Hello world!" using
ARM semihosting.

While targeting a specific developer board, the concept of generic ZFP
run-times can be used on any ARM Cortex-M microcontroller. See the Startup-gen
User's Guide Supplement for more information.

Startup code
------------

Before building the example we have to generate the linker script and startup
code base on the device configuration using the startup-gen tool:

$ startup-gen -P hello.gpr -l src/link.ld -s src/crt0.S

Build
-----

We can then build the example:

$ gprbuild -P hello.gpr

Run
---

And run it on GNATemulator for instance:

$ arm-eabi-gnatemu --board=STM32F4 obj/hello

Adaptation
----------

You can modify this example to work on your ARM Cortex-M target by changing the
Device_Configuration and run-time in the project file.
