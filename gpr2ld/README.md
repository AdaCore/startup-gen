GPR2LD is a tool to generate a linker script, a memory map and the startup code
matching the description in a GPR file.

You must call it with the gpr file as a command line argument.

In order to generate the interrupt vector, we need a separate GPR file
that will describe it. You must import it from the main GPR file used for
the specs.

Options:
        -o ARG : Arg is the directory in which the generated files will be put.
        -l ARG : Arg is the name of the generated ldscript.
                Default value is linker.ld.
        -m ARG : Arg is the name of the generated memory map.
                Default value is memory_map.ld.

