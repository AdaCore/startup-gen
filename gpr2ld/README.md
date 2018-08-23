GPR2LD is a tool to generate a linker script, a memory map and the startup code
matching the description in a GPR file.

You must call it with the gpr file as a command line argument.

In order to generate the interrupt vector, we need a separate GPR file
that will describe it. You must import it from the main GPR file used for
the specs.

Options:
+ -c ARG : Path to the project file mapping supported processors to their
         architecture. The default configuration file is installed with
		 the executable.
        Default value is configuration.gpr.
+ -l ARG : Arg is the name of the generated ldscript.
        Default value is linker.ld.
+ -s ARG: Arg is the name of the generated startup code.
        Default value is startup.S

Known limitations:
+ When the booting from RAM, the tool does not generate the code necessary
  to tell the CPU to reloc the interrupt vector (on ARM cortex m).
