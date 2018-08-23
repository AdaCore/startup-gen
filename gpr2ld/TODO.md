+ Add support for cortex-m3
+ Fix the generation of code regarding booting in RAM
	+ We are too generic with our sections:
	+ We should be able to generic specific code when booting in RAM
	  if the architecture requires it.

+ Move the content of the sections to a template file,
  that way we can have sections dependant upon the target architecture.

+ Change the way we collect architectures and only get
  the one matching the target CPU.
