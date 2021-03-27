# startup-gen

`startup-gen` generates startup files (crt0 and linker script) based on
properties of the target device such as: architecture, memory layout, number of
interrupts.

One of the goals of this tool is to make it easier to start developing on
micro-controllers by providing the basic elements of a Board Support Package
(BSP).

# Build

`startup-gen` depends on the
[template-parser](https://github.com/AdaCore/templates-parser) library. Clone
or download the repository and follow the build instructions.

Once built, set the `GPR_PROJECT_PATH` path using:
```
$  export GPR_PROJECT_PATH=/path/to/built-templates-parser/share/gpr
```

Download and install a native GNAT Community compiler at
[adacore.com/download](https://www.adacore.com/download).

Build with this command:
```
$ gprbuild -P startup_gen.gpr
```

# Usage

See [documentation](doc/source/index.rst).

# Work-in-progress

The `dev` branch of this repository also contains work-in-progress tools to
build a database of target properties for more than 3000 ARM micro-controllers
from CMSIS-Pack files.
