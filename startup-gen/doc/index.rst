=========================
startup-gen Documentation
=========================

.. |startup-gen| replace:: ``startup-gen``

.. toctree::
   :maxdepth: 2
   :caption: Contents:


About startup-gen
=================

|startup-gen| generates startup files (crt0 and linker script) based on
properties of the target device such as: architecture, memory layout, number of
interrupts.

One of the goals of this tool is to make it easier to start developing on
micro-controllers by providing the basic elements of a Board Support Package
(BSP).

The target device properties are provided inside a GNAT Project file (see
`Input format`_) and the output files are generated from templates in the
`template-parser <https://github.com/AdaCore/templates-parser>`_ format (see
`Custom templates`_).

Usage
=====

To use |startup-gen| you need a GNAT Project file that contains the target
device properties (see `Input format`_).

Here is an example of project file:

.. code-block:: ada

  project Spec is

    package Device_Configuration is

      for CPU_Name use "cortex-m4f";
      for Number_Of_Interrupts use "25";

      for Memories use ("flash", "sram");

      for Boot_Memory use "flash";

      --  flash
      for Mem_Kind ("flash") use "ROM";
      for Address ("flash")  use "16#08000000#";
      for Size ("flash")     use "1024K";

      --  sram
      for Mem_Kind ("sram") use "RAM";
      for Address ("sram")  use "16#20000000#";
      for Size ("sram")     use "128K";

   end Device_Configuration;

  end Spec;


The following command line switches are available:

 * -l ARG Name of the generated linker script.
 * -s ARG Name of the generated startup code.
 * -X ARG Specify an external reference for Project Files.
 * -P ARG Name of the project file with the device configuration.
 * --print-tags Print the tags available in templates.

Here is an example of |startup-gen| command:

.. code-block:: bash

  $ startup-gen -P spec.gpr -l link.ld -s crt0.S

This command will generate a linker script in the file ``link.ld`` and a crt0
in the file ``crt0.S`` based on the target properties provided in ``spec.gpr``.


The ``-P`` switch is required. The linker script and/or crt0 will be generated
only if respectively the ``-l`` and/or ``-s`` switches are provided.

Input format
============

The properties of the target device must be provided in a
``Device_Configuration`` package inside a GNAT Project file.

Here are the attributes supported by |startup-gen|:

Architecture and CPU
--------------------

 * ``for CPU_Name use ("<STRING>");`` This attribute is **required** and
   specifies the name of the target CPU. It is used, for instance, to determine
   the default templates that will be used for output generation.

 * ``for Float_Handling use "<soft|hard>";`` This attribute is optional and
   specifies if the floating point support is provided by the CPU/hardware
   (``hard``) or by the compiler/software (``soft``). It is used, for instance,
   to determine if the Floating Point Unit (FPU) is initialized in the
   ``crt0``. Note that you can decide to not use hardware floating point even
   if the target device has an FPU.

Memory
------

 * ``for Memories use (<LIST_OF_STRINGS>);`` This attribute specifies the list
   of memory banks of the device, e.g. ``("flash", "sram", "dtcm")``. For each
   bank of this list, the ``Kind``, ``Address`` and ``Size`` attributes are
   **required**.

 * ``for Boot_Memory use "flash";``

 * ``for Mem_Kind (<BANK_NAME>) use "<ROM|RAM>";`` This attribute specifies the
   kind of memory ``<BANK_NAME>``, either ``ROM`` (read-only) or ``RAM``. This
   attribute is used to determine what linker sections will allocated to the
   bank. For instance a ``.data`` or ``.bss`` cannot be allocated in read-only
   memory.

 * ``for Address (<BANK_NAME>) use "<VALUE>";`` This attribute specifies the
   base address of the memory bank. ``<VALUE>`` is a string containing an Ada or
   C numeric literal, e.g. ``16#08000000#`` or ``0x08000000``.

 * ``for Size (<BANK_NAME>) use "<VALUE>";`` This attribute specifies the size
   of the memory bank. ``<VALUE>`` is a string containing a numeric literal.
   Multiplier suffix ``K`` and ``M`` are supported, e.g. ``16K``.


Interrupts
----------

 * ``for Number_Of_Interrupts use "<VALUE>";`` This attribute specifies the
   number of interrupt lines on the device. The value is a string containing a
   numeric literal. Depending on the architecture, this value can be used to
   create a trap vector in the ``crt0``.

 * ``for Interrupt("<ID>") use "<NAME>";`` This attribute specifies a name for
   an interrupt. ``<ID>`` is a numeric literal representing the interrupt id
   (starts at zero). ``<NAME>`` is a string that will be used to declare
   assembly symbols for instance. Therefore the name must be a valid assembly
   symbol name.

Templates
---------

 * ``for Startup_Template use "<PATH>";`` This attribute specifies a path to a
   custom template file used to startup (``crt0``) generation (see `Custom
   Templates`_).

 * ``for Linker_Template use "<PATH>"`` This attribute specifies a path to a
   custom template file used to liner script generation (see `Custom
   Templates`_).

Advanced Topics
===============

Scenario Variables
------------------

|startup-gen| supports the use of `scenario variables
<https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/gnat_project_manager.html#expressions>`_
in the input project file.

This can be used in multiple ways, here are some examples:

Select boot memory
^^^^^^^^^^^^^^^^^^

.. code-block:: ada

  project Prj is

     type Boot_Mem is ("flash", "sram");
     Boot : Boot_Mem := external ("BOOT_MEM", "flash");

     package Device_Configuration is

        for Memories use ("flash", "sram");

        for Boot_Memory use Boot;

        --  [...]
     end Device_Configuration;
  end Prj;

.. code-block:: bash

  $ startup-gen -P prj.gpr -l link.ld -s crt0.S -XBOOT_MEM=flash
  $ startup-gen -P prj.gpr -l link.ld -s crt0.S -XBOOT_MEM=sram

Select boards with different device configuration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
.. code-block:: ada

  project Prj is

     type Board_Kind is ("dev_board", "production_board");
     Board : Board_Kind := external ("BOARD", "dev_board");

     package Device_Configuration is

        for Memories use ("flash", "sram");

        case Board is
           when "dev_board" =>
              for Size ("sram")     use "256K";
           when "production_board" =>
              for Size ("sram")     use "128K";
        end case;

        --  [...]
     end Device_Configuration;
  end Prj;

.. code-block:: bash

  $ startup-gen -P prj.gpr -l link.ld -s crt0.S -XBOARD=dev_board
  $ startup-gen -P prj.gpr -l link.ld -s crt0.S -XBOARD=production_board


Custom Templates
----------------

You can provide you own templates for startup and linker script files using the
``Startup_Template`` and ``Linker_Template`` attributes (see `Templates`_).

The output files are generated using the `template-parser
<https://github.com/AdaCore/templates-parser>`_ library, you can therefore
refer to this `documentation
<http://docs.adacore.com/live/wave/aws/html/template_parser/index.html>`_ to
write your templates.

To know the list of predefined tags available for your templates, use the
``--print-tags`` command line switch.

For instance:

.. code-block:: bash

  $ startup-gen -P spec.gpr --print-tags
  --- Template tags ---
  BOOT_FROM_ROM => TRUE
  BOOT_MEM => ("flash")
  BOOT_MEM_ADDR => ("0x8000000")
  BOOT_MEM_SIZE => ("0x100000")
  MAIN_RAM => ("sram")
  MAIN_RAM_ADDR => ("0x20000000")
  MAIN_RAM_SIZE => ("0x20000")
  RAM_REGION => ("ccm")
  RAM_ADDR => ("0x10000000")
  RAM_SIZE => ("0x10000")
  ROM_REGION => ()
  ROM_ADDR => ()
  ROM_SIZE => ()
  DEFAULT_STACK_SIZE => ("2048")
  INTERRUPT_NAME => ()
  INTERRUPT_ID => ()
  ---------------------
  [...]
