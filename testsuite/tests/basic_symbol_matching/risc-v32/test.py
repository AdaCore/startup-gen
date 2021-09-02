from testsuite_support.utils import make_simple_project, nm_symbols,\
                                    check_symbols, MemoryMap

memmap = MemoryMap()
memmap.add('flash', 'ROM', 0x20400000, 512 * 1024 * 1024)
memmap.add('ram', 'RAM', 0x80000000, 16 * 1024)
memmap.add('ccm', 'RAM', 0x90000000, 16 * 1024)

for runtime in ['light-rv32i',
                'light-rv32iac',
                'light-rv32im',
                'light-rv32imac',
                'light-rv32imafc',
                'light-rv32imafdc']:
    print("Testing run-time: %s" % runtime)
    for boot_mem in ['flash', 'ram']:
        for stack_mem in ['ram', 'ccm']:
            print("With boot_mem = %s stack_mem = %s:" % (boot_mem, stack_mem))
            bin = make_simple_project('test_simple_project_%s_%s_%s' % (boot_mem,
                                                                        stack_mem,
                                                                        runtime),
                                      runtime,
                                      'riscv32-elf',
                                      'RISC-V32',
                                      memmap,
                                      boot_mem,
                                      stack_mem)
            syms = nm_symbols(bin)
            check_symbols(syms, memmap, [('_start', boot_mem),
                                         ('__global_pointer$', 'ram'),
                                         ('__stack_start', stack_mem),
                                         ('__stack_end', stack_mem),
                                         ('__heap_start', 'ram'),
                                         ('__heap_end', 'ram')])
