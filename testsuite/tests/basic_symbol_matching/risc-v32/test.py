from testsuite_support.utils import make_simple_project, nm_symbols,\
                                    check_symbols, MemoryMap

memmap = MemoryMap()
memmap.add('flash1', 'ROM', 0x0000000, 32 * 1024)
memmap.add('flash2', 'ROM', 0x1000000, 32 * 1024)
memmap.add('flash3', 'ROM', 0x2000000, 32 * 1024)
memmap.add('ram1', 'RAM', 0x3000000, 32 * 1024)
memmap.add('ram2', 'RAM', 0x4000000, 32 * 1024)
memmap.add('ram3', 'RAM', 0x5000000, 32 * 1024)

for runtime in ['light-rv32i',
                'light-rv32iac',
                'light-rv32im',
                'light-rv32imac',
                'light-rv32imafc',
                'light-rv32imafdc']:
    for boot_mem in ['flash1', 'ram1']:
        for stack_mem in ['ram1', 'ram2']:
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
            if check_symbols(syms, memmap, [('_start', boot_mem),
                                            ('__global_pointer$', 'ram1'),
                                            ('__stack_start', stack_mem),
                                            ('__stack_end', stack_mem),
                                            ('__heap_start', 'ram1'),
                                            ('__heap_end', 'ram1')]):
                print("With boot_mem = %s stack_mem = %s:" % (boot_mem, stack_mem))
                print("Testing run-time: %s" % runtime)
                print("------")
