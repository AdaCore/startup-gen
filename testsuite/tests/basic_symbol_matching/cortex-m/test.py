from testsuite_support.utils import make_simple_project, nm_symbols, \
                                    check_symbols, MemoryMap

memmap = MemoryMap()
memmap.add('flash1', 'ROM', 0x0000000, 32 * 1024)
memmap.add('flash2', 'ROM', 0x1000000, 32 * 1024)
memmap.add('flash3', 'ROM', 0x2000000, 32 * 1024)
memmap.add('ram1', 'RAM', 0x3000000, 32 * 1024)
memmap.add('ram2', 'RAM', 0x4000000, 32 * 1024)
memmap.add('ram3', 'RAM', 0x5000000, 32 * 1024)

for CPU, runtime in [('Cortex-M0', 'light-cortex-m0'),
                     ('Cortex-M0+', 'light-cortex-m0p'),
                     ('Cortex-M1', 'light-cortex-m1'),
                     ('Cortex-M3', 'light-cortex-m3'),
                     ('Cortex-M4', 'light-cortex-m4'),
                     ('Cortex-M4F', 'light-cortex-m4f')]:
    for boot_mem in ['flash1', 'ram1']:
        for stack_mem in ['ram1', 'ram2']:
            bin = make_simple_project('test_simple_project_%s_%s_%s' % (boot_mem,
                                                                        stack_mem,
                                                                        runtime),
                                      runtime,
                                      'arm-eabi',
                                      CPU,
                                      memmap,
                                      boot_mem,
                                      stack_mem)
            syms = nm_symbols(bin)
            if check_symbols(syms, memmap, [('Reset_Handler', boot_mem),
                                            ('__heap_start', 'ram1'),
                                            ('__heap_end', 'ram1'),
                                            ('__stack_start', stack_mem),
                                            ('__stack_end', stack_mem)]):
                print("With boot_mem = %s stack_mem = %s:" % (boot_mem, stack_mem))
                print("CPU: %s run-time: %s" % (CPU, runtime))
                print("------")
