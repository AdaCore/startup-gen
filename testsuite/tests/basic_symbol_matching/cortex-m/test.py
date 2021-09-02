from testsuite_support.utils import make_simple_project, nm_symbols, \
                                    check_symbols, MemoryMap

memmap = MemoryMap()
memmap.add('flash', 'ROM', 0, 32 * 1024)
memmap.add('ram', 'RAM', 0x10000, 32 * 1024)
memmap.add('ccm', 'RAM', 0x20000, 32 * 1024)

for CPU, runtime in [('Cortex-M0', 'light-cortex-m0'),
                     ('Cortex-M0+', 'light-cortex-m0p'),
                     ('Cortex-M1', 'light-cortex-m1'),
                     ('Cortex-M3', 'light-cortex-m3'),
                     ('Cortex-M4', 'light-cortex-m4'),
                     ('Cortex-M4F', 'light-cortex-m4f')]:
    print("Testing CPU: %s run-time: %s" % (CPU, runtime))
    for boot_mem in ['flash', 'ram']:
        for stack_mem in ['ram', 'ccm']:
            print("With boot_mem = %s stack_mem = %s:" % (boot_mem, stack_mem))
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
            check_symbols(syms, memmap, [('Reset_Handler', boot_mem),
                                         ('__heap_start', 'ram'),
                                         ('__heap_end', 'ram'),
                                         ('__stack_start', stack_mem),
                                         ('__stack_end', stack_mem)])
