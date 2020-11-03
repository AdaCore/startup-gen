from testsuite_support.utils import make_simple_project, nm_symbols, \
                                    check_symbols, MemoryMap

memmap = MemoryMap()
memmap.add('flash', 'ROM', 0, 32 * 1024)
memmap.add('ram', 'RAM', 0x10000, 32 * 1024)

for CPU, runtime in [('Cortex-M0', 'zfp-cortex-m0'),
                     ('Cortex-M0+', 'zfp-cortex-m0p'),
                     ('Cortex-M1', 'zfp-cortex-m1'),
                     ('Cortex-M3', 'zfp-cortex-m3'),
                     ('Cortex-M4', 'zfp-cortex-m4'),
                     ('Cortex-M4F', 'zfp-cortex-m4f')]:
    print("Testing CPU: %s run-time: %s" % (CPU, runtime))
    for boot_mem in memmap:
        print("With boot_mem = %s:" % boot_mem)
        bin = make_simple_project('test_simple_project_%s_%s' % (boot_mem,
                                                                 runtime),
                                  runtime,
                                  'arm-eabi',
                                  CPU,
                                  memmap,
                                  boot_mem)
        syms = nm_symbols(bin)
        check_symbols(syms, memmap, [('_start', boot_mem),
                                     ('__heap_start', 'ram'),
                                     ('__heap_end', 'ram')])
