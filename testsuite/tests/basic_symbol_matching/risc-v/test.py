from testsuite_support.utils import make_simple_project, nm_symbols, check_symbols, MemoryMap

memmap = MemoryMap()
memmap.add('flash', 'ROM', 0x20400000, 512 * 1024 * 1024)
memmap.add('ram', 'RAM', 0x80000000, 16 * 1024)

for boot_mem in memmap:
    print "With boot_mem = %s:" % boot_mem
    bin = make_simple_project('test_simple_project+' + boot_mem,
                              'zfp-rv32imac',
                              'riscv32-elf',
                              'RISC-V32',
                              memmap,
                              boot_mem)
    syms = nm_symbols (bin)
    check_symbols (syms, memmap, [('_start', boot_mem),
                                  ('__global_pointer$', 'ram'),
                                  ('__stack_start', 'ram'),
                                  ('__stack_end', 'ram'),
                                  ('__heap_start', 'ram'),
                                  ('__heap_end', 'ram')])
