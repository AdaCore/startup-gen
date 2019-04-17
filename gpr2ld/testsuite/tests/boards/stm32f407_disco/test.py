from testsuite_support.utils import run_tool, gprbuild, gnatemu, contents_of


for boot_mem in ['flash', 'sram', 'ccm']:

    run_tool (['-P', 'prj.gpr', '-XBOOT_MEM=%s' % boot_mem,
               '-s', 'src/crt0.S', '-l', 'src/linker.ld'])

    gprbuild(['-f', '-P', 'prj.gpr', '-XBOOT_MEM=%s' % boot_mem])

    gnatemu(['--board=STM32F4', 'obj/main'], output='gnatemu.out')

    print contents_of('gnatemu.out')
