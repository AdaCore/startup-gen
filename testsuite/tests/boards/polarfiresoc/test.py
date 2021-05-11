from testsuite_support.utils import run_tool, gprbuild, runcross, contents_of

run_tool(['-P', 'prj.gpr', '-s', 'src/crt0.S', '-l', 'src/linker.ld'])

gprbuild(['-f', '-P', 'prj.gpr'])

runcross('riscv64-elf', 'qemu-polarfiresoc', 'obj/main', output='runcross.out')

print(contents_of('runcross.out'))
