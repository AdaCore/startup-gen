import fileinput
from testsuite_support.utils import run_tool, gprbuild, runcross, contents_of

run_tool (['-P', 'prj.gpr', '-s', 'src/crt0.S', '-l', 'src/linker.ld'])

def add_hard_busy_loop(filename):
    f = open(filename, "r")
    contents = f.readlines()
    f.close()

    for index in range(len(contents)):
        if "__stack_end" in contents[index]:
            # Insert a piece of code that will only allow hart 1 to continue
            contents.insert(index + 1, "        li   t0, 1\n")
            contents.insert(index + 2, "1:      csrr t1, mhartid\n")
            contents.insert(index + 3, "        bne t0, t1, 1b\n")
            break
    f = open(filename, "w")
    contents = "".join(contents)
    f.write(contents)
    f.close()

add_hard_busy_loop("src/crt0.S")

gprbuild(['-f', '-P', 'prj.gpr'])

runcross('riscv64-elf', 'qemu-polarfiresoc', 'obj/main', output='runcross.out')

print(contents_of('runcross.out'))
