from e3.os.process import Run
from e3.fs import mkdir
from e3.env import Env

import os

try:
    from pycross.runcross.main import run_cross
except ImportError:
    # Make pycross an optional dependency

    def run_cross(*args, **kwargs):
        raise ImportError("Could not import pycross.runcross.main")


def contents_of(filename):
    """Return contents of file FILENAME"""
    with open(filename) as f:
        contents = f.read()

    return contents


def run_tool(args, output='startup-gen.out', error='startup-gen.err'):
    p = Run(['startup-gen'] + args, output=output, error=error)

    if p.status != 0:
        print("command failed:")
        print(contents_of(output))
        print(contents_of(error))

    return contents_of(output)


def gprbuild(args, output='gprbuild.out', error='gprbuild.err'):
    p = Run(['gprbuild'] + args, output=output, error=error)

    if p.status != 0:
        print("Build failed:")
        print(contents_of(output))
        print(contents_of(error))


def runcross(target, board, bin, output='runcross.out'):
    Env().set_target(target, '', board)

    p = run_cross([bin], output=output, timeout=5)

    if p.status != 0:
        print("runcross failed:")
        print(contents_of(output))


class MemoryDescription:
    def __init__(self, name, kind, addr, size):
        self.name = name
        self.kind = kind
        self.addr = addr
        self.size = size


class MemoryMap(dict):
    def add(self, name, kind, addr, size):
        self[name] = MemoryDescription(name, kind, addr, size)


class SymbolList:
    def __init__(self):
        self.list = []

    def append(self, name, addr):
        self.list.append((name, addr))

    def defined(self, symbol):
        return symbol in (x[0] for x in self.list)

    def in_range(self, symbol, start, end):
        for name, addr in self.list:
            if name == symbol:
                return addr >= start and addr <= end
        return False

    def in_memory(self, symbol, mem):
        return self.in_range(symbol, mem.addr, mem.addr + mem.size)

    def in_memmap(self, symbol, memmap, mem):
        return self.in_range(symbol,
                             memmap[mem].addr,
                             memmap[mem].addr + memmap[mem].size)


def check_symbols(symbols, memmap, to_check):
    """
    With a list of tuple (symbol_name, memory_name), check that the symbol are
    define in the expected memory region
    """
    for name, mem in to_check:
        if not symbols.defined(name):
            print("Symbol '%s' is not defined" % name)
        elif not symbols.in_memmap(name, memmap, mem):
            print("Symbol '%s' is not defined in '%s'" % (name, mem))
        else:
            print("Symbol '%s' defined in '%s'" % (name, mem))


def nm_symbols(binary):
    """Return a SymbolList from a binary file"""

    output = 'nm.out'
    error = 'nm.err'

    triplet = os.environ['target-triplet']
    args = [triplet + '-nm', binary]
    p = Run(args, output=output, error=error)

    if p.status != 0:
        print("nm failed:")
        print(contents_of(output))
        print(contents_of(error))

    result = SymbolList()
    for line in contents_of(output).splitlines():
        addr, typ, symbol = line.split(' ')

        if typ in ['A', 'B', 'C', 'D', 'd', 'G', 'g',
                   'R', 'r', 'S', 's', 'T', 't']:
            result.append(symbol, int('0x' + addr, base=16))

    return result


def generate_gpr(filename, runtime, target, CPU, memmap, boot_mem, stack_mem):
    """ Generate a simple project file from the provided memory layout"""

    with open(filename, "w") as f:
        f.write('project Prj is\n')
        f.write('   for Target use "%s";\n' % target)
        f.write('   for Runtime ("Ada") use "%s";\n' % runtime)
        f.write('   for Languages use ("Ada", "Asm_CPP");\n')
        f.write('   for Source_Dirs use (Project\'Project_Dir & "src");\n')
        f.write('   for Object_Dir use Project\'Project_Dir & "obj";\n')
        f.write('   for Main use ("main.adb");\n')
        f.write('   for Create_Missing_Dirs use "True";\n')
        f.write('   package Linker is\n')
        f.write('      for Switches ("Ada") use ("-T", "src/linker.ld");\n')
        f.write('   end Linker;\n')
        f.write('   package Device_Configuration is\n')
        f.write('      for CPU_Name use "%s";\n' % CPU)
        f.write('      for Memories use ("%s");\n' % '", "'.join(memmap))
        f.write('      for Boot_Memory use \"%s\";\n' % boot_mem)
        f.write('      for Main_Stack_Memory use \"%s\";\n' % stack_mem)
        for mem in memmap:
            f.write('      for Mem_Kind ("%s") use "%s";\n' %
                    (memmap[mem].name, memmap[mem].kind))
            f.write('      for Address ("%s")  use "%d";\n' %
                    (memmap[mem].name, memmap[mem].addr))
            f.write('      for Size ("%s")     use "%d";\n' %
                    (memmap[mem].name, memmap[mem].size))

        if runtime.startswith('light-rv'):
            f.write('      for User_Tag ("qemu_sifive_test_exit") use "True";')
        f.write('   end Device_Configuration;\n')
        f.write('end Prj;\n')


def make_simple_project(dir, runtime, target, CPU, mems, boot_mem, stack_mem):
    """
    Create and build a simple project with empty main procedure, and return the
    path to the output binary.
    """
    mkdir(dir)
    mkdir(os.path.join(dir, 'src'))

    # Create an empty main procedure
    with open(os.path.join(dir, 'src', 'main.adb'), 'w') as f:
        f.write("procedure Main is begin null; end Main;\n")

    # Generate the project file from memory layout
    generate_gpr(os.path.join(dir, 'prj.gpr'), runtime,
                 target, CPU, mems, boot_mem, stack_mem)

    # Generate crt0 and linker script
    run_tool(['-P', os.path.join(dir, 'prj.gpr'),
              '-s', os.path.join(dir, 'src', 'crt0.S'),
              '-l', os.path.join(dir, 'src', 'linker.ld')])

    # Build
    gprbuild(['-f', '-P', os.path.join(dir, 'prj.gpr')])

    # Return path to the binary file
    return os.path.join(dir, 'obj', 'main')
