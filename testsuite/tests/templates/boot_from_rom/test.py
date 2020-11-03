from testsuite_support.utils import run_tool, contents_of

run_tool(['-s', 'test.out', '-P', 'spec.gpr', '-XBOOT_MEM=rom'])
print(contents_of('test.out'))

run_tool(['-s', 'test.out', '-P', 'spec.gpr', '-XBOOT_MEM=ram'])
print(contents_of('test.out'))
