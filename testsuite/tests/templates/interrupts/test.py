from testsuite_support.utils import run_tool, contents_of

run_tool(['-s', 'test.out', '-P', 'spec.gpr'])
run_tool(['-s', 'test_no_int.out', '-P', 'spec_no_int.gpr'])

print("with interrupts:")
print(contents_of('test.out'))
print("without interrupts:")
print(contents_of('test_no_int.out'))
