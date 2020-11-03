from testsuite_support.utils import run_tool, contents_of

run_tool(['-s', 'test.out', '-P', 'spec.gpr'])

print(contents_of('test.out'))
