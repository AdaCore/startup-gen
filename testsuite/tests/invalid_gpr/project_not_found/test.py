from testsuite_support.utils import run_tool

run_tool(['-P', 'invalid.gpr', '-s', 'crt0.S', '-l', 'linker.ld'])
