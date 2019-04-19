from testsuite_support.utils import run_tool, gprbuild, gnatemu, contents_of


run_tool (['-P', 'invalid.gpr', '-s', 'crt0.S', '-l', 'linker.ld'])
