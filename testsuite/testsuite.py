#! /usr/bin/env python

"""
e3.testsuite-based testsuite for startup-gen.
"""

import sys
from e3.testsuite import Testsuite
from testsuite_support.python_driver import PythonDriver


class StartupgenTestsuite(Testsuite):
    tests_subdir = 'tests'
    test_driver_map = {'python': PythonDriver}

    enable_cross_support = True


if __name__ == '__main__':
    sys.exit(StartupgenTestsuite().testsuite_main())
