from gnatpython.testsuite import Testsuite as BaseTestsuite

import python_driver


class Testsuite(BaseTestsuite):
    TEST_SUBDIR = 'tests'
    DRIVERS = {
        'python': python_driver.PythonDriver
    }

    def add_options(self):
        self.main.add_option(
            '--with-python', default=None,
            help='If provided, use as the Python interpreter in testcases.')
