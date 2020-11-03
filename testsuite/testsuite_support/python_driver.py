import os
import os.path
import sys

from e3.testsuite.driver.diff import DiffTestDriver


class PythonDriver(DiffTestDriver):

    @property
    def baseline_file(self):
        return ('test.ref', False)

    @property
    def diff_ignore_white_chars(self):
        return True

    def run(self):
        env = dict(os.environ)
        env['target-triplet'] = str(self.env.target.triplet)

        # give it access to our Python helpers
        python_path = env.get('PYTHONPATH', '')
        path_for_drivers = os.path.abspath(
            os.path.dirname(os.path.dirname(__file__)))
        env['PYTHONPATH'] = '{}{}{}'.format(
            path_for_drivers, os.path.pathsep, python_path
        ) if python_path else path_for_drivers

        self.shell([sys.executable, 'test.py'], env=env)
