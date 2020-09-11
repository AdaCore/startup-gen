import os
import os.path
import sys


from gnatpython.ex import PIPE, Run, STDOUT
from gnatpython import fileutils
from gnatpython.testsuite.driver import TestDriver
from gnatpython.env import Env


class PythonDriver(TestDriver):
    """
    If the 'helper' option is used in the YAML file, the corresponding test
    helper (from the 'tests/test_helpers' directory) is run with a set of
    standard arguments ('output_dir', etc.). Otherwise, the default 'test.py'
    file is run.
    """

    #
    # Driver entry points
    #

    py_file = 'test.py'
    """
    Name of the file for the Python script to run.
    """

    out_file = 'actual.out'
    """
    Name of the file for output redirection.
    """

    expected_file = 'test.ref'
    """
    Name of the file that contains the expected output.
    """

    timeout = 300
    """
    Timeout (in seconds) to run the Python script.
    """

    # Add current directory in PYTHONPATH (to find test_utils.py)
    env = Env()
    env.add_search_path('PYTHONPATH', os.getcwd())

    @property
    def python_interpreter(self):
        """
        Return the path to the Python interpreter to use to run tests.

        :rtype: str
        """
        return self.global_env['options'].with_python or sys.executable

    @property
    def test_helpers_dir(self):
        """
        Return the path to the "test_helpers" directory.

        :rtype: str
        """
        return os.path.join(self.global_env['test_dir'], 'test_helpers')

    @property
    def external_sources_dir(self):
        """
        Return the path to the "ext_src" directory.

        :rtype: str
        """
        return os.path.join(self.global_env['test_dir'], 'ext_src')

    @property
    def python_interpreter_args(self):
        """
        Return the arguments to pass to the python interpreter.
        :rtype: list[str]
        """
        if 'helper' in self.test_env:
            py_file = os.path.join(
                self.test_helpers_dir,
                self.test_env['helper']
            )

            return [py_file, '--output_dir=output']
        else:
            return [self.py_file]

    def test_working_dir(self, *args):
        """
        Build a path under the temporary directory created for this testcase.

        :param list[str] args: Path components.
        :rtype: str
        """
        return os.path.join(self.global_env['working_dir'],
                            self.test_env['test_name'],
                            *args)

    def tear_up(self):
        super(PythonDriver, self).tear_up()
        fileutils.sync_tree(self.test_env['test_dir'], self.test_working_dir())

        if 'target' in self.test_env and self.test_env['target'] != Env().target.triplet:
            self.result.set_status('DEAD', 'skip: test for %s' % self.test_env['target'])
            return


        # See if we expect a failure for this testcase
        try:
            comment = self.test_env['expect_failure']
        except KeyError:
            self.expect_failure = False
            self.expect_failure_comment = ''
        else:
            self.expect_failure = True
            if not (comment is None or isinstance(comment, basestring)):
                self.result.set_status(
                    'PROBLEM',
                    'Invalid "expect_failure" entry: expected a string but got'
                    ' {}'.format(repr(comment))
                )
                return
            # Because of wrapping in the YAML file, we can get multi-line
            # strings, which is not valid for comments.
            self.expect_failure_comment = comment.replace('\n', ' ').strip()

    def set_result_status(self, failed, message=''):
        """
        Shortcut to call `self.result.set_status`, taking the expected failure
        message into account (if provided).

        :param bool failed: Whether the testcase should be considered as
            failed.
        :param None|str message: Message for the status, if any.
        """
        if failed:
            if self.expect_failure:
                status = 'XFAIL'
                message = '{} ({})'.format(message,
                                           self.expect_failure_comment)
            else:
                status = 'FAILED'
        else:
            status = 'UOK' if self.expect_failure else 'PASSED'
            message = self.expect_failure_comment

        self.result.set_status(status, message)

    def run(self):
        # Run the Python script and redirect its output to `self.out_file`.
        argv = [self.python_interpreter] + self.python_interpreter_args
        env = os.environ.copy()
        env['EXT_SRC'] = self.external_sources_dir
        env['target-triplet'] = str(self.env.target.triplet)

        p = Run(argv, timeout=self.timeout, output=PIPE, error=STDOUT,
                cwd=self.test_working_dir(), env=env)

        with open(self.test_working_dir(self.out_file), 'a') as f:
            f.write(p.out)

        if p.status != 0:
            self.result.actual_output += '{} returned status code {}\n'.format(
                ' '.join(argv), p.status
            )
            self.result.actual_output += p.out
            self.set_result_status(True, 'error status code')

    def analyze(self):
        diff = fileutils.diff(self.test_working_dir(self.expected_file),
                              self.test_working_dir(self.out_file))

        # Determine the status of this test, ignoring expected failure (for
        # now).
        if diff:
            self.result.actual_output += diff
            failed = True
            message = 'diff in output'
        else:
            failed = False
            message = ''

        self.set_result_status(failed, message)
