from setuptools import setup

def readfile(filename):
    with open(filename, 'r+') as f:
        return f.read()
setup(
    name="JSON2GPR",
    version="1",
    description="Python module that translates json output to gpr format.",
    long_description=readfile('README.md'),
    author="Corentin Gay",
    author_email="gay@adacore.com",
    url="https://github.com/AdaCore/Bare_Metal_Project_Generator",
    py_modules=['json2gpr'],
    license=readfile('README.md'),
    entry_points={
        'console_scripts': [
            'json2gpr = json2gpr:entry_from_cmdline'
        ]
    },
)
