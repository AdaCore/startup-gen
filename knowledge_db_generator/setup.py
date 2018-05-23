from setuptools import setup

def readfile(filename):
    with open(filename, 'r+') as f:
        return f.read()
setup(
    name="QBDB",
    version="1",
    description="Python modules for interacting with the board database.",
    long_description=readfile('README.md'),
    author="Corentin Gay",
    author_email="gay@adacore.com",
    url="https://github.com/AdaCore/Bare_Metal_Project_Generator",
    py_modules=['modifybdb', 'querybdb'],
    license=readfile('README.md'),
    entry_points={
        'console_scripts': [
            'modifybdb = modifybdb:entry',
            'querybdb = querybdb:entry'
        ]
    },
)
