from gnatpython.ex import Run

def contents_of(filename):
    """Return contents of file FILENAME"""
    with open(filename) as f:
        contents = f.read()

    return contents

def run_tool(args, output='gpr2ld.out', error='gpr2ld.err'):
    p = Run(['gpr2ld'] + args, output=output, error=error)

    if p.status != 0:
        print "command failed:"
        print contents_of(output)
        print contents_of(error)


def gprbuild(args, output='gprbuild.out', error='gprbuild.err'):
    p = Run(['gprbuild'] + args, output=output, error=error)

    if p.status != 0:
        print "Build failed:"
        print contents_of(output)
        print contents_of(error)


def gnatemu(args, output='gnatemu.out', error='gnatemu.err'):
    p = Run(['arm-eabi-gnatemu'] + args, output=output, error=error)

    if p.status != 0:
        print "GNATemu failed:"
        print contents_of(output)
        print contents_of(error)
