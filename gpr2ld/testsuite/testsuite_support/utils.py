from gnatpython.ex import Run

def contents_of(filename):
    """Return contents of file FILENAME"""
    with open(filename) as f:
        contents = f.read()

    return contents

def run_tool(args, output='gpr2ld.out', error='gpr2ld.err'):
    p = Run(['gpr2ld'] + args,
            output=output, error=error)

    if p.status != 0:
        print "command failed:"
        print contents_of(output)
        print contents_of(error)
