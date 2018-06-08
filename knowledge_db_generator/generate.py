import querytool
import json2gpr
import subprocess
import os

import sys
import sqlite3

db = "bdb"

co = sqlite3.connect(db)
co.row_factory = sqlite3.Row
c = co.cursor()

devices = list()

print "Which device do you want to select ? (Type `list` to display all the devices.)"
sys.stdout.write("> ")

input_line = sys.stdin.readline().strip()

if input_line == "list":
    devices = querytool.query_attributes(c, ["name"], "device")
    for device in devices:
        print device
else:
    if devices == list():
        devices = querytool.query_attributes(c, ["name"], "device")

    if input_line in devices:
            device = input_line
            print "Selected \"%s\"" % device
            dev_json = querytool.get_device_json(device, c)

            files = ["spec.gpr"]

            if "interrupts" in dev_json:
                files.append("interruptions.gpr")

            print "Dumping GPR content to %s" % ' and '.join(files)
            json2gpr.dump_gpr_files(dev_json, *files)

            path = '/home/gay/project/project_generator/gpr2ld/'

            files = [os.path.abspath(f) for f in files]

            subprocess.call(['gpr2ld', '-c',\
                                     path + 'configuration.gpr',\
                                     files[0], files[1]],\
                                     close_fds=True)
    else:
        print "Device %s is not in database !" % input_line
co.commit()
co.close()
