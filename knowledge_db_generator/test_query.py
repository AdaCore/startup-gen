import querybdb
import modifybdb
import json
import cProfile
import re
import os
import glob



def modify_test():
    db = "bdb"
    for f in glob.glob("cmsis_packs/*.pack"):
        modifybdb.add_package(os.path.abspath(f), db)

def query_test():
    db = "bdb"
    packages = querybdb.query_names("package", db)
    for package in packages:
        print package

#for board_name in querybdb.query_names("board", db):
#    print board_name

#device_name = "Z32F38412ALS"
#
#devices = querybdb.query_devices(db)
#
#out = querybdb.get_json_output_for_device(device_name, db)
#json_output = json.dumps(out)
#
#print json_output
#project_file, interrupts = json2gpr.translate(json_output)

#modify_test()

cProfile.run('modify_test()')
      

