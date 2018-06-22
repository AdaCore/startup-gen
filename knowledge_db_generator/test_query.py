import querybdb
import modifybdb
import json
import cProfile
import re
import os
import glob
import sqlite3


db = "bdb"

def add_all_packages(db):
    for f in glob.glob("cmsis_packs/*.pack"):
#        try: #XXX: temporary exception handling, while packs are not fixed
        modifybdb.add_package(os.path.abspath(f), db)
#        except Exception as e:
#            print "Exception:", e
#            pass


def print_devices(c):
    for device in querybdb.query_devices(c):
        dev_json = querybdb.get_json_output_of_device(device, c)
        print dev_json



add_all_packages(db)

#co = sqlite3.connect(db)
#co.row_factory = sqlite3.Row
#c = co.cursor()
 

#print querybdb.get_json_output_of_device("STM32F429IG", c)
