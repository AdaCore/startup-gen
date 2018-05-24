import querybdb
import modifybdb
import json

db = "bdb"

#for row in querybdb.query_names("tree", db):
#    print row

device_name = "Z32F38412ALS"

info = querybdb.query_device(device_name, db)

out = querybdb.get_json_output_for_device(device_name, db)
json_output = json.dumps(out)

print json_output
#project_file, interrupts = json2gpr.translate(json_output)
