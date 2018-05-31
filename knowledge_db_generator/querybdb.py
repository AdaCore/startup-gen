import os
import sys
import shutil
import subprocess
import glob
import sqlite3
import json
import argparse
import copy
import ntpath
import inspect

from xml.dom import minidom


# GLOBAL VARIABLES #
add_documentation = False

def entry_from_cmdline():
    cmd = CommandLine()

    cmd.register_command("packages", False,\
        lambda c: query_attributes(["name"],"package", c))

    cmd.register_command("device_info", True, get_json_output_of_device)

    cmd.register_command("package_of_device", True, query_package_of_device)

    #cmd.register_command("devices_of_package", True, query_devices_of_package)

    cmd.register_command("cpu_of_device", True, query_cpu_of_device)

    cmd.register_command("devices", False, query_devices)

    result = cmd.execute()
    if type(result) is list:
        for elt in result:
            print elt
    else:
        print result

def query_attributes(attributes, table, c):
    select = build_select_statement(table, attributes)
    c.execute(select)

    names = c.fetchall()
    return names

def query_cpu_of_device(name, c):
    device = query_device(name, c)
    cpu_id = device["cpu_id"]

    select = build_select_statement(table="cpu",\
                                    columns_needed=["name, fpu, mpu, endianness"],\
                                    columns_conditions=["id"])

    c.execute(select, [cpu_id])
    cpu = c.fetchone()

    return cpu


def query_package_of_device(name, c):
    top_node = {}

    for row in TreeClimber(name, c):
        top_node = row

    select = build_select_statement(table="package",\
                                    columns_needed=["name"],\
                                    columns_conditions=["family_id"])

    c.execute(select, [top_node["id"]])
    result = c.fetchone()
    if result is None:
        raise Exception("%s device not found in database." % name)
    package_name = c.fetchone()[0]
    print package_name
    return package_name

def get_json_output_of_device(name, c):
    device = query_device(name, c)

    memories = _get_mem_json(name, device, c)
    cpu = _get_cpu_json(name, device, c)
    interrupts = _get_interrupts_json(name, device, c)
  
    json_output = dict()
    cpu_dict = dict()

    for k in cpu.keys():
        cpu_dict[k] = cpu[k]

    json_output = {"device" : dict()}
    json_output["device"]["name"] = device["name"]
    json_output["device"]["cpu"] = cpu_dict
    json_output["device"]["memory"] = memories

    if interrupts is not None:
        json_output["interrupts"] = interrupts

    return json.dumps(json_output)

def _get_interrupts_json(name, device, c):
    dev_id = device["id"]

    select = build_select_statement(table="interrupt_to_node",\
                                    columns_needed=["interrupt_id"],\
                                    columns_conditions=["node_id"])
    c.execute(select, [dev_id])
    ids = c.fetchall()

    # The device has no interrupts defined in the database.
    if ids == list():
        return None

    ids = [device_id[0] for device_id in ids]

    select = '''SELECT interrupt_index,name FROM interrupt WHERE '''+\
             ' OR '.join(['''id IS ?''' for interrupt_id in ids])
            
    c.execute(select, ids)
    interrupts = dict()
    for val in c.fetchall():
        interrupts[val["interrupt_index"]] = val["name"]
    return interrupts


def _get_cpu_json(name, device, c):
    cpu_id = device["cpu_id"]

    select = build_select_statement(table="cpu",\
                                    columns_needed=["name", "fpu"],\
                                    columns_conditions=["id"])

    c.execute(select, [cpu_id])
    cpu = c.fetchone()
    return cpu

def _get_mem_json(name, device, c):
    ids = list()
    for node in TreeClimber(name, c):
        ids.append(node["id"])

    select = build_select_statement(table="memory_to_node",\
                                    columns_needed=["memory_id"],\
                                    columns_conditions=["node_id"])

    memory_ids = list()
    for device_id in ids:
        c.execute(select, [device_id])
        memories = c.fetchall()
        if memories:
            for memory_id in memories:
                memory_ids.append(memory_id[0])

    memories = list()
    select = build_select_statement(table="memory",\
                                    columns_needed=["address", "size", "name"],\
                                    columns_conditions=["id"])
    for memory_id in memory_ids:
        c.execute(select, [memory_id])
        result = c.fetchone()
        temp = dict()
        for key in result.keys():
            temp[key] = result[key]

        if memory_id == device["startup_memory_id"]:
            temp["startup"] = 1
        else:
            # memory_id does not point to the startup memory,
            # we remove the key from the dictionnary
            temp.pop("startup", None)
        memories.append(temp)
    return memories

def merge(child, parent):
    res = child.copy()
    for k,v in parent.items():
        if k not in res.keys():
            res[k] = v
        else:
            if res[k] is None:
                res[k] = v
    return res
    

def query_device(name, c):
    device = {}
    for row in TreeClimber(name, c):
        device = merge(device, row)
    return device 

def query_devices(c):
    select =\
        '''\
        SELECT id,name FROM tree WHERE id NOT IN (SELECT parent_id FROM tree)\
        '''
    c.execute(select)

    devices = [dev["name"] for dev in c.fetchall()]

    return devices



class Command:
    def __init__(self, argument_needed, callback):
        self.callback = callback
        self.argument_needed = argument_needed

class CommandLine:
    def __init__(self):
        self.parser = argparse.ArgumentParser()
        self.commands = dict()

    def add_argument(self):
        self.parser.add_argument(*args, **kwargs)

    def register_command(self, name, argument_needed, callback):
        self.commands[name] = Command(argument_needed, callback)

    def execute(self):
        commands = [command for command in self.commands.keys()]
        self.parser.add_argument('command', choices=commands, help="commands")

        self.parser.add_argument('argument', nargs='?',\
            help="additional argument for certain commands")

        self.parser.add_argument("database",
            help="path to the database file",
            type=str)

        self.arguments = self.parser.parse_args()

        callback_args = list()

        if self.commands[self.arguments.command].argument_needed:
            callback_args.append(self.arguments.argument)

        db = self.arguments.database
        full_path_db = os.path.abspath(db)

        # Opens the databse.
        co = sqlite3.connect(full_path_db)
        co.row_factory = sqlite3.Row
        c = co.cursor()
 
        callback_args.append(c)
        result = self.commands[self.arguments.command].callback(*callback_args)

        # Closes the connection.
        co.commit()
        co.close()
        return result

def build_select_statement(table, columns_needed, columns_conditions=list()):
    statement = '''SELECT ''' + ','.join(columns_needed) + ''' FROM ''' + table
    conditions = ""
    if columns_conditions != list():
        conditions = ''' WHERE ''' +\
            ' and '.join([column + " IS ?" for column in columns_conditions])
    return statement + conditions

def query_attribute_with_condition(cursor, table, attribute, conditions):
    string_conditions = ""
    value_list = ()
    for k,v in conditions.items():
        tmp = "%s = ?" % k
        value_list = value_list + (v,)
        if string_conditions == "":
            string_conditions = tmp
        else:
            string_conditions += " and %s" % tmp

    req = ('''SELECT %s FROM %s WHERE ''' % (attribute, table)) + string_conditions
    cursor.execute(req, value_list)
    return cursor.fetchone()[0]

# Cursor to a database containing a tree table.
class TreeClimber:
    def __init__(self, name, cursor):
        self.cursor = cursor
        self.start_request = '''
                SELECT * FROM tree WHERE name IS ?;
                '''
        self.request = '''
                SELECT * FROM tree WHERE id IS ?;
                '''
        args = [name]
        self.cursor.execute(self.start_request, args)
        result = self.cursor.fetchone()
        self.node = {}
        self.prev_node = {}
        for row in result.keys():
            self.node[row] = result[row]

    def __iter__(self):
        return self

    def next(self):
        if (self.node["id"] == self.node["parent_id"]):
            if (self.prev_node["id"] == self.prev_node["parent_id"]):
                raise StopIteration
        self.prev_node = copy.deepcopy(self.node)

        args = (self.node["parent_id"],)
        self.cursor.execute(self.request, args)
        result = self.cursor.fetchone()
        for row in result.keys():
            self.node[row] = result[row]

        return self.prev_node



