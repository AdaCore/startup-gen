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
from xml.dom import minidom


# GLOBAL VARIABLES #
add_documentation = False

def entry():
    cmd = CommandLine()

    cmd.register_command("packages", False,\
        lambda db: query_names("package", db))

    cmd.register_command("device", True, query_device)

    cmd.register_command("package_of_device", True, query_package_of_device)

    cmd.register_command("devices_of_package", True, query_devices_of_package)

    cmd.register_command("cpu_of_device", True, query_cpu_of_device)

    cmd.register_command("devices", False,\
        lambda db: query_names("tree", db))

    return cmd.execute()

def query_names(table, db):
    co = sqlite3.connect(db)
    co.row_factory = sqlite3.Row
    c = co.cursor()

    rows = list()
    for row in Table(table, c):
        rows.append(row["name"])

    co.commit()
    co.close()
    return rows

def query_devices_of_package(name, db):
    co = sqlite3.connect(db)
    co.row_factory = sqlite3.Row
    c = co.cursor()

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

    co.commit()
    co.close()

def query_cpu_of_device(name, db):
    co = sqlite3.connect(db)
    co.row_factory = sqlite3.Row
    c = co.cursor()

    device = query_device(name, db)
    cpu_id = device["cpu_id"]

    select = build_select_statement(table="cpu",\
                                    columns_needed=["name, fpu, mpu, endianness"],\
                                    columns_conditions=["id"])

    c.execute(select, [cpu_id])
    cpu = c.fetchone()

    co.commit()
    co.close()
    return cpu


def query_package_of_device(name, db):
    co = sqlite3.connect(db)
    co.row_factory = sqlite3.Row
    c = co.cursor()

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

    co.commit()
    co.close()

def get_json_output_for_device(name, db):
    co = sqlite3.connect(db)
    co.row_factory = sqlite3.Row
    c = co.cursor()

    device = query_device(name, db)

    memories = _get_mem_json(name, device, c)
    cpu = _get_cpu_json(name, device, c)
    #interrupts = _get_interrupts_json(name, c)
  
    co.commit()
    co.close()
    out = dict()
    cpu_dict = dict()

    for k in cpu.keys():
        cpu_dict[k] = cpu[k]

    out["cpu"] = cpu_dict
    out["memory"] = memories
    #out["interrupts"] = interrupts
    return out

def _get_cpu_json(name, device, c):
    cpu_id = device["cpu_id"]

    select = build_select_statement(table="cpu",\
                                    columns_needed=["name, fpu, mpu, endianness"],\
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
                                    columns_needed=["id", "address", "size"],\
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
            temp["startup"] = 0
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
    

def query_device(name, db):
    co = sqlite3.connect(db)
    c = co.cursor()

    device = {}

    for row in TreeClimber(name, c):
        device = merge(device, row)

    #device_json = format_info_to_json(device)

    co.commit()
    co.close()

    #return device_json
    return device 

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

        self.parser.add_argument('name', nargs='?',\
            help="additional argument for certain commands")

        self.parser.add_argument("database",
            help="path to the database file",
            type=str)

        self.arguments = self.parser.parse_args()

        callback_args = list()
        if self.commands[self.arguments.command].argument_needed:
            callback_args.append(self.arguments.name)
        callback_args.append(self.arguments.database)
        self.commands[self.arguments.command].callback(*callback_args)


# Used to iterate over a table from the python world.
class Table:
    def __init__(self, name, cursor):
        self.cursor = cursor
        self.dic = {}
        request = '''SELECT * FROM %s;'''
        request = request % name
        self.cursor.execute(request)
        self.names = [description[0] for description in self.cursor.description]
        for name in self.names:
            self.dic[name] = None

    def __iter__(self):
        return self

    def next(self):
        tmp = self.cursor.next()
        i = 0
        for name in self.names:
            self.dic[name] = tmp[i]
            i += 1
        return self.dic

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
                SELECT *
                FROM tree WHERE name IS ?;
                '''
        self.request = '''
                SELECT *
                FROM tree WHERE id IS ?;
                '''
        args = [name]
        self.cursor.execute(self.start_request, args)
        result = self.cursor.fetchone()
        self.node = {}
        self.prev_node = {}
        self.node["id"] = result[0]
        self.node["parent_id"] = result[1]
        self.node["kind"] = result[2]
        self.node["name"] = result[3]
        #self.node["version"] = result[4]
        self.node["startup_memory_id"] = result[4]
        self.node["cpu_id"] = result[5]

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

        self.node["id"] = result[0]
        self.node["parent_id"] = result[1]
        self.node["kind"] = result[2]
        self.node["name"] = result[3]
        #self.node["version"] = result[4]
        self.node["startup_memory_id"] = result[4]
        self.node["cpu_id"] = result[5]
        return self.prev_node



