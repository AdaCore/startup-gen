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
from itertools import izip
from itertools import tee

from xml.etree import cElementTree

def entry_from_cmdline():
    cmd = CommandLine()

    # Commands to query infos from the database.
    cmd.register_command("packages", False,\
        lambda c: query_attributes(["name"],"package", c))
    cmd.register_command("device_info", True, get_json_output_of_device)
    cmd.register_command("package_of_device", True, query_package_of_device)
    #cmd.register_command("devices_of_package", True, query_devices_of_package)
    cmd.register_command("cpu_of_device", True, query_cpu_of_device)
    cmd.register_command("devices", False, query_devices)

    # Commands that modify the database.
    cmd.register_command("init", False, init_db)
    cmd.register_command("add_package", True, add_package)
    cmd.register_command("update_packages", False, update_packages)
    cmd.register_command("update_package", True, update_package)

    result = cmd.execute()
    if type(result) is list:
        for elt in result:
            print elt
    else:
        print result

## Query functions ##

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

## Functions to modify the database ##

def update_all_packages(c):
    #TODO:
    pass

def init_db(c):
    ## CONTENT TABLES ##

    # We dont want duplicate info in the memory table.
    c.execute('''CREATE TABLE IF NOT EXISTS memory (
                    id      INTEGER PRIMARY KEY,
                    address INTEGER,
                    size    INTEGER,
                    name    TEXT,
                    UNIQUE (address, size, name));''')
    # CPU table
    # We use the restricted integers as booleans.
    c.execute('''CREATE TABLE IF NOT EXISTS cpu (
                      id         INTEGER PRIMARY KEY,
                      name       TEXT,
                      fpu        INTEGER NOT NULL CHECK (fpu in (0,1)),
                      mpu        INTEGER NOT NULL CHECK (mpu in (0,1)),
                      endianness TEXT NOT NULL CHECK
                          (endianness in ("little","big")),
                      clock      INTEGER NOT NULL,
                      UNIQUE (name,endianness,fpu,mpu,clock));''')

    c.execute('''CREATE TABLE IF NOT EXISTS interrupt (
                      id              INTEGER PRIMARY KEY,
                      name            TEXT,
                      interrupt_index INTEGER NOT NULL,
                      UNIQUE (name, interrupt_index));''')

    # Documentation
    c.execute('''CREATE TABLE IF NOT EXISTS documentation (
                      id         INTEGER PRIMARY KEY,
                      name       TEXT,
                      path       TEXT,
                      UNIQUE (name, path));''')

    c.execute('''CREATE TABLE IF NOT EXISTS family (
                    id                INTEGER PRIMARY KEY,
                    name              TEXT,
                    UNIQUE (name));''')

    c.execute('''CREATE TABLE IF NOT EXISTS subfamily (
                    id                INTEGER PRIMARY KEY,
                    name              TEXT,
                    UNIQUE (name));''')

    c.execute('''CREATE TABLE IF NOT EXISTS device (
                    id                INTEGER PRIMARY KEY,
                    kind              TEXT,
                    name              TEXT,
                    startup_memory_id INTEGER REFERENCES memory(id),
                    cpu_id            INTEGER REFERENCES cpu(id),
                    UNIQUE (name, startup_memory_id, cpu_id));''')

    c.execute('''CREATE TABLE IF NOT EXISTS board (
                      id      INTEGER PRIMARY KEY,
                      name    TEXT,
                      device  INTEGER NOT NULL REFERENCES tree(id),
                      UNIQUE (name));''')

    # Table to check if a certain package has been dowloaded and its content
    # available in the knowledge database.
    # family_id is the index to the top of the hierarchy for the package.
    c.execute('''CREATE TABLE IF NOT EXISTS package (
                      id        INTEGER PRIMARY KEY,
                      name      TEXT,
                      version   TEXT,
                      family_id INTEGER REFERENCES family(id),
                      UNIQUE (name));''')

    # SVD files from which we have stored the interrupts
    c.execute('''CREATE TABLE IF NOT EXISTS svd (
                      id   INTEGER PRIMARY KEY,
                      name TEXT NOT NULL,
                      UNIQUE (name));''')

    c.execute('''CREATE TABLE IF NOT EXISTS family (
                      id   INTEGER PRIMARY KEY,
                      name TEXT NOT NULL,
                      UNIQUE (name));''')

    c.execute('''CREATE TABLE IF NOT EXISTS subfamily (
                      id   INTEGER PRIMARY KEY,
                      name TEXT NOT NULL,
                      UNIQUE (name));''')


    ## INTERMEDIATE TABLES ##

    # Family <-> Subfamily
    c.execute('''CREATE TABLE IF NOT EXISTS family_to_subfamily (
                      id           INTEGER PRIMARY KEY,
                      family_id    INTEGER NOT NULL REFERENCES family(id),
                      subfamily_id INTEGER NOT NULL REFERENCES subfamily(id),
                      UNIQUE (family_id, subfamily_id));''')

    # Subfamily <-> Device
    c.execute('''CREATE TABLE IF NOT EXISTS subfamily_to_device (
                      id           INTEGER PRIMARY KEY,
                      subfamily_id INTEGER NOT NULL REFERENCES subfamily(id),
                      device_id    INTEGER NOT NULL REFERENCES device(id),
                      UNIQUE (subfamily_id, device_id));''')


    # In certain cases, we do not have subfamilies.
    # Family <-> Device
    c.execute('''CREATE TABLE IF NOT EXISTS family_to_device (
                      id        INTEGER PRIMARY KEY,
                      family_id INTEGER NOT NULL REFERENCES family(id),
                      device_id INTEGER NOT NULL REFERENCES device(id),
                      UNIQUE (family_id, device_id));''')

    # Documentation <-> Device
    c.execute('''CREATE TABLE IF NOT EXISTS documentation_to_device (
                      id               INTEGER PRIMARY KEY,
                      documentation_id INTEGER NOT NULL REFERENCES documentation(id),
                      device_id        INTEGER NOT NULL REFERENCES device(id),
                      UNIQUE (documentation_id, device_id));''')

    # Memory <-> Device
    c.execute('''CREATE TABLE IF NOT EXISTS memory_to_device (
                      id        INTEGER PRIMARY KEY,
                      memory_id INTEGER NOT NULL REFERENCES memory(id),
                      device_id   INTEGER NOT NULL REFERENCES device(id),
                      UNIQUE (memory_id, device_id));''')

    # SVD <-> Device
    c.execute('''CREATE TABLE IF NOT EXISTS svd_to_device (
                      id              INTEGER PRIMARY KEY,
                      svd_id    INTEGER REFERENCES svd(id),
                      device_id         INTEGER REFERENCES device(id),
                      UNIQUE (svd_id, device_id));''')

    # Interrupt <-> SVD
    c.execute('''CREATE TABLE IF NOT EXISTS interrupt_to_svd (
                      id           INTEGER PRIMARY KEY,
                      interrupt_id INTEGER REFERENCES interrupt(id),
                      svd_id       INTEGER REFERENCES svd(id),
                      UNIQUE (interrupt_id, svd_id));''')

def build_insert(table, columns):
    insert = '''INSERT OR IGNORE INTO %s (''' + ','.join(columns.keys())\
        + ''') VALUES ('''\
        + ','.join(['?' for k in columns.keys()]) + ''')'''
    insert = insert % table
    return insert
 
def insert_into(c, table, columns):
    insert = build_insert(table, columns)
    print "VALUES:", columns.values()
    print "INSERT:", insert
    c.execute(insert, columns.values())

def insert_and_get_id(c, table, columns):
    insert_into(c, table, columns)
    select = build_select_statement(table, ["id"], columns.keys())
    print "SELECT:", select
    return c.execute(select, columns.values()).fetchone()[0]

def get_nodes_from_xml(handle, name):
    events = cElementTree.iterparse(handle, events=("start", "end",))
    _, root = next(events)  # Grab the root element.
    for event, elem in events:
        if event == "end" and elem.tag == name:
            yield elem
            root.clear()  # Free up memory by clearing the root element.

def search_in(c, table, columns_needed, columns_conditions, number=1):
    statement = '''SELECT ''' + ','.join(columns_needed) + ''' FROM ''' + table
    number_of_results = ''' ORDER BY id DESC LIMIT %s''' % number
    conditions = ""
    if columns_conditions != list():
        conditions = ''' WHERE ''' +\
            ' and '.join([column + " IS ?" for column in columns_conditions])

    query = statement + conditions + number_of_results

    result = c.execute(query, columns_conditions.values()).fetchall()
    if number == 1:
        return result[0]
    return result
 
"""
Returns the next available id in the tree table.
We have to do that because the node has its parent id in its columns,
so for the insertion of a family we have to know the id that will be inserted.
"""
def get_next_id(c):
    c.execute('''SELECT id FROM tree ORDER BY id DESC LIMIT 1''')
    next_id = 1
    result = c.fetchone()
    if result != None:
        next_id = next_id + result[0]
    return next_id

def get_and_override_attributes(dev_repr, node):
    result = copy.deepcopy(dev_repr)
    if node.tag == "device":
        result["name"] = node.attrib["Dname"]

    for svd_node in node.findall("debug"):
        result["svd"] = svd_node.attrib["svd"]

    for mem in node.findall("memory"):
        dic = {}
        for k,v in mem.attrib.items():
            k = k.replace('id', 'name').replace(r'start\b', 'address')
            v = v
            dic[k] = v
            print "%s <=> %s" % (k, v)
        result["memory"].append(dic)

    for proc in node.findall("processor"):
        for k,v in proc.attrib.items():
            result["cpu"][k] =\
                v.replace('-endian', '').lower()
            print "%s <=> %s" % (k, v)

    for doc in node.findall("book"):
        dic = {}
        for k,v in doc.attrib.items():
            dic[k.replace('name', 'path')] = v.replace('title', 'name')
            print "%s <=> %s" % (k, v)
        result["documentation"].append(dic)

    return result 

def pairwise(iterable):
    "s -> (s0,s1), (s1,s2), (s2, s3), ..."
    a, b = tee(iterable)
    next(b, None)
    return izip(a, b)

def add_device(c, device_repr, parent_ids):
    print "DEV REPR:", device_repr
    cpu = device_repr["cpu"]
    cpu_id = insert_and_get_id(c, "cpu",
        {"name"       : cpu["Dcore"],
         "fpu"        : cpu["Dfpu"],
         "mpu"        : cpu["Dmpu"],
         "clock"      : cpu["Dclock"],
         "endianness" : cpu["Dendian"]})

    startup_mem_id = 0
    memory_ids = list()
    memories = device_repr["memory"]
    for mem in memories:
        print mem.keys()
        if "startup" in mem.keys() and mem["startup"]:
            startup_mem_id = insert_and_get_id(c, "memory",
                {"address" : mem["start"],
                "size"     : mem["size"],
                "name"     : mem["name"]})
            memory_ids.append(startup_mem_id)
        else:
            memory_ids.append(insert_into(c, "memory",
                            {"address" : mem["start"],
                            "size"     : mem["size"],
                            "name"     : mem["name"]}))

    if not startup_mem_id:
        raise Exception("Malformed device %s has no startup memory." % device_repr["name"])

    # Insert device
    # connect the tables

    device_id = 1
    parent_ids["device"] = device_id
    print "IDS:", parent_ids

    # We connect the different ids using the intermediate tables.
    for l,r in pairwise(reversed(parent_ids.keys())):
        print "LEFT",l
        print "RIGHT",r
        table = "%s_to_%s" % (l, r)
        insert_into(c, table,\
            {"%s_id" % l : parent_ids[l],
             "%s_id" % r : parent_ids[r]})

 
    # add cpu


def add_pdsc_to_database(c, f):
    family_id = 1

    #TODO: Factorize this code.

    family_repr = dict()
    family_repr["cpu"] = dict()
    family_repr["memory"] = list()
    family_repr["documentation"] = list()

    for family in get_nodes_from_xml(f, "family"):
        family_name = family.attrib["Dfamily"]
        family_id = insert_and_get_id(c, "family", {"name" : family_name})
        family_repr = get_and_override_attributes(family_repr, family)
        parent_ids = {"family" : family_id}

        if family.find("subFamily"):
            for subfamily in family.iter("subFamily"):
                subfamily_name = subfamily.attrib["DsubFamily"]
                subfamily_id = insert_and_get_id(c, "subfamily",\
                    {"name" : subfamily_name})
                subfamily_repr = get_and_override_attributes(family_repr, subfamily)

                parent_ids["subfamily"] = subfamily_id

                for device in subfamily.iter("device"):
                    device_repr = get_and_override_attributes(subfamily_repr, device)
                    add_device(c, device_repr, parent_ids)
        else:
            for device in family.iter("device"):
                device_repr = get_and_override_attributes(family_repr, device)
                add_device(c, device_repr, parent_ids)


    board_requests = list()
    for elt in get_nodes_from_xml(f, "boards"):
        for board in elt.iter("board"):
            board_name = board.attrib["name"]
            # We get the first item of the generator.
            dev_name = board.iter("mountedDevice").next().attrib["Dname"]
            print "MOUNTED DEVICE:", dev_name
            #dev_id = search_in(c, "tree", ["id"], {"name" : dev_name})
            #board_requests.append({"name" : board_name, "dev_id" : dev_id})

    #We insert the boards.
    #c.executescript('\n'.join([build_insert("board", request)\
    #    for request in board_requests]))
    return family_id #TODO: return family id


def update_packages(path, c):
    #TODO
    pass

def update_package(path, c):
    #TODO
    # Do we update all packages forcefully ?
    # Dow we keep unzip archives ?
    pass

def add_package(path, c):
    #TODO: if there is an already present package dont add it
    # instead we print on the command line the version of the current package.
    # unzip add pdsc
    # Do we keep unzip archives ?

    unzip_dirs = get_unzipped_paths([path])
    for unzip_dir in unzip_dirs:
        pdsc_pattern = os.path.join(unzip_dir, "*.pdsc")
        pdsc = glob.glob(pdsc_pattern)[0]

        package = ntpath.basename(path)
        package_name = '.'.join(package.split('.')[0:2])
        package_version = '.'.join(package.split('.')[2:-1])

        family_id = add_pdsc_to_database(c, pdsc)

        # We mark the package present in the database.
        #TODO: handle collisions.
#        insert_into(c, "package",\
#            {"name" : package_version,
#             "version" : package_version,
#             "family_id" : family_id})

    # Vacuum the database.
    c.execute('''VACUUM''')

    # cleanup(unzip_dirs)


def get_unzipped_paths(file_list, tmp_dir=".tmp"):
    # Unzip all files in a temporary directory
    # The path to the unzipped files.
    # Create the temporary directory.
    if not (os.path.isdir(tmp_dir)):
        os.makedirs(tmp_dir)

    paths = list()
    for f in file_list:
        unzip_dir = os.path.join(tmp_dir, os.path.basename(f)[:-5])
        os.makedirs(unzip_dir)

        # Unzip the archive.
        try:
            print "Unzipping file %s." % f 
            subprocess.check_output(['unzip', '-d', unzip_dir, f],\
                                             close_fds=True)
            print "Done."
            paths.append(unzip_dir)
        except Exception as e:
            print "Could not unzip %s.", unzip_dir
            print e

    return paths

"""
Removes the unzipped packages.
"""
def cleanup(file_list):
    for f in file_list:
        shutil.rmtree(f)


