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
import urllib2
import ssl
from StringIO import StringIO

from itertools import izip
from itertools import tee
from sets import Set

from packaging.version import Version

from string import Template
from xml.etree import cElementTree

def entry_from_cmdline():
    cmd = CommandLine()

    # Commands to query infos from the database.
    cmd.register_command("packages", False, get_packages)

    cmd.register_command("families_of_package", True, get_families_of_package)

    cmd.register_command("subfamilies_of_family", True, get_subfamilies_of_family)

    cmd.register_command("devices_of_subfamily", True, get_devices_of_subfamily)

    cmd.register_command("devices_of_package", True, get_devices_of_package)

    cmd.register_command("devices_of_family", True, get_devices_of_family)

    cmd.register_command("device_infos", True, get_device_infos)

    # Commands that modify the database.
    cmd.register_command("init", False, init_db)

    cmd.register_command("add_package", True, add_package)

    cmd.register_command("update_all_packages", False, update_all_packages)

    cmd.register_command("update_package", True, update_package)

    result = cmd.execute()

    if result is None:
        return

    # In case of a list, we display line by line to ease
    # so that from a bash script we can use for on each line easily.
    if type(result) is list:
        for elt in result:
            print elt
    else:
        print result

## Query functions ##

def get_packages(c):
    return query_attributes(c, ["name"], "package")

def get_families_of_package(package_name, c):
    return query_fields_from_root_table(c,\
               ["package", "family"], ["name"], package_name)

def get_subfamilies_of_family(family_name, c):
    return query_fields_from_root_table(c,\
               ["family", "subfamily"], ["name"], family_name)

def get_devices_of_subfamily(subfamily, c):
    return query_fields_from_root_table(c,\
               ["subfamily", "device"], ["name"], subfamily_name)

def get_devices_of_family(family_name, c):
    return query_fields_from_root_table(c,\
               ["family", "subfamily", "device"], ["name"], family_name)\
           or query_fields_from_root_table(c,\
               ["family", "device"], ["name"], family_name)

def get_devices_of_package(package_name, c):
    return query_fields_from_root_table(c,\
                ["package", "family", "device"], ["name"], package_name)\
           or query_fields_from_root_table(c,\
                ["package", "family", "subfamily", "device"], ["name"], package_name)

def query_attributes(c, attributes, table):
    select = build_select_statement(table, attributes)
    c.execute(select)
    names = c.fetchall()
    return [name for tuple in names for name in tuple]

"""
Will query all `fields` from the final table in `tables`, traversing the list
of tables `tables` to get all possible values. It builds a statement joining
the multiple tables, then executing it.
"""
def query_fields_from_root_table(c, tables, fields, start_name):
    statement = '''SELECT '''\
                + ','.join([tables[-1] +'.'+ field for field in fields])\
                + ''' FROM ''' + tables[0]

    # We build an inner join for the intermediate table
    # and the next table to join.
    join = Template(" INNER JOIN ${join_table}\
                    on ${join_table}.${src}_id IS ${src}.id\
                    INNER JOIN ${dest}\
                    on ${join_table}.${dest}_id IS ${dest}.id")

    for left_table,right_table in pairwise(tables):
        statement = statement\
            + join.substitute(join_table=left_table + '_to_' + right_table,\
                              src=left_table, dest=right_table)

    statement = statement + ''' WHERE %s.%s IS ?''' % (tables[0], "name")
    #print "CURSOR", c
    #print "STATEMENT:", statement
    #print "NAME:", start_name
    result = c.execute(statement, [start_name]).fetchall()
    return [name for tuple in result for name in tuple]


def get_device_infos(device_name, c):
    device = dict()
    device["device"] = dict()
    device["device"]["memory"] = list()
    device["device"]["cpu"] = dict()
    device["interrupt"] = dict()

    cpu_query = '''SELECT cpu.name, cpu.fpu FROM device
                    INNER JOIN cpu ON device.cpu_id IS cpu.id
                    WHERE device.name is ?'''

    startup_memory_query = '''SELECT memory.size, memory.address, memory.name FROM device\
        INNER JOIN memory ON device.startup_memory_id IS memory.id\
        WHERE device.name IS ?'''

    memory_query = '''SELECT memory.size, memory.address, memory.name FROM device\
        INNER JOIN memory_to_device ON device.id IS memory_to_device.device_id\
        INNER JOIN memory ON memory.id IS memory_to_device.memory_id\
        WHERE device.name IS ? AND memory.id IS NOT device.startup_memory_id'''

    interrupts_query = '''SELECT interrupt.interrupt_index, interrupt.name FROM device\
        INNER JOIN svd_to_device ON device.id IS svd_to_device.device_id\
        INNER JOIN svd ON svd.id IS svd_to_device.svd_id\
        INNER JOIN interrupt_to_svd ON svd.id IS interrupt_to_svd.svd_id\
        INNER JOIN interrupt ON interrupt.id IS interrupt_to_svd.interrupt_id\
        WHERE device.name IS ?'''

    cpu = c.execute(cpu_query, [device_name]).fetchone()
    device["device"]["cpu"]["name"] = cpu["name"]
    device["device"]["cpu"]["fpu"] = 1 if cpu["fpu"] or cpu["fpu"].contains("fpu") else 0

    startup_memory = c.execute(startup_memory_query, [device_name]).fetchone()
    mem_to_add = dict()
    mem_to_add["size"] = startup_memory["address"]
    mem_to_add["address"] = startup_memory["size"]
    mem_to_add["name"] = startup_memory["name"]
    mem_to_add["startup"] = 1
    device["device"]["memory"].append(mem_to_add)

    for interrupt in c.execute(interrupts_query, [device_name]).fetchall():
        device["interrupt"][interrupt[0]] = interrupt[1]

    for memory in c.execute(memory_query, [device_name]).fetchall():
        mem_to_add = dict()
        mem_to_add["size"] = memory["address"]
        mem_to_add["address"] = memory["size"]
        mem_to_add["name"] = memory["name"]
        device["device"]["memory"].append(mem_to_add)

    return json.dumps(device)

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

        #print self.arguments
        if self.commands[self.arguments.command].argument_needed:
            if self.arguments.argument is not None:
                callback_args.append(self.arguments.argument)
            else:
                print "Missing argument."
                sys.exit(-1)

        db = self.arguments.database
        full_path_db = os.path.abspath(db)

        # Opens the database.
        co = sqlite3.connect(full_path_db)
        co.row_factory = sqlite3.Row

        c = co.cursor()
        # We enable foreign key checking.
        c.execute("PRAGMA foreign_keys = ON")

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

## Functions to modify the database ##

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
                      id    INTEGER PRIMARY KEY,
                      title TEXT,
                      path  TEXT,
                      UNIQUE (path, title));''')

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
                    name              TEXT,
                    startup_memory_id INTEGER REFERENCES memory(id),
                    cpu_id            INTEGER REFERENCES cpu(id),
                    UNIQUE (name, startup_memory_id, cpu_id));''')

    c.execute('''CREATE TABLE IF NOT EXISTS board (
                      id         INTEGER PRIMARY KEY,
                      name       TEXT,
                      device_id  INTEGER REFERENCES device(id) ON DELETE CASCADE,
                      UNIQUE (name, device_id));''')

    # Table to check if a certain package has been dowloaded and its content
    # available in the knowledge database.
    # family_id is the index to the top of the hierarchy for the package.
    c.execute('''CREATE TABLE IF NOT EXISTS package (
                      id        INTEGER PRIMARY KEY,
                      name      TEXT,
                      version   TEXT,
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

    # Package <-> Family
    c.execute('''CREATE TABLE IF NOT EXISTS package_to_family (
                      id         INTEGER PRIMARY KEY,
                      family_id  INTEGER NOT NULL REFERENCES family(id),
                      package_id INTEGER NOT NULL REFERENCES package(id),
                      UNIQUE (family_id, package_id));''')

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
                      device_id INTEGER NOT NULL REFERENCES device(id),
                      UNIQUE (memory_id, device_id));''')

    # SVD <-> Device
    c.execute('''CREATE TABLE IF NOT EXISTS svd_to_device (
                      id        INTEGER PRIMARY KEY,
                      svd_id    INTEGER REFERENCES svd(id),
                      device_id INTEGER REFERENCES device(id),
                      UNIQUE (svd_id, device_id));''')

    # Interrupt <-> SVD
    c.execute('''CREATE TABLE IF NOT EXISTS interrupt_to_svd (
                      id           INTEGER PRIMARY KEY,
                      interrupt_id INTEGER REFERENCES interrupt(id),
                      svd_id       INTEGER REFERENCES svd(id),
                      UNIQUE (interrupt_id, svd_id));''')

    # We delete the tree describing a package.
    # For board deletion, we take care of it with the constraints.
    c.execute('''
       CREATE TRIGGER delete_package
           AFTER DELETE on package
           BEGIN
               DELETE FROM package_to_family
                   WHERE package_to_family.package_id IS OLD.id;
           END;''')

    c.execute('''
        CREATE TRIGGER delete_package_to_family
            AFTER DELETE on package_to_family
            BEGIN
                DELETE FROM family
                    WHERE family.id IS OLD.family_id;
            END;''')

    c.execute('''
        CREATE TRIGGER delete_family
            AFTER DELETE on family
            BEGIN
                DELETE FROM family_to_subfamily
                    WHERE family_to_subfamily.family_id IS OLD.id;

                DELETE FROM family_to_device
                    WHERE family_to_device.family_id IS OLD.id;

           END;''')

    c.execute('''
        CREATE TRIGGER delete_family_to_subfamily
            AFTER DELETE on family_to_subfamily
            BEGIN
                DELETE FROM subfamily
                    WHERE subfamily.id IS OLD.subfamily_id;

                DELETE FROM subfamily_to_device
                    WHERE subfamily_to_device.subfamily_id IS OLD.subfamily_id;
            END;''')

    c.execute('''
        CREATE TRIGGER delete_subfamily_to_device
            AFTER DELETE on subfamily_to_device
            BEGIN
                DELETE FROM device
                    WHERE device.id IS OLD.device_id;
            END;''')


    c.execute('''
        CREATE TRIGGER delete_device
            AFTER DELETE on device
            BEGIN
                DELETE FROM memory_to_device
                    WHERE memory_to_device.device_id IS OLD.id;

                DELETE FROM svd_to_device
                    WHERE svd_to_device.device_id IS OLD.id;

                DELETE FROM documentation_to_device
                    WHERE documentation_to_device.device_id IS OLD.id;

                DELETE FROM cpu
                    WHERE cpu.id IS OLD.cpu_id
                        AND cpu.id NOT IN (SELECT cpu_id FROM device);
            END;''')

    c.execute('''
        CREATE TRIGGER delete_memory_to_device
            AFTER DELETE on memory_to_device
            BEGIN
                DELETE FROM memory
                    WHERE (memory.id IS OLD.memory_id
                        AND memory.id NOT IN (SELECT memory_id FROM memory_to_device));
            END;''')

    c.execute('''
        CREATE TRIGGER delete_svd_to_device
            AFTER DELETE on svd_to_device
            BEGIN
                DELETE FROM svd
                    WHERE (svd.id IS OLD.svd_id
                        AND svd.id NOT IN (SELECT svd_id FROM svd_to_device));

                DELETE FROM interrupt_to_svd
                    WHERE interrupt_to_svd.svd_id IS OLD.svd_id;
            END;''')

    c.execute('''
        CREATE TRIGGER delete_interrupt_to_svd
            AFTER DELETE ON interrupt_to_svd
            BEGIN
                DELETE FROM interrupt
                    WHERE (interrupt.id IS OLD.interrupt_id
                        AND interrupt.id NOT IN (SELECT interrupt_id FROM interrupt_to_svd));
            END;''')

    c.execute('''
        CREATE TRIGGER delete_doc_to_device
            AFTER DELETE ON documentation_to_device
            BEGIN
                DELETE FROM documentation
                    WHERE (documentation.id IS OLD.documentation_id
                        AND documentation.id NOT IN (SELECT documentation_id FROM documentation_to_device));
            END;''')

def build_insert(table, columns):
    insert = '''INSERT OR IGNORE INTO %s (''' + ','.join(columns)\
        + ''') VALUES ('''\
        + ','.join(['?' for k in columns]) + ''')'''
    insert = insert % table
    return insert

def insert_into(c, table, columns):
    insert = build_insert(table, columns.keys())
    print "VALUES:", columns.values()
    print "INSERT:", insert
    c.execute(insert, columns.values())

def insert_and_get_id(c, table, columns):
    insert_into(c, table, columns)
    select = build_select_statement(table, ["id"], columns.keys())
    print "VALUES:", columns.values()
    print "SELECT:", select
    return c.execute(select, columns.values()).fetchone()[0]

def get_nodes_from_xml(handle, name):
    events = cElementTree.iterparse(handle, events=("start", "end",))
    _, root = next(events)  # Grab the root element.
    for event, elem in events:
        if event == "end" and elem.tag == name:
            yield elem
            root.clear()  # Free up memory by clearing the root element.

def search_in(c, table, columns_needed, columns_conditions=dict(), nb_items=1):
    statement = '''SELECT ''' + ','.join(columns_needed) + ''' FROM ''' + table
    number_of_results = ''' ORDER BY id DESC LIMIT %s''' % nb_items
    conditions = ""
    if columns_conditions != dict():
        conditions = ''' WHERE ''' +\
            ' and '.join([column + " IS ?" for column in columns_conditions.keys()])

    query = statement + conditions + number_of_results

    #print "VALUES:", columns_conditions.values()
    #print "QUERY:", query

    result = c.execute(query, columns_conditions.values()).fetchall()
    if nb_items == 1 and result:
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
        result["memory"].append(dic)

    for proc in node.findall("processor"):
        for k,v in proc.attrib.items():
            result["cpu"][k] =\
                v.replace('-endian', '').lower()

    for doc in node.findall("book"):
        dic = {}
        for k,v in doc.attrib.items():
            dic[k.replace('name', 'path')] = v.replace('title', 'name')
        result["documentation"].append(dic)

    return result

def pairwise(iterable):
    "s -> (s0,s1), (s1,s2), (s2, s3), ..."
    a, b = tee(iterable)
    next(b, None)
    return izip(a, b)

def add_svd(c, unzip_dir, svd, device_id):

    # FIXME For now only works on Linux
    f = ntpath.join(unzip_dir, svd).replace('\\', '/')
    svd_name = ntpath.basename(svd)[:-4]
    svd_id = search_in(c, "svd", ["id"], {"name" : svd_name})
    #print "SVD_ID:", svd_id
    if svd_id:
        # SVD file is already in database,
        # we connect the device to the already present svd.
        insert_into(c, "svd_to_device",\
            {"svd_id"    : svd_id[0],
             "device_id" : device_id})

        return svd_id
    else: # SVD is absent from the database
        svd_id = insert_and_get_id(c, "svd", {"name" : svd_name})
        insert_into(c, "svd_to_device",\
            {"svd_id"    : svd_id,
             "device_id" : device_id})

        dict_interrupts = {}
        for interrupt in get_nodes_from_xml(f, "interrupt"):
            interrupt_name = interrupt.find("name").text
            interrupt_index = interrupt.find("value").text
            dict_interrupts[interrupt_index] = interrupt_name
            int_id = insert_and_get_id(c, "interrupt",\
                {"name" : interrupt_name,
                 "interrupt_index" : interrupt_index})

            insert_into(c, "interrupt_to_svd",\
                {"svd_id" : svd_id, "interrupt_id" : int_id})


"""
Converts a string attribute representing fpu, mpu or the clock
in an integer form.
"""
def get_string_attrib(cpu, name):
    if name in cpu.keys():
        if name.replace('D', '') in cpu[name]:
            return 1
        else:
            return cpu[name]
    return 0

"""
Adds all components of a device to the database and
connects them to the device table.
"""
def add_device(c, unzip_dir, device_repr, parent_ids):
    #print "DEV REPR:", device_repr
    cpu = device_repr["cpu"]
    #print "CPU ATTRIBUTES:", cpu.keys()
    cpu_id = insert_and_get_id(c, "cpu",
        {"name"       : cpu["Dcore"],
         "fpu"        : get_string_attrib(cpu, "Dfpu"),
         "mpu"        : get_string_attrib(cpu, "Dmpu"),
         "clock"      : get_string_attrib(cpu, "Dclock"),
         "endianness" : cpu["Dendian"]})

    # We add all memories and get the default startup memory.
    startup_mem_id = 0
    memory_ids = list()
    memories = device_repr["memory"]
    for mem in memories:
        if "startup" in mem.keys() and mem["startup"]:
            startup_mem_id = insert_and_get_id(c, "memory",
                {"address" : mem["start"],
                "size"     : mem["size"],
                "name"     : mem["name"]})
            memory_ids.append(startup_mem_id)
        else:
            memory_ids.append(insert_and_get_id(c, "memory",
                                    {"address" : mem["start"],
                                    "size"     : mem["size"],
                                    "name"     : mem["name"]}))

    if not startup_mem_id:
        raise Exception("Malformed device %s has no startup memory." %\
                         device_repr["name"])

    device_id = insert_and_get_id(c, "device",
        {"name"              : device_repr["name"],
         "cpu_id"            : cpu_id,
         "startup_memory_id" : startup_mem_id,
        })
    # Connect the memory table with the device table.
    for mem_id in memory_ids:
         insert_into(c, "memory_to_device",\
            {"memory_id" : mem_id,
             "device_id"  : device_id})

    # Add the SVD to the SVD device table.
    svd_id = add_svd(c, unzip_dir, device_repr["svd"], device_id)

    # Add the documentation to the intermediate table.
    for book in device_repr["documentation"]:
        book_id = insert_and_get_id(c, "documentation", book)
        insert_into(c, "documentation_to_device",\
            {"documentation_id" : book_id , "device_id" : device_id})

    parent_ids["device"] = device_id

    # We connect the different ids using the intermediate tables.
    for left_table,right_table in pairwise(reversed(parent_ids.keys())):
        table = "%s_to_%s" % (left_table, right_table)
        insert_into(c, table,\
            {"%s_id" % left_table : parent_ids[left_table],
             "%s_id" % right_table : parent_ids[right_table]})

    # add cpu


def add_pdsc_to_database(c, unzipped_dir, f):
    #TODO: Factorize this code.
    family_id = 1
    family_repr = dict()
    family_repr["cpu"] = dict()
    family_repr["memory"] = list()
    family_repr["documentation"] = list()

    for family in get_nodes_from_xml(f, "family"):
        family_name = family.attrib["Dfamily"]
        family_id = insert_and_get_id(c, "family", {"name" : family_name})
        family_repr = get_and_override_attributes(family_repr, family)
        parent_ids = {"family" : family_id}

        #TODO Use .findall() for immediates children instead of .find and an if
        for subfamily in family.findall("subFamily"):
            subfamily_name = subfamily.attrib["DsubFamily"]
            subfamily_id = insert_and_get_id(c, "subfamily",\
                {"name" : subfamily_name})
            subfamily_repr = get_and_override_attributes(family_repr, subfamily)

            parent_ids["subfamily"] = subfamily_id

            for device in subfamily.iter("device"):
                device_repr = get_and_override_attributes(subfamily_repr, device)
                add_device(c, unzipped_dir, device_repr, parent_ids)

        for device in family.findall("device"):
            device_repr = get_and_override_attributes(family_repr, device)
            add_device(c, unzipped_dir, device_repr, parent_ids)


    board_requests = list()
    for elt in get_nodes_from_xml(f, "boards"):
        for board in elt.iter("board"):
            board_name = board.attrib["name"]
            # We get the first item of the generator.
            dev_name = board.iter("mountedDevice").next().attrib["Dname"]
            # TODO: In the case of the pack ABOV.CM3, the mounted Device
            # and not a specific device, we have to handle that case.
            dev = search_in(c, "device", ["id"], {"name" : dev_name})
            print "DEV:", dev
            board_requests.append((board_name,  dev['id']))

    #We insert the boards.
    statement = build_insert("board", ["name", "device_id"])

    for req in board_requests:
        c.execute(statement, req)

    return family_id

def update_all_packages(c):
    packages = get_packages(c)
    download_necessary_packages(Set(packages), c)

def update_package(name, c):
    download_necessary_packages(Set([name]), c)

def selected_packages(handle, packages):
    events = cElementTree.iterparse(handle, events=("start", "end",))
    _, root = next(events)  # Grab the root element.
    for event, elem in events:
        if event == "end" and elem.tag == "pdsc":
            pack_name = '.'.join(elem.attrib["name"].split('.')[:-1])
            if pack_name in packages:
                yield elem
            root.clear()  # Free up memory by clearing the root element.

def format_url(package):
    name = package.attrib["name"]
    version = package.attrib["version"]
    url = package.attrib["url"]
    if not url.endswith('/'):
        url = url + '/'
    name = name[:-5]
    return url + name + '.' + version + '.pack'


def download_package(package):
    url = format_url(package)
    name = package.attrib["name"][:-5]
    version = package.attrib["version"]

    dl_url = format_url(package)

    #TODO: fix ssl certificate.
    #gcontext = ssl.create_default_context()
    gcontext = ssl._create_unverified_context()

    f = urllib2.urlopen(dl_url, context=gcontext)
    temp_file = name + "." + version +".pack"
    print "Downloading %s" % dl_url
    with open(temp_file, "wb") as local_file:
        local_file.write(f.read())
    print "Finished downloading %s" % dl_url
    return temp_file

def delete_package(name, c):
    pack_id = search_in(c, "package", ["id"], {"name" : name})[0]
    statement = '''
        DELETE
        FROM package
        WHERE package.name is ?
                '''
    c.execute(statement, [name])

def download_necessary_packages(packages, c):
    index = urllib2.urlopen('http://sadevicepacksprodus.blob.core.windows.net/idxfile/index.idx')
    index_page = index.read()
    xml = index_page
    # We format the xml document.
    xml = xml.split('\n')[0] + '\n'\
        + '<blah>' + '\n'.join(xml.split('\n')[1:]) + '</blah>'

    f = StringIO(xml)

    for package in selected_packages(f, packages):
        #version = package.attrib["version"]
        version = package.attrib["version"]
        name = '.'.join(package.attrib["name"].split('.')[:-1])
        print name, version
        current_version = search_in(c, "package", ["version"],\
                                    {"name" : name})
        if current_version:
            current_version = current_version[0]
            print "Package %s present" % name
            print "Current version %s" % current_version
            print "Distant version %s" % version

            current_version = Version(current_version)
            version = Version(version)
            if current_version >= version:
                print "Keeping old version"
            else:
                print "Overriding current version"
                local_file = download_package(package)
                delete_package(name, c)
                # No need to vacuum the database, add_package does it.
                add_package(os.path.abspath(local_file), c)
        else:
            pass
    # We get the index page from Keil.
    # We check the version of the package.
    # if its greater than the current package version we update it.
    pass

def add_package(path, c):
    #TODO: if there is an already present package dont add it
    #TODO: handle collisions.
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

        #print "PATH:", os.path.abspath(unzip_dir)
        family_id = add_pdsc_to_database(c, os.path.abspath(unzip_dir), pdsc)

        # We mark the package present in the database.
        #print "VERSION:", package_version
        #print "NAME:", package_name
        package_id = insert_and_get_id(c, "package",\
            {"name" : package_name,
             "version" : package_version})

        insert_into(c, "package_to_family",
           {"family_id"  : family_id,
            "package_id" : package_id})

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


