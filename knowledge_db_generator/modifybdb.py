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
    parser = argparse.ArgumentParser()
    parser.add_argument('command',
        choices=['init', 'add_package'],
        help="commands")
    parser.add_argument('name', nargs='?',
        help="device/board name or the package path, depending on the context")

    parser.add_argument("database",
        help="path to the database file",
        type=str)

    arguments = parser.parse_args()
    if (command_check(arguments)):
        command_dispatch(arguments)

def command_check(args):
    if args.command == "init" and args.name is not None:
        print "init takes no parameter."
        return False

    if args.command == "add_package" and args.name is None:
        print "Need a package path."
        return False

    return True

## COMMANDS ##

def add_package(name, db):
    _add_package(name, db)

def init(unused, db):
    # Add the list of packages to the table package
    print "init with db:", db
    setup_db(db)

## END COMMANDS ##

def command_dispatch(args):
    functions = {
        "add_package" : add_package,
        "init"        : init
    }
    functions[args.command](args.name, args.database)

def help():
    print "HELP!"

def setup_db(db):
    co = sqlite3.connect(db)
    c = co.cursor()
    # We dont want dupplicate info in the memory table.
    c.execute('''CREATE TABLE IF NOT EXISTS memory (
                    id      INTEGER PRIMARY KEY,
                    address INTEGER,
                    size    INTEGER,
                    UNIQUE (address, size));''')
    # CPU table
    # We use the restricted integers as booleans.
    c.execute('''CREATE TABLE IF NOT EXISTS cpu (
                      id         INTEGER PRIMARY KEY,
                      name       TEXT,
                      fpu        INTEGER NOT NULL CHECK (fpu in (0,1)),
                      mpu        INTEGER NOT NULL CHECK (mpu in (0,1)),
                      endianness TEXT NOT NULL CHECK
                          (endianness in ("little","big")),
                      UNIQUE (name,endianness,fpu,mpu));''')

    c.execute('''CREATE TABLE IF NOT EXISTS interrupt (
                      id              INTEGER PRIMARY KEY,
                      interrupt_index INTEGER NOT NULL,
                      name            TEXT NOT NULL,
                      UNIQUE (interrupt_index, name));''')

    # Documentation
    c.execute('''CREATE TABLE IF NOT EXISTS documentation (
                      id         INTEGER PRIMARY KEY,
                      name       TEXT,
                      path       TEXT,
                      UNIQUE (name, path));''')

    # The different ids can be null, in that case
    # we need to go up the tree in order to get the desired info.
    # This table represents the tree of family/subfamily/device
    c.execute('''CREATE TABLE IF NOT EXISTS tree (
                    id                INTEGER PRIMARY KEY,
                    parent_id         INTEGER REFERENCES tree(id),
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
                      family_id INTEGER REFERENCES tree(id),
                      UNIQUE (name));''')

    # Documentation <-> Node
    c.execute('''CREATE TABLE IF NOT EXISTS documentation_to_node (
                      id               INTEGER PRIMARY KEY,
                      documentation_id INTEGER NOT NULL REFERENCES documentation(id),
                      node_id          INTEGER NOT NULL REFERENCES tree(id),
                      UNIQUE (documentation_id, node_id));''')

    # Memory <-> Node
    c.execute('''CREATE TABLE IF NOT EXISTS memory_to_node (
                      id        INTEGER PRIMARY KEY,
                      memory_id INTEGER NOT NULL REFERENCES memory(id),
                      node_id   INTEGER NOT NULL REFERENCES tree(id),
                      UNIQUE (memory_id, node_id));''')

    # Interrupt <-> Node
    c.execute('''CREATE TABLE IF NOT EXISTS interrupt_to_node (
                      id           INTEGER PRIMARY KEY,
                      interrupt_id INTEGER REFERENCES interrupt(id),
                      node_id      INTEGER REFERENCES tree(id),
                      UNIQUE (interrupt_id, node_id));''')

    co.commit()
    co.close()

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

def identity(obj, k, v):
    return k,v

def cpu_name_to_lower(obj, k, v):
    if k == "name":
        v = v.lower()
    if k == "endianness":
        v = v.split('-')[0].lower()
    if k == "fpu" or k == 'mpu':
        v = v if v is not None else 0
        if not isinstance(v, (int, long)):
            if "fpu" in v.lower() or "mpu" in v.lower():
                v = 1
            else:
                v = 0
    return k,v

"""
TODO DOCUMENTATION
"""
class Insertor:
    def __init__(self, cursor, table_name, attribute_column, pre_insertion=identity):
        self.cursor = cursor
        self.table = table_name
        self.attribute_column = attribute_column
        self.pre_insertion = pre_insertion
        self.values = {}

    def set_values(self,attributes):
        print "Attributes", attributes.items()
        for xml_attrib, sql_attrib in self.attribute_column.items():
            sql_value = attributes[xml_attrib].value\
                if xml_attrib in attributes.keys() else None
            sql_attrib, sql_value = self.pre_insertion(self, sql_attrib, sql_value)
            self.values[sql_attrib] = sql_value

    def insert(self):
        values = list()
        columns = list()
        for k,v in self.values.items():
            values.append(v)
            columns.append(k)
        insert_statement = ('''INSERT OR IGNORE INTO %s (''' + ','.join(columns) +
            ''') VALUES (''' + ','.join(['?'] * len(values)) + ''')''') % self.table

        self.cursor.execute(insert_statement, values)

        select_statement = build_select_statement(table = self.table,
                                                columns_needed = ["id"],
                                                columns_conditions = columns)

        self.cursor.execute(select_statement, values)
        element_id = self.cursor.fetchone()[0]
        return element_id

"""
"""
def insert_directly_linked_elements(elt, cursor):
    mapping_proc = {"Dcore"   : "name",
                    "Dfpu"    : "fpu",
                    "Dmpu"    : "mpu",
                    "Dendian" : "endianness"}

    mapping_startup_mem = {"start"   : "address",
                           "size"    : "size"}

    cpu_insertor = Insertor(cursor, "cpu", mapping_proc,
                            pre_insertion=cpu_name_to_lower)

    mem_insertor = Insertor(cursor, "memory", mapping_startup_mem)

    insert_map = {"processor" : cpu_insertor, "memory" : mem_insertor}
    inserted_keys = {"cpu_id" : None, "memory_id" : None}
    found = list()

    for tag in elt.childNodes:
        if ((tag.nodeName == "processor")      and\
           len(tag.attributes.keys()) > 1)     or\
           (tag.nodeName == "memory"           and\
            "startup" in tag.attributes.keys() and\
            tag.attributes["startup"].value == "1"):
            insertor = insert_map[tag.nodeName]
            insertor.set_values(tag.attributes)
            found.append(insertor)

    for i in found:
        inserted_keys[i.table + "_id"] = i.insert()
    return inserted_keys

def get_list_of_ids_to_connect(elt, cursor):
    global add_documentation

    mapping_mem = {"start" : "address",
                   "size"  : "size"}

    mapping_doc = {"name"  : "path",
                   "title" : "name"}

    tags = {"memory" : {"table" : "memory", "mapping" : mapping_mem},
            "book"   : {"table" : "documentation", "mapping" : mapping_doc}}

    ids = {"memory" : list(), "documentation" : list()}
    for tag in elt.childNodes:
        if tag.nodeName in tags.keys():
            table = tags[tag.nodeName]["table"]
            mapping = tags[tag.nodeName]["mapping"]
            i = Insertor(cursor, table, mapping)
            i.set_values(tag.attributes)
            ids[table].append(i.insert())

    return ids


"""
element_name is a dictionnary of the form
{name:<tag element that contains the name>}
"""
def add_device_info_to_database(elt, c, name_attribute, parent_id):
    name = elt.getAttribute(name_attribute)
    # We add the family to the DB
    # As the family is the top of the hierarchy, we need to get the
    # next id that will be used, in order to have id == parent_id
    # We add all its direct descendant with their parent id.

    # Returns the mapping {startup_mem : id, cpu : id}
    ids_node = insert_directly_linked_elements(elt, c)

    # List of ids to put in an intermediate table
    # to connect the tree node with the element in question
    # Returns a dictionary of the form:
    #   { "documentation" : (2, 3, 5, 91, 42), "memory" : (1, 2)}
    external_ids = get_list_of_ids_to_connect(elt, c)

    insert_request = '''INSERT INTO tree\
        (parent_id, kind, name, startup_memory_id, cpu_id)\
         VALUES (?,?,?,?,?)'''

    requested_values = [parent_id, "test", name,\
                        ids_node["memory_id"], ids_node["cpu_id"]]

    c.execute(insert_request, requested_values)


    # We get the id of the inserted item.
    columns = ["parent_id", "kind", "name", "startup_memory_id", "cpu_id"]
    select_statement = build_select_statement(table = "tree",
                                              columns_needed = ["id"],
                                              columns_conditions=columns)

    c.execute(select_statement, requested_values)
    result_id = c.fetchone()[0]

   # We insert the current infos that we have
    for table, row_ids in external_ids.items():
        for row_id in row_ids:
            insert_request =\
            '''INSERT INTO %s_to_node (%s_id, node_id) VALUES (?,?)'''\
            % (table, table)
            c.execute(insert_request, [row_id, result_id])

    return result_id

def add_subfamily_to_database(subfamily, c, name, parent_id):
    subfamily_id = add_device_info_to_database(subfamily, c, name, parent_id)
    device_list = subfamily.getElementsByTagName('device')
    for device in device_list:
        add_device_info_to_database(device, c, "Dname", subfamily_id)

def add_family_to_database(family, c):
    c.execute('''SELECT max(id) FROM tree''')


    # We fetch the first result.
    c.fetchone()

    result = c.fetchone()
    first_id = (result[0] if (result != None) else 0) + 1

    add_device_info_to_database(family, c, "Dfamily", first_id)

    # We get all subfamilies and add them to the tree table
    # if we have no "subFamily" tag we add the "device" tags directly.

    if family.getElementsByTagName('subFamily') != list():
        subfamily_list = family.getElementsByTagName('subFamily')
        for subfamily in subfamily_list:
            add_subfamily_to_database(subfamily, c, "DsubFamily", first_id)
    elif family.getElementsByTagName('device') != list():
        device_list = family.getElementsByTagName('device')
        for device in device_list:
            add_device_info_to_database(device, c, "Dname", first_id)
    else:
        raise "CMSIS-Pack is malformed. We have neither <subFamily> nor <device>."
    return first_id

"""
Adds the content of the file f to the database pointed by the cursor c.
"""
def add_pdsc_to_database(f, c, package_name, package_version):
    root = minidom.parse(f)
    devices = root.getElementsByTagName('devices')
    dev = devices[0]
    familylist = dev.getElementsByTagName('family')
    family = familylist[0]

    family_id = add_family_to_database(family, c)

    insert = '''INSERT INTO package (name, version, family_id) VALUES (?,?,?)'''
    c.execute(insert, [package_name, package_version, family_id])

    boards_root = root.getElementsByTagName('boards')
    for boards in boards_root:
        boardlist = boards.getElementsByTagName('board')
        for board in boardlist:
            name = board.getAttribute('name')
            try:
                dev_id = query_attribute_with_condition(c, "tree", "id", {"name" : name})
                request_insert_board = '''INSERT INTO board (name, device)
                                         VALUES (?,?)''';
                c.execute(request_insert_board, (name, dev_id))
            except Exception as e:
                pass

def _add_package(path, db):
    tmp_dir = ".tmp"
    package_file_name = path[:-5]
    unzip_dir = os.path.join(tmp_dir, package_file_name)
    print "Unzip dir", unzip_dir
    # we create the directory in which we will unzip the packs.
    if not (os.path.isdir(tmp_dir)):
        os.makedirs(tmp_dir)

    if (not os.path.isfile(path)) or (not os.access(path, os.R_OK)):
        print "is_file:", not os.path.isfile(path)
        print "r_ok:", not os.access(path, os.R_OK)
        print path, "is not a valid file."
        sys.exit(-1)
    try:
        if (os.path.exists(unzip_dir) or (not os.access(path, os.R_OK))):
            print "BADDD"
            raise IOError;
        # We unzip the pack in its own temporary directory.
        pdsc_pattern = os.path.join(unzip_dir, "*.pdsc")
        print "Unzip ", unzip_dir
        os.makedirs(unzip_dir)

        FNULL = open(os.devnull, 'w')
        subprocess.call(['unzip', '-d', unzip_dir, path],\
                        stdout=FNULL, close_fds=True)

        co = sqlite3.connect(db)
        c = co.cursor()


        package = ntpath.basename(path)
        package_name = '.'.join(package.split('.')[0:2])
        package_version = '.'.join(package.split('.')[2:-1])

        # We add the content of all the pdsc files in the database.
        # Usually there is only one file.
        file_list = glob.glob(pdsc_pattern)
        if not (len(file_list) >= 1):
            raise Exception('Pack does not contain a valid pdsc file.')
        f = file_list[0]

        add_pdsc_to_database(f, c, package_name, package_version)

        # We delete the temporary file
        # shutil.rmtree(unzip_dir)
        co.commit()
        co.close()
    except IOError:
        print "IOError"

