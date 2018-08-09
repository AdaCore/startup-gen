import json
import sys
import argparse

from string import Template


wrapper = Template("${Type} ${name} is\n%s\nend ${name};")

attribute = Template("for ${var} use ${content};")

"""
Returns the package declaration containing the infos about the different
memory regions.
"""
def get_mem_output(json):
    package = wrapper.substitute(Type="package", name="Memory_Map")
    content = list()
    memory_names = ["\"" + mem["name"] + "\"" for mem in json]

    memories = attribute.substitute(var="Memories",
                                    content="(" + ','.join(memory_names) + ")")

    content.append(memories)

    boot_mem_name = next((mem["name"] for mem in json if mem["startup"]), None)
    boot_mem = attribute.substitute(var="Boot_Memory",
                                    content="\"%s\"" % boot_mem_name)

    content.append(boot_mem)

    memory_names = ["ROM" if "RAM" not in mem["name"] else "RAM" for mem in json]

    for mem in json:
        # We get the attribute common to all memory regions.
        addr_decl = attribute.substitute(var="Address(\"%s\")" % mem["name"],
                                    content="\"%s\"" % mem["address"])

        size_decl = attribute.substitute(var="Size(\"%s\")" % mem["name"],
                                    content="\"%s\"" % mem["size"])

        mem_kind = "ROM" if "RAM" not in mem["name"].upper() else "RAM"
        kind_decl = attribute.substitute(var="Mem_Kind(\"%s\")" % mem["name"],
                                    content="\"%s\"" % mem_kind)
        content.append(addr_decl)
        content.append(size_decl)
        content.append(kind_decl)

    return package % ('\n'.join("    " + element for element in content))

"""
Returns the package declaration containing the infos about the CPU used.
"""
def get_cpu_output(json, int_nb):
    # Spaces are here for coding style purposes.
    package = wrapper.substitute(Type="    package", name="CPU")
    cpu_name = json["name"]
    fpu = json["fpu"]
    content = list()

    name_decl = attribute.substitute(var=("Name"),
                                     content=("\"%s\"" % cpu_name))

    float_handling = "\"%s\"" % ("hard" if fpu else "soft")

    float_decl = attribute.substitute(var="Float_Handling",
                                     content=float_handling)

    nb_int_decl = attribute.substitute(var="Number_Of_Interrupts",
                                     content=("\"%s\"" % int_nb))
    content.append(name_decl)
    content.append(float_decl)
    content.append(nb_int_decl)
    return package % ('\n'.join("    " + element for element in content))


"""
Returns the project declaration for the device characteristics.
"""
def get_device_output(json, int_nb):
    cpu = get_cpu_output(json["device"]["cpu"], int_nb)
    mem = get_mem_output(json["device"]["memory"])

    project = wrapper.substitute(Type="project", name="Spec")

    withed_interrupt_project = "with \"interruptions\";\n" if ("interrupts" in json.keys()) else ""

    return withed_interrupt_project\
        + project % (cpu + "\n" + mem).replace('\n', '\n    ')

"""
Returns the project declaration describing the interrupt vector.
"""
def get_interrupt_output(json):
    project = wrapper.substitute(Type="project",
                                 name="Interruptions")
    package = wrapper.substitute(Type="package",
                                 name="Interrupt_Vector")
    content = list()
    int_nb = 0
    for key, name in sorted(json["interrupts"].items(), key=lambda k:int(k[0])):
        interrupt_decl = attribute.substitute(var=("Interrupt(\"%s\")" % key),
                                              content=("\"%s\"" % name))
        content.append(interrupt_decl)
        ++int_nb
    package = package % ('\n'.join("    " + interrupt for interrupt in content))
    return project %\
        ('\n'.join("    " + line for line in package.split('\n'))), int_nb


"""
Function called from the python world.
It will dump the translated json to the 2 files passed as parameters.
Sometimes we have no interrupts for the given device, in that case
we dont generate the `interruptions.gpr` file.
"""
def dump_gpr_files(parsed_json, dev_file, int_file=""):
    int_nb = 0
    if int_file != "" :
        interrupts, int_nb = get_interrupt_output(parsed_json)
        with open(int_file, 'w+') as f:
            f.write(interrupts)

    device = get_device_output(parsed_json, int_nb)
    with open(dev_file, 'w+') as f:
        f.write(device)

def entry_from_cmdline():
    parser = argparse.ArgumentParser()

    parser.add_argument('device_file',
                        help='Output file for the device description')

    parser.add_argument('interrupt_file',
                        action="store",
                        help='Output file for the interrupt vector')

    args = parser.parse_args()

    json_input = sys.stdin.read().strip()

    dump_gpr_files(json.loads(json_input),\
                   args.device_file,\
                   args.interrupt_file)
