import wget
import os
import re
from subprocess import call
import multiprocessing

from xml.etree import cElementTree

import requests

def dl_pack(dic):
    try:
        name = dic["name"]
        version = dic["version"]
        url = dic["url"]
        if not url.endswith('/'):
            url = url + '/'
        outdir = dic["outdir"]
        name = name[:-5]
        pack_url = url + name + '.' + version + '.pack'
        outfile = os.path.join(outdir, name + '.' + version + '.pack')
        if not os.path.exists(outfile):
            print "Downloading : %s" % pack_url
            wget.download(pack_url, out=outfile, bar=None)
            #r = requests.get(pack_url, stream=True)
            print "Outfile : %s" % outfile
    except Exception as e:
        print e
    return

def create_subdir(dir):
    if not os.path.exists(dir):
        os.makedirs(dir)

# Get pack list from the website by parsing the HTML page
def etree_get_nodes(handle, name):
    events = cElementTree.iterparse(handle, events=("start", "end",))
    _, root = next(events)  # Grab the root element.
    print "TAG:",elem.tag
    for event, elem in events:
        if event == "end" and elem.tag == name:
            print "TAG:",elem.tag
            yield elem
            root.clear()  # Free up memory by clearing the root element.

def main():
    outdir='cmsis_packs'
    
    create_subdir(outdir)
    
    index=os.path.join(outdir, 'index.html')
    
    # Index page is generated when going to `https://www.keil.com/pack/index.idx`
    # Contains all the software packs made by Keil.
    index=wget.download('http://sadevicepacksprodus.blob.core.windows.net/idxfile/index.idx',
                        out=index, bar=None)
    
    regex = r'.*pdsc url="(.*)" name"(.*) version="([0-9.]*)/*'
    
    files_to_download = list()
    
    # We populate the list of files to download.
    with open(index, 'r') as file:
        for line in file:
            if line.startswith('<pdsc'):
                xmldoc  = cElementTree.fromstring('<blah>' + line + '</blah>')
                for child in xmldoc.findall("pdsc"):
                    for sub in child.iter():
                        if "DFP" in sub.attrib["name"]:
                            files_to_download.append({"name"     : sub.attrib["name"],
                                                      "url" : sub.attrib["url"],
                                                      "version"  : sub.attrib["version"],
                                                      "outdir"   : outdir
                                                      })
    
    
    cpu_count = int(multiprocessing.cpu_count() - 1)
    print "Setting up pool with %s workers" % cpu_count
    pool = multiprocessing.Pool(processes = cpu_count)
    pool.map(dl_pack, files_to_download)
    pool.close()
    pool.join()

if __name__ == '__main__':
    main()
