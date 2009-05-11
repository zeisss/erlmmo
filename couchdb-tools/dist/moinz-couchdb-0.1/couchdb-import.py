#!/usr/bin/env python2.6
import sys
#for x in sys.path: print x

import json
import os
import os.path
import io
import httplib
from StringIO import StringIO
from optparse import OptionParser

from moinz.couchdb import *
  
def import_file(filep, hostname, port, database, overwrite=False):
  data = filep.read()
  jsondata = json.loads(data)
  
  url = "/%s/" % database
  method="POST"
  if jsondata.has_key and jsondata.has_key('_id'):
    url = url + jsondata['_id']
    method="PUT"  
    
  try:
    url_read(hostname, port, url, data=jsondata, method=method)
  except ConflictException:
    if overwrite:
      doc = url_read(hostname, port, url) # fetch the current document
      jsondata['_rev'] = doc['_rev'] # set the revision in our doc to the current rev
    
      url_read(hostname, port, url, data=jsondata, method=method)
    else: 
      raise
  filep.close()

def import_folder(source, hostname, port, databases, overwrite=False):
  remote_databases = url_read(hostname, port, "/_all_dbs")
  if databases == []:
    databases = remote_databases
  if databases == []:
    print "Failed to find databases on the server. Please provide explicite db names."
    return 1

  print "Importing %s to http://%s:%d into" % (source, hostname, port), databases
  
  for dir in os.listdir(source):
    path = os.path.join(source, dir)
    if os.path.isdir(path) and dir in databases:
      print "Database: %s" % dir
      # Create the database if necessary
      if not dir in remote_databases:
        url_read(hostname, port, "/"+ dir, method="PUT")
      
      # Import the files
      for file in os.listdir(path):
        filepath = os.path.join(path, file)
        if os.path.isfile(filepath) and file[-5:] == ".json":
          print "  Importing %s/%s" % (dir,file)
          try:
            import_file(open(filepath,"r"), hostname, port, dir, overwrite)
          except ConflictException:
            print "  - Document with this id already exists. Skipping"

def main (args = None):
  if args == None:
    args = sys.argv[1:]
    
  usage = """usage: %prog [options] [databases]"""
  
  opt = OptionParser(usage=usage)
  opt.add_option('-s','--server','--host', action="store", dest="host", default="localhost", help="Which server to use")
  opt.add_option('-p', '--port', action="store", dest="port", default=5984, help="The port")
  opt.add_option('-d', '--dir', action="store", dest="dir", default=".", help="Where to read the files")
  opt.add_option('-f', '--force', action="store_true", dest="overwrite", default=False, help="Force the creation of documents, even if a given ID exists")
  (options, args) = opt.parse_args(args)
  
  import_folder(options.dir, options.host, int(options.port), databases=args, overwrite=options.overwrite)

if __name__ == "__main__":
    sys.exit(main())
