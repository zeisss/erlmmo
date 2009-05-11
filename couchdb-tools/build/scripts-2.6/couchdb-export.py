#!/Library/Frameworks/Python.framework/Versions/2.6/Resources/Python.app/Contents/MacOS/Python
import sys

import json
import os
import os.path
import io
from StringIO import StringIO
from optparse import OptionParser

from moinz.couchdb import *

def export_database (hostname, port, database, path):
  ids = url_read(hostname, port, "/%s/_all_docs" % database)
  
  for row in ids['rows']:
    id = row['id']

    data = url_read(hostname, port, "/%s/%s" % (database, id))
    
    # we drop the revision, so it doesn't conflict later when reimporting
    del data['_rev']
    
    filepath = os.path.join(path, id + ".json")
    fp = io.open(filepath, "wb")
    json.dump(data, fp, sort_keys=True, indent=4)
    fp.write(chr(10))
    fp.close()
    
  
def export (destination, hostname, port, databases = []):
  print "Backing up %s:%d to %s" % (hostname, port, destination)
  
  if databases == []:
    databases = url_read(hostname, port, "/_all_dbs")
    
  if not os.path.exists(destination):
    os.mkdir(destination)
    
  for db in databases:
    path = os.path.join(destination, db)
    print " %s => %s/" % (db, path)

    if not os.path.exists(path):
      os.mkdir(path)
      
    export_database(hostname, port, db, path)
   
def main (args = None):
  if args == None:
    args = sys.argv[1:]
    
  usage = """usage: %prog [options] [databases]"""
  
  opt = OptionParser(usage=usage)
  opt.add_option('-s','--server','--host', action="store", dest="host", default="localhost", help="Which server to use")
  opt.add_option('-p', '--port', action="store", dest="port", default=5984, help="The port")
  opt.add_option('-d', '--destination', action="store", dest="destination", default=".", help="Where to write the files")
    
  (options, args) = opt.parse_args(args)
  
  export(options.destination, options.host, int(options.port), args)

if __name__ == "__main__":
  sys.exit(main())