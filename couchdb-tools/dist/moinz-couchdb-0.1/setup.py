from distutils.core import setup

setup (
  name = "moinz-couchdb",
  version = "0.1",
  description="Commandline tools for importing and exporting data from/to a CouchDB",
  
  author = "Stephan Zeissler",
  author_email = "couchdb@moinz.de",
  url="http://www.moinz.de/files/",
  
  packages = ['moinz', 'moinz.couchdb'],
  
  scripts = [
    'couchdb-import.py',
    'couchdb-export.py',
  ]
)
