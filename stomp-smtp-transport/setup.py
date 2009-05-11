from distutils.core import setup

setup (
  name = "moinz-stomp-smtp-transport",
  version = "0.1",
  description="A tool sending messages out via smtp that comes in via stomp",
  
  author = "Stephan Zeissler",
  author_email = "couchdb@moinz.de",
  url="http://www.moinz.de/files/",
  
  packages = ['moinz', 'moinz.sst'],
  
  scripts = [
    'stom-smtp-transport.py',
  ]
)
