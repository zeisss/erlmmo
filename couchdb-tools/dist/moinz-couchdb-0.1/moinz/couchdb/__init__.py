import httplib
import json

class ConflictException(Exception): pass

class NotFoundException(Exception): pass

def url_read(hostname, port, url, data=None, method="GET"):
  req = httplib.HTTPConnection(hostname, port)
  req.request(method, url, body=json.dumps(data))
  res = req.getresponse()
  try:
    data = res.read()
    jsondata = json.loads(data)
    if type(jsondata) == dict and jsondata.has_key and jsondata.has_key('error'):

      text = "%s (for url %s)" % (jsondata['reason'], url)
      if jsondata['error'] == "conflict":
        raise ConflictException(text)
      elif jsondata['error'] == "not_found":
        raise NotFoundException(text)
      else:
        raise Exception("%s: %s" % (jsondata['error'],text))
    else:
      return jsondata
  finally:
    req.close()
  
