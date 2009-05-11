#!/usr/bin/env python2.6
import sys
from optparse import OptionParser
import stomp
import smtplib

options = None
smtp = None

def sendmail(headers, body):
  global smtp
  
  if smtp == None:
    smtp = smtplib.SMTP()
  
  print
  print "-------------- Message received -------------------"
  print headers
  print body
  
  
def run(hostname, port, channel ):
  con = stomp.stomp.Stomp(hostname, port)
  con.connect()
  
  try:
    con.subscribe({
        'destination' : channel,
        'ack' : 'client'  
    })
    
    while True:
      frame = con.receive_frame()      
      try:
        sendmail(frame.headers, frame.body)
        if frame.headers.has_key('message-id'):
          con.ack(frame)
      except e:
        print "Error: %s" % e
  finally:
    con.disconnect()

def main (args=None):
  if args == None:
    args = sys.argv[1:]
    
  print "Stomp-Smtp-Transport starting ..."
  
  global options
  usage = "usage: %prog [options] stomp-channel"
  
  opt = OptionParser(usage=usage)
  opt.add_option('-s','--server','--host', action="store", dest="host", 
                 default="localhost", help="Which server to use")
  opt.add_option('-p', '--port', action="store", dest="port", default=61613, help="The port")
  opt.add_option('-v', '--verbose', action="store_true", dest="verbose", default=".")

  (options, args) = opt.parse_args(args)
  
  if len(args) != 1:
    opt.error("Please provide exactly one option.")
  
  run(options.host, options.port, args[0])
  
  
  
if __name__ == "__main__":
  sys.exit(main())