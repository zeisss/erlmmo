#!/usr/bin/env python2.6
import sys
from optparse import OptionParser
import stomp
import smtplib
import email

class MessageReceiver(object):
  def __init__(self, options):
    self.options = options
    self.smtp = smtplib.SMTP(options.smtp_host, options.smtp_port)
    
    self.con = stomp.stomp.Stomp(options.hostname, options.port)
    
  def sendmail(self, email_msg):
    """
    Sends the mail using the smtplib module.
    """
    
    to = email_msg['To'].split(",") or ['root@localhost']
    
    self.log( "Sending mail to %s" % to )
    self.smtp.sendmail(
        email_msg['From'] or 'root@localhost',
        to,
        email_msg.as_string()
    )
    self.log (" Succeded")
    
  def handle_frame(self, frame):
    """
    Parses the body of the frame using the email module and sends it out via self.sendmail().
    """
    eml = frame.body
    
    mailmsg = email.message_from_string(eml)
    self.sendmail(mailmsg)
    
    
  def handle_frames(self):
    try:
      self.con.connect()
      
      self.log("Subscribing to channel %s" % self.options.channel)
      self.con.subscribe({
        'destination' : self.options.channel,
        'ack' : 'client'  
      })
      
      while True:
        frame = self.con.receive_frame()      
        try:
          self.handle_frame(frame)
          
          # Error messages have no message-id
          if frame.headers.has_key('message-id'):
            self.con.ack(frame)
        except Exception as e:
          print "Error: %s" % e
    finally:
      self.smtp.quit()
      self.con.disconnect()
      
   
    return 0
  
  def log(self, message):
    if self.options.verbose:
      print message
     
def main (args=None):
  if args == None:
    args = sys.argv[1:]
    
  print "Stomp-Smtp-Transport starting ..."
  
  global options
  usage = "usage: %prog [options]"
  
  opt = OptionParser(usage=usage)
  opt.add_option('-s','--server','--host', action="store", dest="hostname", 
                 default="localhost", help="Which server to use")
  opt.add_option('-p', '--port', action="store", dest="port", default=61613, help="The port")
  opt.add_option('-v', '--verbose', action="store_true", dest="verbose", default=".")
  opt.add_option('-c', '--channel', action="store", dest="channel", default="/queue/smtp.out")  
  
  opt.add_option('--smtp-host', action='store', dest='smtp_host', default='localhost')
  opt.add_option('--smtp-port', action='store', dest='smtp_port', default=25)

  (options, args) = opt.parse_args(args)

  mr = MessageReceiver(options)
  return mr.handle_frames()
  
if __name__ == "__main__":
  sys.exit(main())