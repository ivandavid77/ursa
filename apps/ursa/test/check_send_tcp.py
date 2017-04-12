#!/usr/bin/env python
import socket
import sys
import time

if len(sys.argv) == 2:
  ok = int(sys.argv[1])
else:
  ok = 1
dt = time.strftime('%Y-%m-%dT%H:%M:%SZ')
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(('localhost', 55056))
print "sending data..."
s.send('{"ip":"10.0.1.30","gps":[19.436726,-99.184367],"ok":%i,"dt":"%s"}'%(ok,dt))
s.close()
