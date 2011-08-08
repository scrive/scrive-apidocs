#!/usr/bin/python

import pycurl
import urllib
import re
from StringIO import StringIO
import sys

if len(sys.argv) < 4:
    print "Usage: docinvariants.py <username> <password> <baseurl> [cafile]"
    print "Example: docinvariants.py ericwnormand@gmail.com admin http://localhost:8000"
    sys.exit(1)

username = sys.argv[1]
password = sys.argv[2]

baseurl = sys.argv[3]

#if len(sys.argv) > 4:
#    cafile = sys.argv[4]
#else:
#    cafile = Nothing

def null(s):
    None


# setup
cu = pycurl.Curl()
cu.setopt(pycurl.FOLLOWLOCATION, True)
cu.setopt(pycurl.WRITEFUNCTION, null)
cu.setopt(pycurl.VERBOSE, 0)
cu.setopt(pycurl.SSLCERT, "/etc/ssl/certs/ca-certificates.crt")
cu.setopt(pycurl.SSL_VERIFYPEER, 0)
def login(cf, uname, passwd):
    postparams = [("email", uname),
                  ("password", passwd)]
    cu.setopt(pycurl.COOKIEFILE, cf)
    cu.setopt(pycurl.COOKIEJAR , cf)

    cu.setopt(pycurl.URL, baseurl + "/login")
    cu.setopt(pycurl.POSTFIELDS, urllib.urlencode(postparams))
    cu.perform()


eric = "docinvariants.eric.cookies"
login(eric, username, password)

response = StringIO()

cu.setopt(cu.FOLLOWLOCATION, False)
cu.setopt(pycurl.WRITEFUNCTION, response.write)
cu.setopt(cu.HTTPGET, 1)
cu.setopt(cu.URL, baseurl + "/adminonly/docproblems")
cu.perform()
code = cu.getinfo(cu.HTTP_CODE)

if not code == 200:
    print "Oh, no! Code should be 200 but %s returned for %s" %(code, url)
    sys.exit(1)
else:
    ps = response.getvalue()
    if ps == "No problems!":
        sys.exit(0)
    else:
        print ps
        sys.exit(1)
