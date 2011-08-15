#!/usr/bin/python

import pycurl
import urllib
import re

username = "ericwnormand@gmail.com"
password = "admin"

secondary = "lukas@skrivapa.se"
spassword = "admin"

baseurl = "http://localhost:8000"

public_urls = ["/"]

should_need_login = ["/d", "/o"]

should_need_primary_author = []
should_need_secondary_author = []

should_need_eric_signatory = []
should_need_lukas_signatory = []


def null(s):
    None


# setup
cu = pycurl.Curl()
cu.setopt(pycurl.FOLLOWLOCATION, True)
cu.setopt(pycurl.WRITEFUNCTION, null)
cu.setopt(pycurl.VERBOSE, 1)
def login(cf, uname, passwd):
    postparams = [("email", uname),
                  ("password", passwd)]
    cu.setopt(pycurl.COOKIEFILE, cf)
    cu.setopt(pycurl.COOKIEJAR , cf)

    cu.setopt(pycurl.URL, baseurl + "/login")
    cu.setopt(pycurl.POSTFIELDS, urllib.urlencode(postparams))
    cu.perform()


eric = "eric.cookies"
login(eric, username, password)
lukas = "lukas.cookies"
login(lukas, secondary, spassword)
nonlogged = "nonlogged.cookies"

# check basic urls
for cj in [eric, lukas, nonlogged]:
    for url in public_urls:
        cu.setopt(pycurl.COOKIEFILE, cj)
        cu.setopt(pycurl.COOKIEJAR, cj)
        cu.setopt(pycurl.HTTPGET, 1)
        cu.setopt(pycurl.URL, baseurl + url)
        cu.perform()
        code = cu.getinfo(pycurl.HTTP_CODE)

        if not code == 200:
            print "Oh, no! Code should be 200 but %s returned for %s" %(code, url)


# create a document per user
def uploaddoc(cj):
    cu.setopt(pycurl.COOKIEFILE, cj)
    cu.setopt(pycurl.COOKIEJAR, cj)

    cu.setopt(pycurl.POST, 1)
    cu.setopt(pycurl.URL, baseurl + "/d")
    l = cu.getinfo(pycurl.INFO_COOKIELIST)
    xtoken = ""
    for s in l:
        m = re.search("xtoken\t(\".*\")", s)
        if not m == None:
            xtoken = m.group(1)
    cu.setopt(pycurl.HTTPPOST, [("doctype", "Contract"), ("doc", (pycurl.FORM_FILE, "/home/eric/test.pdf")),( "xtoken", xtoken)])
    cu.perform()
    return cu.getinfo(pycurl.EFFECTIVE_URL)

def getdocid(url):
    m = re.search("/d/(.*)", url)
    if not m == None:
        return m.group(1)
    return None

ericdocurl = uploaddoc(eric)
lukasdocurl = uploaddoc(lukas)
print ericdocurl
print lukasdocurl
should_need_primary_author.append(ericdocurl)
should_need_secondary_author.append(lukasdocurl)




for cj in [nonlogged, lukas]:
    for url in should_need_primary_author:
        cu.setopt(pycurl.COOKIEFILE, cj)
        cu.setopt(pycurl.COOKIEJAR, cj)


        cu.setopt(cu.FOLLOWLOCATION, False)
        cu.setopt(cu.HTTPGET, 1)
        cu.setopt(cu.URL, url)
        cu.perform()
        code = cu.getinfo(cu.HTTP_CODE)
        if not code == 404:
            print "Oh, no! Code should be 404 but %s returned for %s" %(code, url)
