#!/usr/bin/python

import xml.dom.minidom
import sys

def getText(nodelist):
    rc = []
    for node in nodelist:
        if node.nodeType == node.TEXT_NODE:
            rc.append(node.data)
    return ''.join(rc)

resp = xml.dom.minidom.parse(open(sys.argv[1], "r"))
out = getText(resp.getElementsByTagName("SignedDocument")[0].childNodes)

outfile = open(sys.argv[2], "w")
outfile.write(out)
outfile.close()
