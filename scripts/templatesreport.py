#!/usr/bin/python

import csv
import os
import glob
import fnmatch
import codecs
import re
import sys

# the final exit code
ec = 0

# read in all texts
textsbyname = {}

duplicatetextnames = []

for f in glob.glob("texts/*.csv"):
    rows = csv.reader(open(f, 'rb'))
    header = rows.next()
    for r in rows:
        if len(r) > 0:
            name = r[0].strip()
            if len(name) > 0:
                text = {}
                text['name'] = name
                text['file'] = f
                text['definition'] = r[1:]
            
                if name in textsbyname:
                    duplicatetextnames.append(name)
                    textsbyname[name].append(text)
                else:
                    textsbyname[name] = [text]



alltemplates = {}
duplicatetemplatenames = []

# recursively get all template files

matches = []
for root, dirnames, filenames in os.walk('templates'):
  for filename in fnmatch.filter(filenames, '*.st'):
      matches.append(os.path.join(root, filename))

for temp in matches:
    contents = codecs.open(temp, 'r', 'utf-8').read()
    # split on lines that start with #
    templates = re.split("(^|\n)#.*?(?=(\n|$))", contents)
    for t in templates:
        t = t.strip()
        if t == "":
            continue
        parts = t.split("=", 1)
        if len(parts) == 2:
            [name, defn] = parts
            name = name.strip()
            if len(name) < 1:
                continue
            defn = defn.strip()
            t = {'name': name, 'definition':defn, 'file': temp}
            if name in alltemplates:
                duplicatetemplatenames.append(name)
                alltemplates[name].append(t)
            else:
                alltemplates[name] = [t]
        else:
            print "failing to parse this template: "
            print t

if len(duplicatetemplatenames) > 0:
    print "The following template names are duplicates:"
    for d in duplicatetemplatenames:
        print "    %s" % d
    ec = 1

# conservative garbage collection

blacklist = {}
graylist  = {}
whitelist = {}

for t in alltemplates:
    blacklist[t] = 1
for t in textsbyname:
    blacklist[t] = 1

# mark everything that looks like it could be a template in .hs files
matches = []
for root, dirnames, filenames in os.walk('src'):
  for filename in fnmatch.filter(filenames, '*.hs'):
      matches.append(os.path.join(root, filename))

for hs in matches:
    contents = codecs.open(hs, 'r', 'utf-8').read()
    # find literal strings
    strings = re.findall('"(?:[^"\\\\]|\\\\.)*"', contents)
    for s in strings:
        s = s[1:-1]
        if s in blacklist:
            graylist[s] = 1

for s in graylist:
    if s in blacklist:
        del blacklist[s]

def getcalls(s):
    templatepatterns = re.findall('(?<=\\$).*?(?=\\$)', s, re.MULTILINE | re.DOTALL)
    subtemplates = []
    for tp in templatepatterns:
        names = re.findall('\\w*?(?=(?::noescape)?\\(.*?\\))', tp, re.MULTILINE | re.DOTALL)
        for name in names:
            subtemplates.append(name)
    return subtemplates

while len(graylist.keys()) > 0:
    newgraylist = {}
    for ts in graylist:
        whitelist[ts] = 1
        if ts in alltemplates:
            for t in alltemplates[ts]:
                calls = getcalls(t['definition'])
        elif ts in textsbyname:
            for t in textsbyname[ts]:
                calls = []
                for s in t['definition']:
                    calls = calls + getcalls(s)
        for c in calls:
            newgraylist[c] = 1
    for ts in newgraylist:
        graylist[ts] = 1
        if ts in blacklist:
            del blacklist[ts]
    for ts in whitelist:
        if ts in graylist:
            del graylist[ts]

if len(blacklist) > 0:
    print "The following templates are not used:"
    totallines = 0
    totaltemplates = 0
    for t in sorted(blacklist.keys()):
        if t in alltemplates:
            for x in alltemplates[t]:
                lines = len(x['definition'].split("\n"))
                totallines = totallines + lines
                totaltemplates = totaltemplates +1
                print "  %s in %s with %s lines" % (x['name'], x['file'], lines)
    print " %s templates and %s lines" % (totaltemplates, totallines)
    print ""
    print "The following texts are not used:"
    totallines = 0
    totaltexts = 0
    for t in sorted(blacklist.keys()):
        if t in textsbyname:
            for x in textsbyname[t]:
                lines = len("|".join(x['definition']).split("\n"))
                totallines = totallines + lines
                totaltexts = totaltexts +1
                print "  %s in %s with %s lines" % (x['name'], x['file'], lines)
    print " %s texts and %s lines" % (totaltexts, totallines)
    ec = 1

if len(duplicatetextnames) > 0:
    print ""
    print "The following text names are duplicates:"
    for d in duplicatetextnames:
        print " %s" % d
        for text in textsbyname[d]:
            print "   %s in file %s" % (d, text['file'])
    ec = 1

# figure out texts with same text
bytext = {}

for tn in textsbyname:
    for text in textsbyname[tn]:
        c = "|".join(text['definition'])
        if c in bytext:
            bytext[c].append(text)
        else:
            bytext[c] = [text]
print ""
for t in bytext:
    texts = bytext[t]
    if len(texts) > 1:
        print 'Same texts "%s":' % t
        for text in texts:
            print "  %s in %s" % (text['name'], text['file'])

sys.exit(ec)
