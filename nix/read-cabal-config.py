#!/usr/bin/env python

from yaml import load
import functools
import json
import re
import sys

f = open(sys.argv[1], "r")
config = load(f)
f.close()

constraints_string = config["constraints"]
constraints = {}

for str in constraints_string.split(","):
  name, *args = str.strip().split(" ")
  if constraints.get(name) is None:
    constraints[name] = {}
  pkg = constraints[name]

  for arg in args:
    vm = re.search("==(.+)", arg)
    fm = re.search("([+|-])(.+)", arg)
    if vm is not None:
      pkg["version"] = vm.group(1)
    elif fm is not None:
       if pkg.get("flags") is None:
         pkg["flags"] = []
       flags = pkg["flags"]
       flags.append(fm.group(0))
 
print(json.dumps(constraints))
