#!/usr/bin/python

import email.parser
import sys

parser = email.parser.Parser()
message = parser.parse(sys.stdin)

print message.get_payload(0).get_payload()
