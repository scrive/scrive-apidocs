import email.message
import sys
import base64

zipfile = open(sys.argv[1], "r").read()
zipmsg = email.message.Message()
zipmsg.set_type("text/plain")
zipmsg.add_header("Content-Disposition", "inline; filename=\"" + sys.argv[1] + "\"")
zipmsg.set_payload(zipfile)


outfile = open(sys.argv[2], "w")
outfile.write(zipmsg.as_string())
outfile.close()
