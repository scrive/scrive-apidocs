import email.message
import sys
import base64

zipfile = open(sys.argv[1], "r").read()
zipmsg = email.message.Message()
zipmsg.set_type("application/x-compressed")
zipmsg.add_header("Content-Transfer-Encoding", "base64")
zipmsg.add_header("Content-Disposition", "attachment; filename=\"" + sys.argv[1] + "\"")
zipmsg.set_payload(base64.b64encode(zipfile))

outfile = open("/tmp/mime.txt", "w")
outfile.write(base64.b64encode(zipmsg.as_string()))
outfile.close()
