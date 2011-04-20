import email.message
import sys
import base64

zipfile = sys.stdin.read()
zipmsg = email.message.Message()
zipmsg.set_type("application/x-compressed")
zipmsg.add_header("Content-Transfer-Encoding", "base64")
zipmsg.set_payload(base64.b64encode(zipfile))

multi = email.message.Message()
multi.set_type("multipart/mixed")
multi.attach(zipmsg)

outfile = open("/tmp/mime.txt", "w")
outfile.write(base64.b64encode(multi.as_string()))
outfile.close()
