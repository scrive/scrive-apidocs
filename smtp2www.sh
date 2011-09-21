#!/bin/bash

#URI="http://preprod.skrivapa.se/mailapi/"
URI="$1"
MAGIC="$2"

LOG="/tmp/mailapi-$USER.log"
REQUEST="/tmp/mailapi-${USER}-request.eml"
RESPONSE="/tmp/mailapi-${USER}-response.log"

echo ${URI} >> ${LOG}
chmod 600 ${LOG}

#Step one, send the request
tee ${REQUEST} | curl -v -k --fail -X POST -F "magic=${MAGIC-}" -F "extension=${EXTENSION-}" -F "mail=@-" "${URI}" 2>> ${LOG} | (echo "Subject: SkrivaPa API" && echo "From: api@api.skrivapa.se" && echo "To: ${SENDER}" && echo "" && cat - && echo "" && echo "") | tee -a ${RESPONSE} 

rv="$?"

if [ "$rv" -eq 22 ]; then
exit 69
elif [ "$rv" -eq 6 ]; then
exit 69
elif [ "$rv" -eq 7 ]; then
exit 69
elif [ "$rv" -eq 18 ]; then
exit 69
elif [ "$rv" -eq 52 ]; then
exit 69
elif [ "$rv" -eq 51 ]; then
exit 69
elif [ "$rv" -eq 0 ]; then
exit 0
else
echo "Mail API error $rv" >> ${LOG}
exit 70
fi
