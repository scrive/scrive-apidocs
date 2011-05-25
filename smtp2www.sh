#!/bin/bash

URI="$1"
LONGRANDOMID="$2"

#Step one, send the request
curl -v --fail -X POST -F "action=SENDEMAIL" -F "ID=${LONGRANDOMID-_}" -F "magic=${EXTENSION-}" -F "mail=@-" "${URI}"

#  
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
touch "/tmp/smtperrors$USER" && chmod 600 "/tmp/smtperrors$USER" && echo "ODD I got $rv" >> "/tmp/smtperrors$USER"
exit 70
fi
