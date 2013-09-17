#!/bin/sh -e

if [ -z $1 -o -z $2 ];
then
    echo "Usage:"
    echo "    $1 dbconnstring documentid"
    echo
    echo "Where:"
    echo "   dbconnstring - database name or connection string"
    echo "   documentid  - document id that was signed and should serve as reference screenshot"
    exit 55
fi

SQL_BASE=$(cat <<EOF
  FROM documents
  JOIN signatory_links ON documents.id = signatory_links.document_id
  JOIN signatory_screenshots ON signatory_screenshots.signatory_link_id = signatory_links.id
  JOIN files ON files.id  = signatory_screenshots.file_id
 WHERE documents.id = $2
   AND NOT signatory_links.is_author
   AND signatory_links.is_partner
   AND signatory_links.sign_time IS NOT NULL
   AND signatory_screenshots.type = 'signing'
EOF
)

SQL_SELECT_FILE="SELECT encode(files.content,'base64') $SQL_BASE"
SQL_SELECT_TIME="SELECT EXTRACT(EPOCH FROM signatory_screenshots.time) $SQL_BASE"

psql $1 -q -t -A -c "$SQL_SELECT_FILE" | base64 -D > ../public/reference_screenshot.jpg.tmp

psql $1 -q -t -A -c "$SQL_SELECT_TIME"  > ../public/reference_screenshot_seconds.txt.tmp

if [ -s ../public/reference_screenshot.jpg.tmp -a -s ../public/reference_screenshot_seconds.txt.tmp ];
then
    echo "Seems like it has worked"
    echo "Epoch seconds is:"
    cat ../public/reference_screenshot_seconds.txt.tmp
    mv -f ../public/reference_screenshot_seconds.txt.tmp ../public/reference_screenshot_seconds.txt
    mv -f ../public/reference_screenshot.jpg.tmp ../public/reference_screenshot.jpg
else
    echo "Our magic select statements returned no results"
    echo "Reference screenshot left intact"
    echo "Document id might be wrong, or documents is not signed by a single non-author partner"
    exit 55
fi
