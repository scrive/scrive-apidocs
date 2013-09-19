#!/bin/sh

#
# This script creates another script. As single input it should get
# output from purge_files.sql.
#
if [ -z $1 ];
then
    echo "Usage:"
    echo "   $0 <outout from purge_files.sql>"
    exit 56
fi

echo "#!/bin/sh" > purge_files_from_remote_amazon_account.sh
sed -E -e '1,/START/d' -e '/^UPDATE/d' -e '/FINISH/,$d' -e 's@^[0-9]+ @s3cmd del s3://@' $1 >> purge_files_from_remote_amazon_account.sh

echo "Created purge_files_from_remote_amazon_account.sh"
echo "Run it using:"
echo "    bash purge_files_from_remote_amazon_account.sh"
