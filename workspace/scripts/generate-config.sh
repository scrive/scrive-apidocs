#!/usr/bin/env bash

script_dir="$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )"
KONTRAKCJA_ROOT=${KONTRAKCJA_ROOT:-"."}
KONTRAKCJA_WORKSPACE=${KONTRAKCJA_WORKSPACE:-"$KONTRAKCJA_ROOT"}

echo "Generating config files at $KONTRAKCJA_WORKSPACE"

set_amazon="setpath([\"amazon\"]; $( cat $script_dir/fixture/amazon.json ))"

if [[ -e "$KONTRAKCJA_WORKSPACE/pdftools-lambda.local.json" ]]; then
  pdftools_conf=$( cat "$KONTRAKCJA_WORKSPACE/pdftools-lambda.local.json" )
else
  pdftools_conf=$( cat "$script_dir/fixture/pdftools_lambda.json" )
fi

set_pdftools="setpath([\"pdftools_lambda\"]; $pdftools_conf)"
set_users="setpath([\"initial_users\"]; $( cat $script_dir/fixture/users.json ))"

db_path="$KONTRAKCJA_WORKSPACE/_local/data"
main_conn_string="host='$db_path' user='$USER' dbname='kontrakcja'"
test_conn_string="host='$db_path' user='$USER' dbname='kontrakcja_test'"

set_main_database="setpath([\"database\"]; \"$main_conn_string\")"
set_test_database="setpath([\"database\"]; \"$test_conn_string\")"

sed -e "s|\$scrivepdftools|$scrivepdftools|g" "$script_dir/fixture/template.yaml" > "$KONTRAKCJA_WORKSPACE/template.yaml"

jq \
  "$set_main_database | $set_amazon | $set_pdftools | $set_users" \
  < "$KONTRAKCJA_ROOT/configuration-templates/kontrakcja.conf.template" \
  > kontrakcja.conf

jq \
  "$set_main_database | $set_amazon | $set_pdftools" \
  < "$KONTRAKCJA_ROOT/configuration-templates/cron.conf.template" \
  > cron.conf

jq \
  "$set_main_database | $set_amazon" \
  < "$KONTRAKCJA_ROOT/configuration-templates/mailing_server.conf.template" \
  > mailing_server.conf

jq \
  "$set_main_database | $set_amazon" \
  < "$KONTRAKCJA_ROOT/configuration-templates/messenger_server.conf.template" \
  > messenger_server.conf

jq \
  "$set_test_database | $set_pdftools | $set_amazon" \
  < "$KONTRAKCJA_ROOT/configuration-templates/kontrakcja_test.conf.template" \
  > kontrakcja_test.conf
