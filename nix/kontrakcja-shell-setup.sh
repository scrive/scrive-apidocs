# Ensure PostgreSQL and FakeS3 are killed when we quit.
trap "kill 0" EXIT

nix_local="$PWD/_nix_local"
mkdir -p "$nix_local"

data_created=0
if [[ ! -e "$nix_local/data" ]]; then
  initdb --pgdata "$nix_local/data" --locale sv_SE.utf8
  data_created=1
fi
if [[ ! -e "$nix_local/data/postmaster.pid" ]]; then
  sigshield `which postgres` -D "$nix_local/data" -h "" -k "$nix_local/data" &
fi

kill -s 0 `cat _s3_pid` 2> /dev/null \
  || (sigshield `which fakes3` --root "$nix_local/s3files" --port 4568 & \
      echo $! > _s3_pid)

if [[ ! -e template.yaml ]]; then
cat > template.yaml <<-EOS
AWSTemplateFormatVersion: '2010-09-09'
Transform: AWS::Serverless-2016-10-31
Description: >
    scrivepdftools

    SAM for scrivepdftools

Globals:
    Function:
         Timeout: 60
         MemorySize: 1024

Resources:
    SealingLambda:
        Type: AWS::Serverless::Function
        Properties:
            Handler: com.scrive.pdftools.LambdaInterface::handleRequest
            CodeUri: $scrivepdftools/scrivepdftools.jar
            Runtime: java8

            Environment:
                Variables:
                    bucket: lambda
                    access_key: k
                    secret_key: s
                    fakes3_host: 172.17.0.1
                    fakes3_port: 4568
            Events:
                Seal:
                    Type: Api
                    Properties:
                        Path: /seal
                        Method: post
EOS
fi

kill -s 0 `cat _sam_pid` 2> /dev/null \
  || (sigshield `which sam` local start-api --port 9876 & echo $! > _sam_pid)

# Wait for processes to start.
sleep 2

if [[ "x$data_created" == "x1" ]]; then
  echo "CREATE DATABASE kontrakcja; CREATE DATABASE kontrakcja_test;" \
    | psql -h "$nix_local/data" postgres
fi

read -r -d '' pdftools_lambda <<-EOF
{
  "gateway_url": "http://localhost:9876/seal",
  "api_key": "abc",
  "amazon_s3": {
    "host": "localhost",
    "port": 4568,
    "bucket": "lambda",
    "access_key": "k",
    "secret_key": "s"
  }
}
EOF
set_pdftools="setpath([\"pdftools_lambda\"]; $pdftools_lambda)"

read -r -d '' amazon <<-EOF
{
  "host": "localdomain",
  "port": 4568,
  "bucket": "localhost",
  "access_key": "key",
  "secret_key": "secret"
}
EOF
set_amazon="setpath([\"amazon\"]; $amazon)"

if [[ ! -e kontrakcja.conf ]]; then
  conn_string="host='$nix_local/data' user='$USER' dbname='kontrakcja'"
  jq "setpath([\"database\"]; \"$conn_string\") | $set_pdftools | $set_amazon" \
    < configuration-templates/kontrakcja.conf.template \
    > kontrakcja.conf
fi

if [[ ! -e kontrakcja_test.conf ]]; then
  conn_string="host='$nix_local/data' user='$USER' dbname='kontrakcja_test'"
  jq "setpath([\"database\"]; \"$conn_string\") | $set_pdftools | $set_amazon" \
    < configuration-templates/kontrakcja_test.conf.template \
    > kontrakcja_test.conf
fi
