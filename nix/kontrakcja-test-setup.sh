# Write a random port to the variable whose name is given as argument.
# FIXME: check it's actually available with netstat
get_available_port() {
  local name="$1"
  local port="$(shuf -n1 -i1024-65535)"
  eval "$name=$port"
}

# Ensure PostgreSQL & co are killed if something goes wrong.
trap "exit" INT TERM ERR
trap "kill 0" EXIT

# Initialise PostgreSQL database in directory.
rm -rf test_data
initdb --pgdata "$PWD/test_data" --locale=sv_SE.utf8
postgres -D "$PWD/test_data" -h "" -k "$PWD/test_data" &
sleep 2
psql -h "$PWD/test_data" postgres <<-EOS
  CREATE DATABASE kontrakcja_test;
EOS

# Start FakeS3
get_available_port fakes3_port
rm -rf test_s3files
fakes3 --root test_s3files --port "$fakes3_port" &

# Start scrivepdftools
get_available_port scrivepdftools_port
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
                    fakes3_port: $fakes3_port
            Events:
                Seal:
                    Type: Api
                    Properties:
                        Path: /seal
                        Method: post
EOS
sam local start-api --port "$scrivepdftools_port" &

# Generate the config file.
cat > kontrakcja_test.conf <<-EOS
  {
    "database":"host='$PWD/test_data' user='$USER' dbname='kontrakcja_test'",
    "pdftools_lambda" : {
      "gateway_url": "http://localhost:$scrivepdftools_port/seal",
      "api_key": "abc",
      "amazon_s3": {
          "host": "localhost"
        , "port": $fakes3_port
        , "bucket": "lambda"
        , "access_key": "k"
        , "secret_key": "s"
      }
    }
  }
EOS
