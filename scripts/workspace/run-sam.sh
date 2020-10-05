#!/usr/bin/env bash

set -eux

trap 'kill 0' EXIT

echo "Running AWS SAM local instance"

docker_host=$(docker network inspect -f "{{ json . }}" bridge | jq -r '.IPAM.Config | .[0].Gateway')

echo "Docker Host IP: $docker_host"


if [ -z "${scrivepdftools:-}" ]
then
    pdftools_jar=${KONTRAKCJA_ROOT:-"$(pwd)"}/scrivepdftools/newscrivepdftools.jar
else
    pdftools_jar="$scrivepdftools/scrivepdftools.jar"
fi

echo "PDF Tools JAR path: $pdftools_jar"

template_path=$(mktemp /tmp/template-XXXXXX.yaml)

cat << EOF > $template_path
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
            CodeUri: $pdftools_jar
            Runtime: java8

            Environment:
                Variables:
                    bucket: lambda
                    access_key: k
                    secret_key: s
                    fakes3_host: $docker_host
                    fakes3_port: 4568
            Events:
                Seal:
                    Type: Api
                    Properties:
                        Path: /seal
                        Method: post
EOF

echo "Running SAM with template path at $template_path:"

cat "$template_path"

sam --version
sam local start-api --port 9876 --region eu-central-1 --template "$template_path"
