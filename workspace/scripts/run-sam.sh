#!/usr/bin/env bash

echo "Running AWS SAM local instance"

sam local start-api --port 9876 --region eu-central-1
