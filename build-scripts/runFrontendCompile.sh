#!/bin/bash

# This scripts build static files for frontend

cd frontend/
npm install
LC_ALL="pl_PL.UTF-8" grunt build
