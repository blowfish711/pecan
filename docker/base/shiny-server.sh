#!/bin/bash

# This script is based on the rocker-org/shiny script
# Source: https://github.com/rocker-org/shiny/blob/master/shiny-server.sh 

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server

Rscript -e "sessionInfo(); .libPaths(); require('shiny')"

if [ "$APPLICATION_LOGS_TO_STDOUT" = "false" ];
then
    shiny-server 2>&1
else
    # start shiny server in detached mode
    shiny-server 2>&1 &

    # push the "real" application logs to stdout with xtail
    xtail /var/log/shiny-server/
fi
