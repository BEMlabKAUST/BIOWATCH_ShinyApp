#!/usr/bin/env bash
set -e

APP_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo "Launching BIOWATCH from: $APP_DIR"
echo "Conda env: $CONDA_DEFAULT_ENV"

R -e "shiny::runApp('$APP_DIR/app.R', host='0.0.0.0', port=3838, launch.browser=TRUE)"