#!/bin/bash

# Default values
APP_DIR="./inst/app"
APP_FILE="app.R"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --example)
      APP_DIR="./inst/app/examples"
      shift
      ;;
    --file)
      APP_FILE="$2"
      shift 2
      ;;
    -h|--help)
      echo "Usage: $0 [--example] [--file <filename>]"
      echo "  --example    Change directory to ./inst/app/examples/ (default: ./inst/app/)"
      echo "  --file       Specify R script filename (default: app.R)"
      echo "  -h, --help   Show this help message"
      echo ""
      echo "Examples:"
      echo "  $0                           # Runs ./inst/app/app.R"
      echo "  $0 --example                 # Runs ./inst/app/examples/app.R"
      echo "  $0 --file my_app.R           # Runs ./inst/app/my_app.R"
      echo "  $0 --example --file demo.R   # Runs ./inst/app/examples/demo.R"
      exit 0
      ;;
    *)
      echo "Unknown option: $1"
      echo "Use --help for usage information"
      exit 1
      ;;
  esac
done

# Construct full path
FULL_PATH="$APP_DIR/$APP_FILE"

# Check if the app directory exists
if [ ! -d "$APP_DIR" ]; then
  echo "Error: Directory '$APP_DIR' not found!"
  echo "Current directory: $(pwd)"
  echo "Use --help for usage information"
  exit 1
fi

# Check if the app file exists
if [ ! -f "$FULL_PATH" ]; then
  echo "Error: App file '$FULL_PATH' not found!"
  echo "Current directory: $(pwd)"
  echo "Available files in $APP_DIR:"
  ls -la "$APP_DIR" 2>/dev/null || echo "  (unable to list files)"
  echo "Use --help for usage information"
  exit 1
fi

echo "Starting Shiny app..."
echo "Directory: $APP_DIR"
echo "File: $APP_FILE"
echo "Full path: $FULL_PATH"
echo "App will be available at http://localhost:3838"
echo "Press Ctrl+C to stop"

# Set R options and run the app
Rscript -e "
options(shiny.host = '0.0.0.0', shiny.port = 3838);
shiny::runApp('$FULL_PATH');
"