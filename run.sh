#!/bin/bash

# This script runs an Advent of Code OCaml solution for a given day.
# It expects the day number as an argument (e.g., 01, 02).
# It will load AOC_COOKIE from a .env file in the project root if it exists.

DAY=$1

if [ -z "$DAY" ]; then
  echo "Usage: $0 <day_number>"
  echo "Example: $0 01"
  exit 1
fi

# Load environment variables from .env if the file exists
if [ -f ".env" ]; then
  # echo "Loading environment variables from .env"
  set -a # Automatically export all variables
  source .env
  set +a # Stop automatically exporting
fi

# Check if AOC_COOKIE is set
if [ -z "$AOC_COOKIE" ]; then
  echo "Warning: AOC_COOKIE is not set. It might be needed for input fetching."
  echo "Please set AOC_COOKIE in your .env file or environment."
fi

AOC_COOKIE=$AOC_COOKIE dune exec ./bin/day"${DAY}".exe
