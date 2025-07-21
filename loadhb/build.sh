#!/bin/bash

# Set default HyperBEAM path if not provided
HYPERBEAM_PATH=${HYPERBEAM_PATH:-"../../HyperBEAM"}

# Validate that the HyperBEAM path exists
if [ ! -d "$HYPERBEAM_PATH" ]; then
    echo "Error: HyperBEAM path not found: $HYPERBEAM_PATH"
    echo "Please set HYPERBEAM_PATH environment variable to the correct path"
    echo "Example: HYPERBEAM_PATH=/path/to/HyperBEAM ./build.sh"
    exit 1
fi

echo "Using HyperBEAM path: $HYPERBEAM_PATH"

# Clean and prepare the apps/hb directory
rm -rf apps/hb
mkdir -p apps/hb/src
mkdir -p apps/hb/include

# Copy HyperBEAM source files
echo "Copying HyperBEAM source files..."
cp -r "$HYPERBEAM_PATH/src"/* apps/hb/src/
cp -r "$HYPERBEAM_PATH/src/include"/* apps/hb/include/

# Copy the app file with modifications
cp "$HYPERBEAM_PATH/src/hb.app.src" apps/hb/src/

# Copy other necessary files
if [ -d "$HYPERBEAM_PATH/priv" ]; then
    mkdir -p apps/hb/priv
    cp -r "$HYPERBEAM_PATH/priv"/* apps/hb/priv/
fi

# Copy native code if needed
if [ -d "$HYPERBEAM_PATH/native" ]; then
    mkdir -p apps/hb/native
    cp -r "$HYPERBEAM_PATH/native"/* apps/hb/native/
fi

# Copy Makefile for WAMR setup
if [ -f "$HYPERBEAM_PATH/Makefile" ]; then
    cp "$HYPERBEAM_PATH/Makefile" ./
fi

# Setup WAMR (WebAssembly Micro Runtime) if not already built
if [ ! -d "_build/wamr" ]; then
    echo "Setting up WAMR..."
    if [ -f "Makefile" ]; then
        make wamr || echo "WAMR build failed, continuing without native compilation"
    else
        echo "Makefile not found, skipping WAMR setup"
    fi
fi

# Create build info header file
echo "Creating build info header..."
mkdir -p _build
mkdir -p apps/hb/_build
echo '-define(HB_BUILD_SOURCE, <<"local-build">>).' > _build/hb_buildinfo.hrl
echo '-define(HB_BUILD_SOURCE_SHORT, <<"local">>).' >> _build/hb_buildinfo.hrl
echo "-define(HB_BUILD_TIME, $(date +%s))." >> _build/hb_buildinfo.hrl
# Also create it in the location expected by the apps/hb source
cp _build/hb_buildinfo.hrl apps/hb/_build/hb_buildinfo.hrl

echo "HyperBEAM files copied successfully!"
echo "Running rebar3 compile..."
rebar3 compile