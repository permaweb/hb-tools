#!/bin/bash

HYPERBEAM_PATH="./HyperBEAM"

git clone https://github.com/permaweb/HyperBEAM ./HyperBEAM
cd HyperBEAM
git checkout "edge"
cd ..

# Clean build directories for fresh start
echo "Cleaning previous build..."
rm -rf _build src/hb_imported include apps

# Parse module configuration
if [ ! -f "hb_modules.config" ]; then
    echo "Error: hb_modules.config not found"
    exit 1
fi

# Extract module list from config file - only actual module names
HB_MODULES=$(grep -A 100 "{hb_modules," hb_modules.config | grep -E "^\s*[a-z][a-zA-Z0-9_]*" | grep -v "}]" | sed 's/.*\(^[[:space:]]*\)\([a-z][a-zA-Z0-9_]*\).*/\2/' | sort | uniq)
HB_INCLUDES=$(grep -A 10 "{hb_include_files," hb_modules.config | grep -o '"[^"]*"' | tr -d '"')

echo "Modules to copy: $HB_MODULES"
echo "Include files to copy: $HB_INCLUDES"

# Prepare directories for selective imports
mkdir -p src/hb_imported

# Copy only the needed HyperBEAM modules
echo "Copying selected HyperBEAM modules..."
for module in $HB_MODULES; do
    if [ -f "$HYPERBEAM_PATH/src/${module}.erl" ]; then
        echo "  Copying ${module}.erl"
        cp "$HYPERBEAM_PATH/src/${module}.erl" "src/hb_imported/"
        # Fix include paths in the copied module
        sed -i 's|-include("include/|-include("hb_imported/|g' "src/hb_imported/${module}.erl"
        
        # Fix hb_keccak NIF loading issues
        if [ "$module" = "hb_keccak" ]; then
            echo "    Fixing hb_keccak NIF loading..."
            sed -i '/-on_load/d' "src/hb_imported/${module}.erl"
            sed -i '/init() ->/,/erlang:load_nif/c\%% Stub implementation - no NIF loading' "src/hb_imported/${module}.erl"
        fi
    else
        echo "  Warning: ${module}.erl not found in $HYPERBEAM_PATH/src/"
    fi
done

# Copy needed include files
echo "Copying selected include files..."
for include in $HB_INCLUDES; do
    if [ -f "$HYPERBEAM_PATH/src/include/${include}" ]; then
        echo "  Copying ${include}"
        cp "$HYPERBEAM_PATH/src/include/${include}" "src/hb_imported/"
        # Fix include paths in the copied include files
        sed -i 's|-include("include/|-include("hb_imported/|g' "src/hb_imported/${include}"
    else
        echo "  Warning: ${include} not found in $HYPERBEAM_PATH/src/include/"
    fi
done

# Create build info header file
echo "Creating build info header..."
mkdir -p _build
echo '-define(HB_BUILD_SOURCE, <<"local-build">>).' > _build/hb_buildinfo.hrl
echo '-define(HB_BUILD_SOURCE_SHORT, <<"local">>).' >> _build/hb_buildinfo.hrl
echo "-define(HB_BUILD_TIME, $(date +%s))." >> _build/hb_buildinfo.hrl

echo "Selected HyperBEAM modules copied successfully!"
echo "Running rebar3 compile..."
rebar3 compile

rm -rf $HYPERBEAM_PATH